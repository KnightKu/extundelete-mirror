// extundelete -- An ext3 and ext4 file system undeletion utility
// 
// Parts of this program are based upon ext3grep, which was licensed under the
// GPL v2 or later and copyright 2008 by Carlo Wood.
// extundelete Copyright 2009-10, by Nic Case
//
// This program may be redistributed under the terms of the GNU Public
// License version 2.

/*
Search for "FIXME:" to find the parts of the program needing improvements.

Useful options:
--version
--help
--superblock
--journal --superblock
--inode #
--block #
--restore-file path/to/deleted/file
--restore-inode #
--restore-files filename
--restore-all
--restore-directory path/of/directory
-j journal_dev
-b block_number
-B block_size
--before #
--after #


Important future enhancements:

	Add an --all-versions option, to restore all versions of inodes in the journal
	to separate files (.v1, .v2, etc.) by changing recover_inode and restore_inode.

	Restore extended attributes from the partition.

	Add support for the journal=data mount option (search through the journal for
	data blocks)

	Handle other file types (symbolic links, etc.) to restore those, too.

	Put partially-recovered files in a separate directory structure.

	Add an --interactive option, so the journal and group descriptors needn't be
	read when examining the file system.

	Generally incorporate ext2fs library functions where appropriate and
	possible.
		- use e2p's list_super instead of printing the superblock
		- possibly use ext2fs functions for endian corrections

	Rework the program to consider what to do to comprehensively search the
	journal's information to restore all possible inodes (including ones with
	identical numbers, but different block pointers), and also comprehensively
	search the directory blocks for different inode numbers which may correspond
	to the same file name (just different versions).

*/

#include "config.h"

/* C++ libraries */
#include <algorithm>
#include <assert.h>
#include <bitset>
#include <cerrno>
#include <climits>
#include <cstring>
#include <cstdlib>
#include <csignal>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <new>
#include <set>
#include <sstream>
#include <stdint.h>
#include <vector>

/* POSIX libraries */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <utime.h>

/* ext3/4 libraries */
#include <ext2fs/ext2fs.h>
#include "extundelete.h"
#include "extundelete-priv.h"
#include "block.h"


#ifndef HAVE_EXT2FS_BMAP2
static errcode_t ext2fs_bmap2(ext2_filsys fs, ext2_ino_t ino,
                              struct ext2_inode *inode,
                              char *block_buf, int bmap_flags, blk64_t block,
                              int *ret_flags, blk64_t *phys_blk)
{
	errcode_t retval;
	blk_t block32, phys_blk32;
	if(block > UINT_MAX)
		return EDOM;
	block32 = (blk_t) block;
	retval = ext2fs_bmap(fs, ino, inode, block_buf, bmap_flags, block32, &phys_blk32);
	*phys_blk = phys_blk32;
	return retval;
}
#endif


#ifndef HAVE_EXT2FS_GET_GENERIC_BMAP_START
static blk64_t ext2fs_get_generic_bmap_start(ext2fs_generic_bitmap bitmap)
{
#ifdef HAVE_EXT2FS_GET_GENERIC_BITMAP_START
	return ext2fs_get_generic_bitmap_start(bitmap);
#else
	blk_t *start = (blk_t *) ((char *) bitmap + sizeof(errcode_t)
		+ sizeof(ext2_filsys));
	return *start;
#endif
}
#endif

#ifndef HAVE_EXT2FS_GET_GENERIC_BMAP_START
static blk64_t ext2fs_get_generic_bmap_end(ext2fs_generic_bitmap bitmap)
{
#ifdef HAVE_EXT2FS_GET_GENERIC_BITMAP_START
	return ext2fs_get_generic_bitmap_end(bitmap);
#else
	blk_t *end = (blk_t *) ((char *) bitmap + sizeof(errcode_t)
		+ sizeof(ext2_filsys) + sizeof(blk_t) );
	return *end;
#endif
}
#endif

#ifndef HAVE_EXT2FS_INODE_TABLE_LOC
static inline blk64_t ext2fs_inode_table_loc(ext2_filsys fs, dgrp_t group)
{
	return fs->group_desc[group].bg_inode_table;
}
#endif

// extern variable definitions
std::string outputdir;
uint32_t block_size_;
unsigned int inode_size_;
std::vector<uint32_t> tag_seq;
block_list_t tag_jblk;
block_list_t tag_fsblk;
block_list_t rvk_block;
journal_map_t journ_map;
std::ostream Log::error(std::cerr.rdbuf());
std::ostream Log::warn(std::cout.rdbuf());
std::ostream Log::info(0);
std::ostream Log::debug(0);
std::ofstream Log::efile;
std::ofstream Log::wfile;
std::ofstream Log::ifile;
std::ofstream Log::dfile;


// Define triad: a class similar to std::pair, but with three values.
template <class T1, class T2, class T3> struct triad
{
	typedef T1 first_type;
	typedef T2 second_type;
	typedef T3 third_type;

	T1 first;
	T2 second;
	T3 third;
	triad() : first(T1()), second(T2()), third(T3()) {}
	triad(const T1& x, const T2& y, const T3& z) : first(x), second(y), third(z) {}
	template <class U, class V, class W>
		triad (const triad<U,V,W> &p) : first(p.first), second(p.second), third(p.third) { }
};

// Sorting the triad will be done by looking at only the first value.
template<class T1, class T2, class T3>
	inline bool
	operator<(const triad<T1, T2, T3>& x, const triad<T1, T2, T3>& y)
	{ return x.first < y.first; }


struct nth_block {
	blk64_t n;
	blk64_t *blknum;
};


#ifndef EXT4_FEATURE_INCOMPAT_64BIT
#define EXT4_FEATURE_INCOMPAT_64BIT 0x0080
#endif
/* This function unifies the 32 and 64 bit inode bitmap tests */
static int extundelete_test_inode_bitmap(const ext2_filsys fs, ext2_ino_t ino)
{
	int allocated = -1;
	if(! EXT2_SB(fs->super)->s_feature_incompat & EXT4_FEATURE_INCOMPAT_64BIT) {
		allocated = ext2fs_test_inode_bitmap(fs->inode_map, ino);
	}
#ifdef HAVE_EXT2FS_TEST_INODE_BITMAP2
	else {
		allocated = ext2fs_test_inode_bitmap2(fs->inode_map, ino);
	}
#endif
	return allocated;
}


/* Returns 0 if block is unallocated; 1 if block is allocated;
 * 2 if block number is out of range
 * Use when a "bad block number" error should be treated like an
 * "allocated block" as an alternative to the ext2fs version.
*/
static int extundelete_test_block_bitmap(ext2fs_block_bitmap block_map, blk64_t blocknr)
{
	int allocated;
	if(blocknr == 0) return 0;
	if(blocknr < ext2fs_get_generic_bmap_start(block_map) ||
		blocknr > ext2fs_get_generic_bmap_end(block_map) )
	{
		allocated = 2;
	}
	else {
	#ifdef HAVE_EXT2FS_TEST_BLOCK_BITMAP2
		allocated = ext2fs_test_block_bitmap2(block_map, blocknr);
	#else
		if(blocknr > UINT32_MAX)
			return 2;
		allocated = ext2fs_test_block_bitmap(block_map, (blk_t) blocknr);
	#endif
		if(allocated) allocated = 1;
	}
	return allocated;
}

// Returns the number of the first inode in the block.
/*static*/ ext2_ino_t block_to_inode(const ext2_filsys fs, blk_t block)
{
	//First, make a map of all the block group inode table locations
	std::map<blk_t, uint32_t> block_to_group_map;
	blk_t max_offset = (EXT2_INODES_PER_GROUP(fs->super) - 1) *
		EXT2_INODE_SIZE(fs->super);
	blk_t max_block = max_offset >> EXT2_BLOCK_SIZE_BITS(fs->super);
	for(uint32_t group = 0; group < fs->group_desc_count; group++ ) {
		blk64_t block_nr = ext2fs_inode_table_loc(fs, group);
		block_to_group_map.insert(std::pair<blk_t, uint32_t>(block_nr, group));
	}

	std::map<blk_t, uint32_t>::iterator bgit = block_to_group_map.lower_bound(block);
	uint32_t group;

	// If the block contains inodes, find the deleted ones
	if ( bgit != block_to_group_map.end() && block - (*bgit).first < max_block) {
		group = (*bgit).second;
	}
	else {
		return 0;
	}

	blk64_t inode_table = ext2fs_inode_table_loc(fs, group);
	assert(block >= inode_table && (size_t)block_size_ * (block + 1) <= (size_t)block_size_ * inode_table + EXT2_INODES_PER_GROUP(fs->super) * inode_size_);
	return (ext2_ino_t)( 1 + group * EXT2_INODES_PER_GROUP(fs->super) + (size_t)block_size_ * (block - inode_table) / inode_size_);
}

// Returns the number of blocks a non-sparse file would need for the data
static inline blk64_t numdatablocks(const struct ext2_inode * const inode) {
	blk64_t val = 0;
	if( LINUX_S_ISDIR(inode->i_mode) ) {
		val = (inode->i_size + block_size_ - 1) / block_size_;
	} else {
		val = (EXT2_I_SIZE(inode) + block_size_ - 1) / block_size_;
	}
	return val;
}

// Returns the number of total blocks of the inode used on disk, including
// indirect blocks
static inline blk64_t numblocks(const struct ext2_inode * const inode) {
	// Calculated this way to avoid overflow
	blk64_t spb = (blk64_t) block_size_ / 512; // Sectors per block
	blk64_t val = (inode->i_blocks + block_size_/1024) / spb;
	return val;
}

// Below are a bunch of functions used to convert 16, 32, and 64 bit
// values read from disk to the proper endianness for the cpu we are
// running this program on.  The 64-bit version has not undergone testing.
static inline uint64_t be64_to_cpu(uint64_t *y)
{
	int n = 1 ;
	if(*(char *) &n == 1)  // True if the cpu is little endian.
	{
		*y = (ext2fs_swab32(*y >> 32) |
			(((__u64)ext2fs_swab32(*y & 0xFFFFFFFFUL)) << 32));
	}
	return *y;
}

static inline uint32_t be32_to_cpu(uint32_t *y)
{
	int n = 1 ;
	if(*(char *) &n == 1)  // True if the cpu is little endian.
	{
		uint32_t x = *y;
		*y = x << 24 | x >> 24 |
			(x & (uint32_t)0x0000ff00UL) << 8 |
			(x & (uint32_t)0x00ff0000UL) >> 8;
	}
	return *y;
}

static inline uint16_t be16_to_cpu(uint16_t *x)
{
	int n = 1 ;
	if(*(char *) &n == 1)  // True if the cpu is little endian.
	{
		*x = *x << 8 | *x >> 8;
	}
	return *x;
}

static inline uint16_t le16_to_cpu(uint16_t *x)
{
	int n = 1 ;
	if(*(char *) &n != 1)  // False if the cpu is little endian.
	{
		*x = *x << 8 | *x >> 8;
	}
	return *x;
}

static inline uint32_t le32_to_cpu(uint32_t *y)
{
	int n = 1 ;
	if(*(char *) &n != 1)  // False if the cpu is little endian.
	{
		uint32_t x = *y;
		*y = x << 24 | x >> 24 |
			(x & (uint32_t)0x0000ff00UL) << 8 |
			(x & (uint32_t)0x00ff0000UL) >> 8;
	}
	return *y;
}


//FIXME: linux kernel's version of this macro checks the journal version >= 2.
#define JOURNAL_HAS_INCOMPAT_FEATURE(j,mask)                               \
        ((j)->s_header.h_blocktype == 4 &&                                  \
         ((j)->s_feature_incompat & ext2fs_cpu_to_be32((mask))))

static size_t journ_tag_bytes(journal_superblock_t *jsb)
{
	journal_block_tag_t tag;
	size_t x = 0;

	if (JOURNAL_HAS_INCOMPAT_FEATURE(jsb, JFS_FEATURE_INCOMPAT_CSUM_V2))
		x += sizeof(tag.t_checksum);

	if (JOURNAL_HAS_INCOMPAT_FEATURE(jsb, JFS_FEATURE_INCOMPAT_64BIT))
		return x + JBD_TAG_SIZE64;
	else
		return x + JBD_TAG_SIZE32;
}


// Changes a journal header, as read from disk, to the same
// endianness as the computer this program is running on.
static void journal_header_to_cpu(char *jhead)
{
	int item = sizeof(uint32_t)/sizeof(char);
	be32_to_cpu( (uint32_t *) jhead );
	be32_to_cpu( (uint32_t *) &jhead[item*1] );
	be32_to_cpu( (uint32_t *) &jhead[item*2] );
}

// Changes a journal revoke header, as read from disk, to the same
// endianness as the computer this program is running on.
// Note: This function may fail if (for example) the partition was created on
// a system which has sizeof(int) != 4.  
static void journal_revoke_header_to_cpu(char *jrh)
{
	journal_header_to_cpu(jrh);
	char *rvkd = jrh + sizeof(journal_header_t);
	if (sizeof(int) == 2)
		be16_to_cpu( (uint16_t *) rvkd );
	else if (sizeof(int) == 4)
		be32_to_cpu( (uint32_t *) rvkd );
	else if (sizeof(int) == 8)
		be64_to_cpu( (uint64_t *) rvkd );
	else {
		Log::error << "The system's integer size is nonstandard and the"
		<<" program cannot continue." << std::endl;
		assert(sizeof(int) == 4);
	}
}

// Changes a journal block tag, as read from disk, to the same
// endianness as the computer this program is running on.
static void journal_block_tag_to_cpu(char *jbt, journal_superblock_t *jsb)
{
	int item = sizeof(uint32_t)/sizeof(char);
	be32_to_cpu( (uint32_t *) jbt );
	be16_to_cpu( (uint16_t *) &jbt[item*1] );
	be16_to_cpu( (uint16_t *) &jbt[item*1 + 2] );
	if(journ_tag_bytes(jsb) > 8)
		be32_to_cpu( (uint32_t *) &jbt[item*2] );
}

// Changes the journal superblock, as read from disk, to the same
// endianness as the computer the program is running on.
static void journal_superblock_to_cpu(char *jsb)
{
	int item = sizeof(uint32_t)/sizeof(char);
	be32_to_cpu( (uint32_t *) jsb );
	be32_to_cpu( (uint32_t *) &jsb[item*1] );
	be32_to_cpu( (uint32_t *) &jsb[item*2] );
	be32_to_cpu( (uint32_t *) &jsb[item*3] );
	be32_to_cpu( (uint32_t *) &jsb[item*4] );
	be32_to_cpu( (uint32_t *) &jsb[item*5] );
	be32_to_cpu( (uint32_t *) &jsb[item*6] );
	be32_to_cpu( (uint32_t *) &jsb[item*7] );
	be32_to_cpu( (uint32_t *) &jsb[item*8] );
	be32_to_cpu( (uint32_t *) &jsb[item*9] );
	be32_to_cpu( (uint32_t *) &jsb[item*10] );
	be32_to_cpu( (uint32_t *) &jsb[item*11] );
	// UUIDs are endian-independent, so don't swap those bytes
	be32_to_cpu( (uint32_t *) &jsb[item*15] );
	be32_to_cpu( (uint32_t *) &jsb[item*16] );
	be32_to_cpu( (uint32_t *) &jsb[item*17] );
	be32_to_cpu( (uint32_t *) &jsb[item*18] );
	be32_to_cpu( (uint32_t *) &jsb[item*19] );
	// User IDs are endian-independent
}


int load_super_block(ext2_filsys fs)
{
	int errcode = 0;
	// Frequently used constants.
	ext2_super_block super_block = *(fs->super);
	uint32_t groups_ = fs->super->s_inodes_count / fs->super->s_inodes_per_group;
	block_size_ = EXT2_BLOCK_SIZE(fs->super);
	uint32_t inodes_per_group_ = EXT2_INODES_PER_GROUP(fs->super);
	inode_size_ = EXT2_INODE_SIZE(fs->super);

	// Sanity checks.
	// ext2-based filesystem
	if (super_block.s_magic != 0xEF53) return EU_FS_ERR;
	// All inodes belong to a group.
	if (groups_ * inodes_per_group_ != fs->super->s_inodes_count) return EU_FS_ERR;
	// The inode bitmap has to fit in a single block.
	if (inodes_per_group_ > 8 * block_size_) return EU_FS_ERR;
	// Each inode must fit within one block.
	if (inode_size_ > block_size_) return EU_FS_ERR;
	// inode_size must be a power of 2.
	if( (inode_size_ - 1) & inode_size_ ) return EU_FS_ERR;
	// There should fit exactly an integer number of inodes in one block.
	if ((block_size_ / inode_size_) * inode_size_ != block_size_) return EU_FS_ERR;
	// File system must have a journal.
	if(!(super_block.s_feature_compat & EXT3_FEATURE_COMPAT_HAS_JOURNAL)) {
		Log::error << "ERROR: The specified device does not have a journal file.	"
			<< "This program only undeletes files from file systems with journals.";
		return EU_FS_ERR;
	}

	// superblock flags that don't matter for undeletion:
	// EXT2_FEATURE_COMPAT_DIR_PREALLOC, EXT2_FEATURE_COMPAT_RESIZE_INO,
	// EXT2_FEATURE_COMPAT_DIR_INDEX
	// FIXME: check the rest of the possible flags, too.
	if ((super_block.s_feature_compat & EXT2_FEATURE_COMPAT_IMAGIC_INODES))
		Log::warn << "WARNING: Unknown file system feature: EXT2_FEATURE_COMPAT_IMAGIC_INODES\n";
	if ((super_block.s_feature_compat & EXT2_FEATURE_COMPAT_EXT_ATTR))
		Log::warn << "WARNING: Extended attributes are not restored.\n";

	if ((super_block.s_feature_incompat & EXT2_FEATURE_INCOMPAT_COMPRESSION))
		Log::warn << "WARNING: File systems with EXT2_FEATURE_INCOMPAT_COMPRESSION set (like this one) have not been tested.\n";
	if ((super_block.s_feature_incompat & EXT2_FEATURE_INCOMPAT_META_BG))
		Log::warn << "WARNING: Unknown file system feature: EXT2_FEATURE_INCOMPAT_META_BG\n";
	if ((super_block.s_feature_incompat & EXT3_FEATURE_INCOMPAT_RECOVER))
	{
		Log::warn << "WARNING: EXT3_FEATURE_INCOMPAT_RECOVER is set.\n"
		"The partition should be unmounted to undelete any files without further data loss.\n"
		"If the partition is not currently mounted, this message indicates \n"
		"it was improperly unmounted, and you should run fsck before continuing.\n";
		errcode = EU_FS_RECOVER;
	}
	if ((super_block.s_feature_incompat & EXT3_FEATURE_INCOMPAT_JOURNAL_DEV))
		Log::warn << "WARNING: extundelete may have problems reading from an external journal.\n";

	return errcode;
}

static int print_entry(ext2_ino_t /*dir*/, int entry,
		struct ext2_dir_entry *dirent, int /*offset*/,
		int /*blocksize*/, char * /*buf*/, void * /*priv*/) {

	// ext2_filsys fs = (ext2_filsys) priv;
	struct ext2_dir_entry_2 *dirent2 = (struct ext2_dir_entry_2 *)dirent;
	if(!dirent2->inode) return 0;
	std::cout << std::string(dirent2->name, dirent2->name_len);
	for(size_t n = dirent2->name_len; n < 50; n++)
		std::cout << " ";
	std::cout << dirent2->inode;
	if(entry == DIRENT_DELETED_FILE ) {
		for(size_t n = to_string(dirent->inode).size(); n < 15; n++)
			std::cout << " ";
		std::cout << "Deleted";
	}
	std::cout << std::endl;

	return 0;
}

void print_directory_inode(ext2_filsys fs, struct ext2_inode *inode,
		ext2_ino_t ino)
{
	blk64_t blocknr;
	int bmapflags = 0;
	char *buf = new char[block_size_];
	std::cout << "File name                                       ";
	std::cout << "| Inode number | Deleted status\n";
	for(blk64_t n = 0; n < numdatablocks(inode); n++) {
		ext2fs_bmap2(fs, ino, inode, NULL, bmapflags, n, NULL, &blocknr);
		std::cout << "Directory block " << blocknr << ":\n";
		struct dir_context ctx = {0,
			DIRENT_FLAG_INCLUDE_REMOVED, buf, print_entry, fs, 0};
		extundelete_process_dir_block(fs, &blocknr, 0, 0, 0, &ctx);
	}
	delete[] buf;
}

// Print the contents of a block in hexadecimal format.
static void dump_hex_to(std::ostream& os, char const* buf, size_t size)
{
	for (size_t addr = 0; addr < size; addr += 16)
	{
		os << std::hex << std::setfill('0') << std::setw(4) << addr << " |";
		int offset;
		for (offset = 0; offset < 16 && addr + offset < size; ++offset) {
			os << ' ' << std::hex << std::setfill('0') << std::setw(2)
			<< (int)(unsigned char)buf[addr + offset];
		}
		for (; offset < 16; ++offset)
			os << "	 ";
		os << " | ";
		for (offset = 0; offset < 16 && addr + offset < size; ++offset)
		{
			char c = buf[addr + offset];
			if (!std::isprint(c))
				c = '.';
			os << c;
		}
		os << std::endl;
	}
	os << std::dec;
}

// This function inserts the data from block blocknr[0] into the input buffer
// 'buf'.  The data from the block is inserted into
// the input buffer beginning at location 'blockcnt'.
// NOTE: The output returned by this command should be corrected to the proper
//    endianness for the host cpu when reading multi-byte structures from disk.
errcode_t read_block64(ext2_filsys fs, blk64_t *blocknr, e2_blkcnt_t blockcnt,
		blk_t /*ref_blk*/, int /*ref_offset*/, void *buf)
{
	errcode_t retval;
#ifdef HAVE_IO_CHANNEL_READ_BLK64
	retval = io_channel_read_blk64(fs->io, *blocknr, 1,
		reinterpret_cast<char *>(buf) + block_size_ * blockcnt);
#else
	if(*blocknr > UINT32_MAX)
		return EDOM;
	retval = io_channel_read_blk(fs->io, (blk_t) *blocknr, 1,
		reinterpret_cast<char *>(buf) + block_size_ * blockcnt);
#endif
	return retval;
}


dgrp_t extundelete_group_of_blk(ext2_filsys fs, blk64_t block) {
#ifdef HAVE_EXT2FS_GROUP_OF_BLK2
	return ext2fs_group_of_blk2(fs, block);
#else
	if(block > UINT32_MAX)
		return EDOM;
	return ext2fs_group_of_blk(fs, (blk_t) block);
#endif
}


errcode_t extundelete_read_dir_block(ext2_filsys fs, blk64_t block, void *buf) {
#ifdef HAVE_EXT2FS_READ_DIR_BLOCK3
	return ext2fs_read_dir_block3(fs, block, buf, 0);
#else
	if(block > UINT32_MAX)
		return EDOM;
	return ext2fs_read_dir_block(fs, (blk_t) block, buf);
#endif
}


// This function prints the data contained within the block blocknr
void classify_block(ext2_filsys fs, blk64_t blocknr)
{
	char* block = new char[block_size_];
	std::cout << "Block size: " << block_size_ << std::endl;

	read_block64(fs, &blocknr, 0, 0, 0, block);
	std::cout << "Contents of block " << blocknr << ":" << std::endl;
	dump_hex_to(std::cout, block, block_size_);
	std::cout << std::endl;
	std::cout << "Block " << blocknr << " is ";
	int allocated = extundelete_test_block_bitmap(fs->block_map, blocknr);
	if(!allocated) std::cout << "not ";
	std::cout << "allocated." << std::endl;
	std::cout << "Block " << blocknr <<" is in group " << extundelete_group_of_blk(fs, blocknr)
	<< "." << std::endl;
	errcode_t retval = extundelete_read_dir_block(fs, blocknr, block);
	if(retval == 0) {
		std::cout << "File name                                       ";
		std::cout << "| Inode number | Deleted status\n";
		struct dir_context ctx = {0, 
			DIRENT_FLAG_INCLUDE_REMOVED, block, print_entry, fs, 0};
		extundelete_process_dir_block(fs, &blocknr, 0, 0, 0, &ctx);
	}
	delete[] block;
}


// Store the block numbers in a buffer.
static int get_block_nums64(ext2_filsys /*fs*/, blk64_t *blocknr, e2_blkcnt_t blockcnt,
		blk64_t /*ref*/, int /*off*/, void *buf)
{
	blk64_t *blkptr = reinterpret_cast<blk64_t *>(buf);
	blkptr[blockcnt] = *blocknr;
	return 0;
}

// Store the nth block number in a buffer.
static int get_nth_block_num64(ext2_filsys /*fs*/, blk64_t *blocknr, e2_blkcnt_t blockcnt,
		blk64_t /*ref*/, int /*off*/, void *buf)
{
	struct nth_block *nth_blk = reinterpret_cast<nth_block *>(buf);
	if( (blk64_t) blockcnt == nth_blk->n) {
		*(nth_blk->blknum) = *blocknr;
		return BLOCK_ABORT;
	}
	return 0;
}


/*static*/ bool is_journal(ext2_filsys fs, blk64_t block)
{
	bool flag = false;
	ext2_ino_t ino = fs->super->s_journal_inum;
	errcode_t errcode;
	struct ext2_inode *inode;
	inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
	errcode = ext2fs_read_inode_full (fs, ino, inode, EXT2_INODE_SIZE(fs->super));
	if(errcode){
		std::cout << "Warning: unable to read journal inode; code "
		<< errcode << std::endl;
		return flag;
	}
	blk64_t *blocks = new blk64_t[ numblocks(inode) ];

	errcode = extundelete_block_iterate3 (fs, *inode, 0, 0, get_block_nums64, blocks);
	if(errcode){
		std::cout << "Warning: unknown error encountered; code "
		<< errcode << std::endl;
		return flag;
	}
	std::cout << blocks[0] << std::endl;
	for (uint32_t n = 0; n < numblocks(inode); n++)
		if (block == blocks[n])
		{
			flag = true;
			break;
		}
	delete inode;
	delete[] blocks;
	return flag;
}

struct pair_struct {
	ext2_filsys fs;
	ext2_filsys jfs;
	std::vector<ext2_ino_t>& inolist;
	ext2_ino_t ino;
	std::string dirname;
	int del;
	std::vector<ext2_ino_t> parent_inos;
};

/*
 * entry_iterate: determine whether to try to restore a given directory
 * entry based on its inode number and deleted status.
*/
static int entry_iterate(ext2_ino_t /*dir*/, int entry,
		struct ext2_dir_entry *dirent, int /*offset*/,
		int /*blocksize*/, char * /*buf*/, void *priv)
{
	struct ext2_dir_entry_2 *dirent2 = (struct ext2_dir_entry_2 *)dirent;
	if( strncmp(dirent2->name, ".", dirent2->name_len) == 0) return 0;
	if( strncmp(dirent2->name, "..", dirent2->name_len) == 0) return 0;
	struct pair_struct *ps = (struct pair_struct *)priv;
	if(entry == DIRENT_DELETED_FILE || ps->del) {
		for( std::vector<ext2_ino_t>::iterator it=(ps->inolist).begin();
				it != (ps->inolist).end(); it++ ) {
			if(dirent2->inode == *it) {
				std::string fname = ps->dirname;
				if(!fname.empty()) fname.append("/");
				fname.append(dirent2->name, dirent2->name_len);
				restore_inode(ps->fs, ps->jfs, dirent2->inode, fname.c_str() );
				(ps->inolist).erase(it);
				break;
			}
		}
	}

	// if this entry is a directory, look for the names in that directory
	//FIXME: sometimes, a regular file can cause this check to be true
	if(dirent2->file_type == EXT2_FT_DIR && ps->ino != dirent2->inode) {
		std::string fname = ps->dirname;
		if(!fname.empty()) fname.append("/");
		fname.append(dirent2->name, dirent2->name_len);
		int newdel = ps->del;
		if(entry == DIRENT_DELETED_FILE)
			newdel = 1;
		std::vector<ext2_ino_t>::iterator it;
		it = std::find(ps->parent_inos.begin(), ps->parent_inos.end(), dirent2->inode);
		if( it == ps->parent_inos.end() ) {
			std::vector<ext2_ino_t> new_parent_inos(ps->parent_inos);
			new_parent_inos.push_back(dirent2->inode);
			pair_names_with(ps->fs, ps->jfs, ps->inolist, dirent2->inode, fname,
				newdel, new_parent_inos);
		}
	}

	return 0;

}

/*
 * pair_names_with: look through directory blocks to find deleted file names,
 * then pair those names with the inode number from the directory block.
 * This function should only be called when we think the file pointed to by 
 * "ino" is a directory.  The flag "del" indicates whether the directory entry
 * was marked as deleted: 1 if deleted, 0 if not.
*/
static int pair_names_with(ext2_filsys fs, ext2_filsys jfs, std::vector<ext2_ino_t>& inolist,
		ext2_ino_t ino, std::string dirname, int del,
		std::vector<ext2_ino_t>& parent_inos )
{
	// Look through the directory structure for what the file name of the inodes
	// should be.
	//int bmapflags = 0;
	if(inolist.size() == 0) { return 0; }

	errcode_t retval;
	struct ext2_inode *inode;
	inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
	char *buf = new char[block_size_];
	// If we are expecting a deleted directory block, then don't try to read
	// an inode in the file system -- it won't be the right one.
	if(del)
		retval = 1;
	else
		retval = ext2fs_read_inode_full(fs, ino, inode, EXT2_INODE_SIZE(fs->super));

	if(retval == 0 && LINUX_S_ISDIR(inode->i_mode) ) {
		blk64_t *blocks = new blk64_t[ numdatablocks(inode) ];
		extundelete_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_block_nums64, blocks);

		for(blk64_t n = 0; n < numdatablocks(inode); n++) {
			blk64_t blknum = blocks[n];
			struct pair_struct ps = {fs, jfs, inolist, ino, dirname, 0, parent_inos};
			struct dir_context ctx = {0, 
				DIRENT_FLAG_INCLUDE_REMOVED, buf, entry_iterate, &ps, 0};
			extundelete_process_dir_block(fs, &blknum, 0, 0, 0, &ctx);
		}
		delete[] blocks;
	}

	if(inolist.size() == 0) { delete inode; delete[] buf; return 0; }

	// Look through blocks of a recovered inode, but don't waste time if it
	// points to the same blocks as the on-disk inode
	if(retval == 0) {
		struct ext2_inode *inode2;
		inode2 = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
		retval = recover_inode(fs, jfs, ino, inode2, 0);
		if(retval == 0) {
			// The block pointers start at byte 40 and end at byte 96
			retval = memcmp(40+(char *)inode, 40+(char *)inode2, 96-40);
			retval = !retval;
		}
		delete inode;
		inode = inode2;
		inode2 = 0;
	}
	else
		retval = recover_inode(fs, jfs, ino, inode, 0);

	if(retval == 0 && LINUX_S_ISDIR(inode->i_mode) ) {

		for(blk64_t n = 0; n < numdatablocks(inode); n++) {
			blk64_t blknum = 0;
			struct nth_block nb = {n, &blknum};
			extundelete_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_nth_block_num64, &nb);
			// If the block is allocated, it is not valid.
			if(extundelete_test_block_bitmap(fs->block_map, blknum))
				continue;
			struct pair_struct ps = {fs, jfs, inolist, ino, dirname, 1, parent_inos};
			struct dir_context ctx = {0, 
				DIRENT_FLAG_INCLUDE_REMOVED, buf, entry_iterate, &ps, 0};
			extundelete_process_dir_block(fs, &blknum, 0, 0, 0, &ctx);
		}
	}

	if(inolist.size() == 0) { delete inode; delete[] buf; return 0; }

	if(ino == EXT2_ROOT_INO) {
		// Look through all revoked blocks as a last resort, and only once,
		// for the initial calling of this function, and only when restoring all
		block_list_t::iterator it;
		for ( it=rvk_block.begin() ; it != rvk_block.end(); it++ ) {
			blk64_t blknum = *it;
			struct pair_struct ps = {fs, jfs, inolist, ino, "lost+found", 1, parent_inos};
			struct dir_context ctx = {0, 
				DIRENT_FLAG_INCLUDE_REMOVED, buf, entry_iterate, &ps, 0};
			extundelete_process_dir_block(fs, &blknum, 0, 0, 0, &ctx);
		}
	}
	delete[] buf;
	delete inode;
	return 0;
}


int restore_directory(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t dirino, std::string dirname)
{
	std::vector<ext2_ino_t> recoverable_inodes;
	std::map<ext2_ino_t, uint32_t> deleted_inodes_map;
	std::map<ext2_ino_t, uint32_t>::iterator dit;
	block_list_t::reverse_iterator it;
	block_list_t::reverse_iterator jit;
	std::vector<uint32_t>::reverse_iterator sit;

	Log::info << "Searching for recoverable inodes in directory ";
	if(dirino == EXT2_ROOT_INO) Log::info << "/";
	Log::info << dirname << " ... " << std::endl;

	//First, make a map of all the block group inode table locations
	std::map<blk64_t, int> block_to_group_map;
	blk64_t max_offset = (EXT2_INODES_PER_GROUP(fs->super) - 1) *
		EXT2_INODE_SIZE(fs->super);
	blk64_t max_block = max_offset >> EXT2_BLOCK_SIZE_BITS(fs->super);
	for(int group = 0; (unsigned int)group < fs->group_desc_count; group++ ) {
		blk64_t block_nr = ext2fs_inode_table_loc(fs, group);
		block_to_group_map.insert(std::pair<blk64_t, int>(block_nr, group));
	}

	// All the block numbers of deleted inode blocks
	char *buf = new char[block_size_];
	struct ext2_inode *inode;
	inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
	for ( it=tag_fsblk.rbegin(), jit=tag_jblk.rbegin(), sit=tag_seq.rbegin();
			it != tag_fsblk.rend();
			it++, jit++, sit++ )
	{
		blk64_t blknum = *it;
		std::map<blk64_t, int>::iterator bgit = block_to_group_map.lower_bound(blknum);
		if( bgit != block_to_group_map.begin() &&
				(bgit == block_to_group_map.end() || (*bgit).first != blknum) ) {
			bgit--;
		}

		// If the block contains inodes, find the deleted ones
		if ( blknum - (*bgit).first < max_block) {
			int group = (*bgit).second;
			blk64_t blknum1 = ext2fs_inode_table_loc(fs, group);
			ext2_ino_t firstino = (ext2_ino_t) ((blknum - blknum1) * block_size_ / inode_size_
				+ group * EXT2_INODES_PER_GROUP(fs->super) + 1);
			read_journal_block(jfs, *jit, buf);
			for(ext2_ino_t ino = firstino; ino < firstino + block_size_/inode_size_ ;
					ino++ ) {
				parse_inode_block(fs, inode, buf, ino);
				// If we have a deleted copy, add to the deleted list
				// If we have a non-deleted copy in the journal, and
				// a newer, deleted copy, then add to the recoverable list
				if(inode->i_dtime > 0) {
					if( (dit=deleted_inodes_map.find(ino)) != deleted_inodes_map.end() ) {
						if( (*dit).second > *sit ) {
							(*dit).second = *sit;
						}
					}
					else {
						deleted_inodes_map.insert(std::pair<ext2_ino_t, uint32_t>(ino, *sit));
					}
				}
				if(inode->i_dtime == 0 && inode->i_blocks > 0) {
					if( (dit=deleted_inodes_map.find(ino)) != deleted_inodes_map.end() ) {
						if( (*dit).second >= *sit ) {
							recoverable_inodes.push_back(ino);
						}
					}
				}
			}
		}
	}
	delete[] buf;
	delete inode;

	std::sort(recoverable_inodes.begin(), recoverable_inodes.end());
	std::vector<ext2_ino_t>::iterator rit =
		std::unique(recoverable_inodes.begin(), recoverable_inodes.end());
	recoverable_inodes.resize( rit - recoverable_inodes.begin() );

	Log::info << recoverable_inodes.size() << " recoverable inodes found." << std::endl;
/*
	Log::debug << "Deleted inodes:  ";
	for(std::list<ext2_ino_t>::iterator it2 = deleted_inodes.begin(); it2 != deleted_inodes.end(); it2++) {

		Log::debug << (int) *it2 << "   " << std::flush;
	}
	Log::debug << "Recoverable inodes:  ";
	for(std::vector<ext2_ino_t>::iterator it2 = recoverable_inodes.begin(); it2 != recoverable_inodes.end(); it2++) {

		Log::debug << (int) *it2 << "   " << std::flush;
	}
//*/
	Log::info << "Looking through the directory structure for deleted files ... "
	<< std::endl;
	std::vector<ext2_ino_t>::size_type rsize = recoverable_inodes.size();
	int pnflag = !extundelete_test_inode_bitmap(fs, dirino);
	std::vector<ext2_ino_t> parent_inos(1, EXT2_ROOT_INO);
	pair_names_with(fs, jfs, recoverable_inodes, dirino, dirname, pnflag, parent_inos);
	Log::info << recoverable_inodes.size() << " recoverable inodes still lost." << std::endl;

	if(dirino == EXT2_ROOT_INO) {
		std::vector<ext2_ino_t>::iterator diit;
		for ( diit=recoverable_inodes.begin() ; diit != recoverable_inodes.end();
									 diit++ ) {
			std::ostringstream fname;
			fname << "file." << *diit;
			restore_inode(fs, jfs, *diit, fname.str());
		}
		if(rsize == 0)
			Log::info << "No files were undeleted." << std::endl;
	}
	else if(rsize == recoverable_inodes.size() ) {
		Log::info << "No files were undeleted." << std::endl;
	}
	return 0;
}

/* Read a single block from the journal */
errcode_t read_journal_block(ext2_filsys fs, blk64_t n, char *buf)
{
	errcode_t retval;
	if(fs->super->s_journal_inum) {
		retval = read_block64(fs, &n, 0, 0, 0, buf);
	}
	else {
		// External journal
		if((retval = read_block64(fs, &n, 0, 0, 0, buf) )) {
			com_err("extundelete", retval, "while reading journal block");
			exit(1);
		}
	}
	return retval;
}

/*
 * Read contents of journal file into global variables
*/
int init_journal(ext2_filsys fs, ext2_filsys jfs, journal_superblock_t *jsb)
{
	// Minimally validate input
	if(fs->super->s_inodes_count == 0) return EU_FS_ERR;
	if(jsb->s_blocksize == 0) return EU_FS_ERR;
	if(jsb->s_header.h_magic != JFS_MAGIC_NUMBER) return EU_FS_ERR;

	// Find the block range used by the journal.
	struct ext2_inode *journal_inode;
	journal_inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
	ext2_ino_t journal_ino = fs->super->s_journal_inum;
	ext2fs_read_inode_full (fs, journal_ino, journal_inode, EXT2_INODE_SIZE(fs->super));

	// Load the journal descriptors into memory.
	Log::info << "Loading journal descriptors ... " << std::flush;

	// Apparently, some bug exists that allocates one too many journal blocks,
	// so add one to the number of data blocks expected to prevent a memory
	// error during block_iterate.
	blk64_t *blocks = new blk64_t[ 1 + jsb->s_maxlen ];
	blocks[ jsb->s_maxlen ] = 0;
	if(fs->super->s_journal_inum) {
		extundelete_block_iterate3 (fs, *journal_inode, BLOCK_FLAG_DATA_ONLY, 0, get_block_nums64, blocks);
	}
	else
		for(blk64_t n = 0; n < jsb->s_maxlen; n++)
			blocks[n] = n;

	char *buf = new char[ block_size_];
	char *descbuf = new char[ block_size_];
	uint32_t number_of_descriptors = 0;
	/* j_tags is sequence number, journal block number, filesystem block number */
	std::vector<triad<uint32_t,blk64_t,blk64_t> > j_tags;

	for (blk64_t n = 0; n < jsb->s_maxlen; n++)
	{
		read_journal_block(jfs, blocks[n], buf);
		journal_header_to_cpu(buf);
		journal_header_t* descriptor = reinterpret_cast<journal_header_t*>(buf);

		if (descriptor->h_magic == JFS_MAGIC_NUMBER)
		{
			uint32_t seq = descriptor->h_sequence;
			++number_of_descriptors;

			switch (descriptor->h_blocktype)
			{
			case JFS_DESCRIPTOR_BLOCK:
			{
				Log::debug << std::endl << "Block " << blocks[n]
				<< ": start sequence " << seq;
				--number_of_descriptors;
				char *jbtbuf = (char *)descriptor + sizeof(journal_header_t);
				journal_block_tag_to_cpu(jbtbuf, jsb);
				journal_block_tag_t* jbt = reinterpret_cast<journal_block_tag_t*>(jbtbuf);
				uint32_t flags;
				uint32_t m = jsb->s_first; // need to skip the superblock
				do
				{
					++number_of_descriptors;
					++n;
					if (n > jsb->s_maxlen)
					{
						// This deals with a wrapped-around transaction
						// Need to break if the wrapped transaction was overwritten.
						read_journal_block(jfs, blocks[m], descbuf);
						journal_header_to_cpu(descbuf);
						journal_header_t* wrapped_descriptor =
							reinterpret_cast<journal_header_t*>(descbuf);
						if(wrapped_descriptor->h_magic == JFS_MAGIC_NUMBER) break;

						j_tags.push_back(triad<uint32_t,blk64_t,blk64_t>
							(seq,blocks[m],jbt->t_blocknr) );
						Log::debug << std::endl << "Journalled block " << blocks[m]
						<< " is a copy of block " << jbt->t_blocknr;
						m++;
					}
					else
					{
						j_tags.push_back(triad<uint32_t,blk64_t,blk64_t>
							(seq,blocks[n],jbt->t_blocknr) );
						Log::debug << std::endl << "Journalled block " << blocks[n]
						<< " is a copy of block " << jbt->t_blocknr;
					}
					flags = jbt->t_flags;
					if (!(flags & JFS_FLAG_SAME_UUID)) {
						jbt = reinterpret_cast<journal_block_tag_t*>(
							(char *)jbt + sizeof(jsb->s_uuid) );
					}
					jbt = reinterpret_cast<journal_block_tag_t*>(
						(char *)jbt + journ_tag_bytes(jsb) );
					journal_block_tag_to_cpu( (char *)jbt, jsb );
				} while(!(flags & JFS_FLAG_LAST_TAG));
				break;
			}
			case JFS_COMMIT_BLOCK:
			{
				Log::debug << std::endl << "Commit block " << blocks[n] << ": "
				<< "Sequence " << descriptor->h_sequence;
				break;
			}
			case JFS_REVOKE_BLOCK:
			{
				journal_revoke_header_t* rvk =
					reinterpret_cast<journal_revoke_header_t*>(buf);
				journal_revoke_header_to_cpu( (char *)rvk );
				Log::debug << std::endl
				<< "Revoke block " << blocks[n] << ":"
				<< " (size " << rvk->r_count << ") ";
				int64_t init_count = sizeof(journal_revoke_header_t);
				int64_t skip;
				if(JOURNAL_HAS_INCOMPAT_FEATURE(jsb, JFS_FEATURE_INCOMPAT_64BIT))
					skip = sizeof(blk64_t);
				else
					skip = sizeof(blk_t);
				for (int64_t count = init_count; count < rvk->r_count;
						count += skip)
				{
					blk64_t blk;
					if(JOURNAL_HAS_INCOMPAT_FEATURE(jsb, JFS_FEATURE_INCOMPAT_64BIT)) {
						blk64_t *block = (blk64_t *)&buf[count];
						be64_to_cpu((uint64_t *)block);
						rvk_block.push_back(*block);
						blk = *block;
					} else {
						uint32_t *block = (uint32_t *) &buf[count];
						be32_to_cpu(block);
						rvk_block.push_back(*block);
						blk = *block;
					}
					if( Log::debug.rdbuf() ) {
						if (count != init_count)
							Log::debug << ", ";
						Log::debug << blk;
					}
				}
				break;
			}
			case JFS_SUPERBLOCK_V1:
			{
				Log::debug << std::endl << "Found Journal Superblock (v1)";
				break;
			}
			case JFS_SUPERBLOCK_V2:
			{
				Log::debug << std::endl << "Found Journal Superblock (v2)";
				break;
			}
			default:
			{
				Log::info << std::flush;
				Log::warn << "WARNING: Unexpected blocktype ("
				<< descriptor->h_blocktype << ") in the journal."
				<< " The journal may be corrupt." << std::endl;
				break;
			} //default case
			} // switch statement
		} // if statement
	} // for loop

	delete[] buf;
	delete[] descbuf;
	// Sort the list by ascending sequence number and populate the global
	// variables in order.
	std::sort(j_tags.begin(), j_tags.end() );
	for(std::vector<triad<uint32_t,blk64_t,blk64_t> >::iterator it = j_tags.begin();
			it != j_tags.end(); it++) {
		tag_seq.push_back( it->first );
		tag_jblk.push_back( it->second );
		tag_fsblk.push_back( it->third );
		// journ_map order is (fsblk, (jblk, seq))
		block_pair_t val = block_pair_t(it->second, it->first);
		journal_map_item point = journal_map_item(it->third, val);
		journ_map.insert( point );
	}

	Log::info << number_of_descriptors << " descriptors loaded." << std::endl;

	if( Log::debug.rdbuf() ) {
		block_list_t::iterator it;
		Log::debug << std::endl << "rvk_block contains:";
		// All the block numbers of deleted directory blocks
		for ( it=rvk_block.begin() ; it != rvk_block.end(); it++ )
			Log::debug << " " << *it;
		Log::debug << std::endl;
		//Log::debug << "tag_seq contains:";
		//for ( it=tag_seq.begin() ; it != tag_seq.end(); it++ )
		//	Log::debug << " " << *it;
		//Log::debug << std::endl;
		Log::debug << "tag_jblk contains:";
		for ( it=tag_jblk.begin() ; it != tag_jblk.end(); it++ )
			Log::debug << " " << *it;
		Log::debug << std::endl;
		Log::debug << "tag_fsblk contains:";
		for ( it=tag_fsblk.begin() ; it != tag_fsblk.end(); it++ )
			Log::debug << " " << *it;

		Log::debug << std::endl;
	}

	delete journal_inode;
	delete[] blocks;
	return 0;
}

// Modifies the inode number of match_struct priv
// to set the inode of the directory entry
static int match_name(ext2_dir_entry *dirent, int /*off*/, int /*blksize*/,
		char * /*buf*/, void *priv)
{
	std::string curr_name = ((match_struct *) priv)->curr_name;
	const struct ext2_dir_entry_2 *curr_ent = 
			reinterpret_cast<const ext2_dir_entry_2 *>(dirent);
	if(curr_name.compare(0, curr_name.length(),
			curr_ent->name, curr_ent->name_len) == 0)
	{
		*(((match_struct *) priv)->ret_ino) = dirent->inode;
	}
	return 0;
}

static int match_name2(ext2_ino_t /*dir*/, int /*entry*/,
		struct ext2_dir_entry *dirent, int /*offset*/,
		int /*blocksize*/, char * /*buf*/, void *priv)
{
	return match_name(dirent, 0, 0, 0, priv);
}

static bool compare_sequence(block_pair_t a, block_pair_t b) {
	if(a.second < b.second) return true;
	else return false;
}

// Returns block number of a copy of blknum that resides in the journal
static blk64_t journ_get_dir_block(ext2_filsys /*fs*/, blk64_t blknum, void * /*buf*/)
{
	if(blknum == 0)
		return 0;

	bool found = false;

	// oldblks2 is (jblk, sequence)
	std::list<block_pair_t> oldblks2;
	std::pair<journal_map_t::iterator, journal_map_t::iterator> ret;
	ret = journ_map.equal_range(blknum);
	journal_map_t::iterator it;
	for(it = ret.first; it != ret.second; ++it) {
		oldblks2.push_back((*it).second);
		found = true;
	}
	oldblks2.sort( compare_sequence );

	if( found ) {
		std::list<block_pair_t>::reverse_iterator rit;
		rit = oldblks2.rbegin();
		return (*rit).first;
	}
	else {
		return 0;
	}
}

static ext2_ino_t find_inode(ext2_filsys fs, ext2_filsys jfs, struct ext2_inode *inode,
		std::string curr_part, blk64_t *blocks, int search_flags)
{
	char *buf = new char[block_size_];
	blk64_t blocknr;
	ext2_ino_t ino2 = 0;
	struct match_struct tmp = {0, curr_part};
	struct match_struct *priv = &tmp;
	ext2_ino_t *new_ino = new ext2_ino_t;

	for (blk64_t n = 0; n < numdatablocks(inode); ++n)
	{
		*new_ino = 0;
		priv->ret_ino = new_ino;
		priv->curr_name = curr_part;
		struct dir_context ctx = {0, 
			DIRENT_FLAG_INCLUDE_REMOVED, buf, match_name2, priv, 0};

		if(search_flags == SEARCH_JOURNAL) {
			blocknr = journ_get_dir_block (jfs, blocks[n], buf);
			if(blocknr == 0) continue;
			extundelete_process_dir_block(jfs, &blocknr, 0, 0, 0, &ctx);
		}
		else {
			blocknr = blocks[n];
			extundelete_process_dir_block(fs, &blocknr, 0, 0, 0, &ctx);
		}

		ino2 = *new_ino;

		if(ino2 != 0) break;
	}

	delete new_ino;
	delete[] buf;

	return ino2;
}

/* Pseudo code for what the search should look like:
dir1/dir2 is inode ino
look for entry curr_part = dir3

ino has blocks 1,2,3
old copies of ino (ino2) have blocks 1,2,3,4,5

look through journal copies of blks 1,2,3 - ino
if not found, look through journal copies of blks 1,2,3,4,5 - ino2
if not found, look through fs blks 1,2,3 - ino
if not found, look through fs blks 1,2,3,4,5 - ino2
if not found, look through all revoked fs blocks
if found entry, assign a new number to ino and new curr_part

:beginsearch
old copies of ino has blocks 11,12,13
look through journal copies of blks 11,12,13 - ino
if not found, look through fs blks 11,12,13 - ino
if not found, look through all revoked fs blocks
if found entry, assign a new number to ino and new curr_part
if found entry, goto beginsearch with new ino and curr_part
*/
errcode_t restore_file(ext2_filsys fs, ext2_filsys jfs, const std::string& fname)
{
	// Look through the directory structure to get as close as possible to the file.
	ext2_ino_t ino = EXT2_ROOT_INO;
	std::string curr_part;
	errcode_t retval;

	struct match_struct tmp = {0, curr_part};
	struct match_struct *priv = &tmp;
	ext2_ino_t *new_ino = new ext2_ino_t;
	*new_ino = EXT2_ROOT_INO;
	size_t place = 0;
	size_t oldplace;
	while( *new_ino != 0 ) {
		ino = *new_ino;
		oldplace = place;
		place = fname.find('/', oldplace);
		if (place != oldplace) {
			curr_part = fname.substr(oldplace, place-oldplace);

			*new_ino = 0;
			priv->ret_ino = new_ino;
			priv->curr_name = curr_part;
			ext2fs_dir_iterate (fs, ino, 0, NULL, match_name, priv );
		}

		if (place == std::string::npos ) {
			if(*new_ino == 0)
				break;

			curr_part = "";
			ino = *new_ino;
			break;
		}
		++place;
	}

	//delete priv;
	//delete new_ino;

	// Here, ino is an allocated filesystem inode number, curr_part is the first
	// part to try to match with the deleted entries
	// We are guaranteed that the inode is allocated here

	if (!commandline_restore_directory.empty() && curr_part == "" ) {
		retval = restore_directory(fs, jfs, ino, fname);
		return retval;
	}
	// Look for the next part in the directory blocks specified by the inode ino
	//int bmapflags = 0;
	struct ext2_inode *inode;
	inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
	retval = recover_inode(fs, jfs, ino, inode, 0);
	blk64_t *blocks = NULL;
	if (retval==0) {
		//A bad inode may cause way too much memory to be allocated
		try {
			//FIXME: find a way to do this without allocating that big chunk in blocks variable
			blocks = new blk64_t[ numdatablocks(inode) ];
			extundelete_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_block_nums64, blocks);
		}
		catch (std::exception& error) {
			delete[] blocks;
			blocks = NULL;
		}
	}

	char *buf = new char[ block_size_];

	struct ext2_inode *inode2;
	inode2 = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
	ext2fs_read_inode_full (fs, ino, inode2, EXT2_INODE_SIZE(fs->super));
	blk64_t *blocks2 = new blk64_t[ numdatablocks(inode2) ];
	extundelete_block_iterate3 (fs, *inode2, BLOCK_FLAG_DATA_ONLY, 0, get_block_nums64, blocks2);

	ext2_ino_t ino2 = 0;

	// Look at the blocks from the allocated inode in the journal
	ino2 = find_inode(fs, jfs, inode2, curr_part, blocks2, SEARCH_JOURNAL);

	// Look at the blocks from the deleted inode in the journal
	if(ino2 == 0 && blocks) {
		ino2 = find_inode(fs, jfs, inode, curr_part, blocks, SEARCH_JOURNAL);
	}

	// Look at the blocks from the allocated inode in the file system
	if(ino2 == 0) {
		ino2 = find_inode(fs, jfs, inode2, curr_part, blocks2, 0);
	}

	// Look at the blocks from the deleted inode in the filesystem
	if(ino2 == 0 && blocks) {
		ino2 = find_inode(fs, jfs, inode, curr_part, blocks, 0);
	}

	// Look through all revoked blocks for matching string
	if(ino2 == 0) {
		block_list_t::reverse_iterator rit;
		//match_struct *priv = new match_struct;
		//ext2_ino_t *new_ino = new ext2_ino_t;
		for ( rit=rvk_block.rbegin() ; rit != rvk_block.rend(); ++rit ) {
			*new_ino = 0;
			priv->ret_ino = new_ino;
			priv->curr_name = curr_part;
			struct dir_context ctx = {0, 
				DIRENT_FLAG_INCLUDE_REMOVED, buf, match_name2, priv, 0};
			extundelete_process_dir_block(fs, &*rit, 0, 0, 0, &ctx);
			ino2 = *new_ino;

			if(ino2 != 0) break;
		}
		//delete priv;
		//delete new_ino;
	}

	if(ino2 != 0) {
		while(1) {
			oldplace = place;
			place = fname.find('/', oldplace);
			if (place != std::string::npos && place == oldplace)
				place++;
			else
				break;
		}
		ino = ino2;

		if (oldplace == std::string::npos) {
			curr_part = "";
		}
		else {
			curr_part = fname.substr(oldplace, place-oldplace);
			if(place != std::string::npos)	++place;
		}

		ino2 = 0;
	}

	//Log::debug << "Post directory block search inode number: " << ino << std::endl;
	//Log::debug << "Next file part: " << curr_part << std::endl;
	//Log::debug << "File name and place: " << fname << "   " << place << std::endl;

	char *buf2 = new char[ block_size_];
	while(curr_part != "") {
		ino2 = 0;
		retval = recover_inode(fs, jfs, ino, inode, 0);
		if(retval != 0) break;

		blk64_t *blocks3 = NULL;
		//A bad inode may cause way too much memory to be allocated
		try {
			blocks3 = new blk64_t[ numdatablocks(inode) ];
			extundelete_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_block_nums64, blocks3);

			// Look through copies of the blocks within the journal
			ino2 = find_inode(fs, jfs, inode, curr_part, blocks3, SEARCH_JOURNAL);

			// Looking through blocks in the filesystem in the right directory
			if(ino2 == 0) {
				ino2 = find_inode(fs, jfs, inode, curr_part, blocks3, 0);
			}

			delete[] blocks3;
		}
		catch (std::exception& error) {
			delete[] blocks3;
			blocks3 = NULL;
		}

		// Looking through all revoked blocks
		if(ino2 == 0) {
			block_list_t::reverse_iterator rit;
			//match_struct *priv = new match_struct;
			//ext2_ino_t *new_ino = new ext2_ino_t;
			for ( rit=rvk_block.rbegin() ; rit != rvk_block.rend(); ++rit ) {
				*new_ino = 0;
				priv->ret_ino = new_ino;
				priv->curr_name = curr_part;
				struct dir_context ctx = {0, 
					DIRENT_FLAG_INCLUDE_REMOVED, buf, match_name2, priv, 0};
				extundelete_process_dir_block(fs, &*rit, 0, 0, 0, &ctx);
				ino2 = *new_ino;

				if(ino2 != 0) break;
			}
			//delete priv;
			//delete new_ino;
		}

		// If we have come to the end of the filename string,
		// or if we have not found a suitable file name, stop looking
		if(ino2 != 0) {
			while(1) {
				oldplace = place;
				place = fname.find('/', oldplace);
				if (place != std::string::npos && place == oldplace)
					place++;
				else
					break;
			}
			ino = ino2;

			if (oldplace == std::string::npos ) {
				curr_part = "";
				break;
			}
			curr_part = fname.substr(oldplace, place-oldplace);
			if(place != std::string::npos) ++place;
		}
		else {
			break;
		}

	}

	// Log::debug << "Post-revoke search current inode number: " << ino << std::endl;
	// Log::debug << "Next file part: " << curr_part << std::endl;

	//delete priv;
	delete new_ino;
	delete inode;
	delete inode2;
	delete[] blocks;
	delete[] blocks2;
	delete[] buf;
	delete[] buf2;

	if (!commandline_restore_directory.empty() && curr_part == "" ) {
		retval = restore_directory(fs, jfs, ino, fname);
		return retval;
	}
	if(curr_part == "") {
		restore_inode(fs, jfs, ino, fname);
		return 0;
	}
	else {
		Log::error << "Failed to restore file " << fname << std::endl
		<< "Could not find correct inode number past inode " << ino
		<< "." << std::endl;
		inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
		ext2fs_read_inode_full (fs, ino, inode, EXT2_INODE_SIZE(fs->super));
		if (LINUX_S_ISDIR(inode->i_mode) && inode->i_blocks > 0)
			Log::error << "Try altering the filename to one of the entries listed below." << std::endl;
			print_directory_inode(fs, inode, ino);
		delete inode;
		return EU_RESTORE_FAIL;
	}
}

static inline errcode_t inode_is_valid(const ext2_filsys fs, const struct ext2_inode * const inode)
{
	/* FIXME: Could also check that at least one block pointer is nonzero */
	/* FIXME: Could also check that file size and block count is consistent */
	/* Remember this must account for both files and directories */
	return
		inode->i_dtime == 0 &&
		numblocks(inode) > 0 &&
		numdatablocks(inode) > 0 &&
		inode->i_blocks <= fs->super->s_blocks_count &&
		inode->i_blocks > 0 &&
		inode->i_links_count > 0;
}

// Use ver = 0 to get previous behavior
static errcode_t recover_inode(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t ino,
		struct ext2_inode *&inode, int ver)
{
	if ((ino == 0) || (ino > fs->super->s_inodes_count))
		return EXT2_ET_BAD_INODE_NUM;
	int group = ext2fs_group_of_ino(fs, ino);
	blk64_t blknum1 = ext2fs_inode_table_loc(fs, group);
	blk64_t blknum2 = (ino - 1 - group * EXT2_INODES_PER_GROUP(fs->super)) * inode_size_ / block_size_;
	blk64_t blknum = blknum1 + blknum2;

	// Find the latest non-deleted inode in the journal that corresponds to the
	// inode number found in the directory block.

	// oldblks2 is (jblk, sequence)
	std::list<block_pair_t> oldblks2;
	std::pair<journal_map_t::iterator, journal_map_t::iterator> ret;
	ret = journ_map.equal_range(blknum);
	journal_map_t::iterator it;
	for(it = ret.first; it != ret.second; ++it) {
		oldblks2.push_back((*it).second);
	}
	oldblks2.sort( compare_sequence );

/*
	std::list<block_pair_t>::iterator oit;
	Log::debug << "oldblks contains:";
	for ( oit=oldblks2.begin() ; oit != oldblks2.end(); oit++ )
		Log::debug << " " << (*oit).first;
	Log::debug << std::endl;
//*/
	bool found = false;
	// If the inode is not allocated, we can just pick the first valid inode
	bool deletedfound = !extundelete_test_inode_bitmap(fs, ino) &&
		commandline_before == LONG_MAX &&
		commandline_after == 0;

	std::list<block_pair_t>::reverse_iterator rit;
	char *buf = new char[block_size_];
	for ( rit=oldblks2.rbegin() ; rit != oldblks2.rend(); ++rit ) {
		read_journal_block(jfs, ((*rit).first), buf);
		parse_inode_block(fs, inode, buf, ino);
		if (inode->i_dtime != 0 && ((int64_t) (inode->i_dtime)) >= commandline_after &&
				((int64_t) (inode->i_dtime)) <= commandline_before)
		{
			deletedfound = true;
			continue;
		}
		if( deletedfound && inode_is_valid(fs, inode)) {
			if(ver == 0) {
				found = true;
				break;
			}
			ver--;
		}
	}
	delete[] buf;
	if( !found ) return 1;

	return 0;
}


static int write_block64(ext2_filsys fs, blk64_t *blocknr, e2_blkcnt_t blockcnt,
		blk64_t /*ref_blk*/, int /*ref_offset*/, void *buf)
{
	std::fstream *file;
	file = (((struct filebuf *)(buf))->file);
	char *charbuf = ((struct filebuf *)buf)->buf;
	int allocated = extundelete_test_block_bitmap(fs->block_map, *blocknr);
	if(allocated == 0) {
		std::streampos pos = blockcnt * block_size_;
		(*file).seekp( pos );
		read_block64(fs, blocknr, 0, 0, 0, charbuf);
		(*file).write (charbuf, block_size_);
		return 0;
	}
	else {
		if(allocated == 1)
			Log::error << "Block " << *blocknr << " is allocated." << std::endl;
		else
			Log::error << "Block " << *blocknr << " is out of range." << std::endl;

		return (BLOCK_ABORT | BLOCK_ERROR);
	}
}

static void sanitize_file_name(std::string& str )
{
	// Remove a leading slash from the file name, and also ensure
	// there are no double slashes in the file name.
	size_t nextslash = str.find('/');
	do {
		if (nextslash+1 < str.size() && str.at(nextslash+1) == '/') {
			str.erase(nextslash, 1);
			continue;
		}
		else if(nextslash == 0)
			str.erase(nextslash, 1);

		nextslash = str.find('/', nextslash+1);
	} while(nextslash != std::string::npos);

}

errcode_t restore_inode(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t ino, const std::string& dname)
{
	errcode_t retval;
	std::streampos fsize = 0;
	blk64_t tsize;
	std::string fname (dname);
	sanitize_file_name(fname);
	struct ext2_inode *inode;
	inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
	retval = recover_inode(fs, jfs, ino, inode, 0);
	if( retval ) {
		Log::warn << "Unable to restore inode " << ino << " (" << fname
		<< "): No undeleted copies found in the journal." << std::endl;
		delete inode;
		return EU_RESTORE_FAIL;
	}
	blk64_t blocknum = 0;
	struct nth_block nb = {0, &blocknum};
	retval = extundelete_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_nth_block_num64, &nb);
	if( retval) {
		Log::warn << "Unable to restore inode " << ino << " (" << fname
		<< "): No data found." << std::endl;
		delete inode;
		return EU_RESTORE_FAIL;
	}
	if (blocknum != 0) {
		int allocated = extundelete_test_block_bitmap(fs->block_map, blocknum);
		if(allocated) {
			Log::warn << "Unable to restore inode " << ino << " (" << fname
			<< "): Space has been reallocated." << std::endl;
			delete inode;
			return EU_RESTORE_FAIL;
		}
	}

	std::string outputdir2 = outputdir + fname;
	size_t nextslash = outputdir2.find('/');
	do {
		mkdir(outputdir2.substr(0, nextslash).c_str(), 0755);
		nextslash = outputdir2.find('/', nextslash+1);
	} while(nextslash != std::string::npos);

	char *buf = new char[ block_size_];
	errcode_t flag = 0;
	std::string fname2 = fname;
	// Make sure inode corresponds to regular file
	if ( LINUX_S_ISREG(inode->i_mode) ) {

		std::fstream file ((outputdir + fname).c_str(), std::ios::in);
		for(int n = 1; file.is_open() && (n < 55); n++) {
			file.close();
			fname2 = fname + ".v" + to_string(n);
			file.open ((outputdir + fname2).c_str(), std::ios::in);
		}

		file.open((outputdir + fname2).c_str(), std::ios::binary|std::ios::out);
		if (file.is_open())
		{
			struct filebuf bufstruct = {&file, buf};
			flag = extundelete_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, write_block64, &bufstruct);
			file.seekg( 0, std::ios::end );
		    fsize = file.tellg();
			file.close();

			if(!flag) {
				std::streamoff rsize = EXT2_BLOCK_SIZE(fs->super) - inode->i_size % EXT2_BLOCK_SIZE(fs->super);
				if( rsize == EXT2_BLOCK_SIZE(fs->super) )
					rsize = 0;
				if( EXT2_I_SIZE(inode) > fsize ) {
					fsize = EXT2_I_SIZE(inode);
				}
				tsize = fsize - rsize;
				if ((retval = truncate( (outputdir + fname2).c_str(), tsize)) == 0) {
					Log::info << "Restored inode " << ino << " to file "
					<< (outputdir + fname2) << std::endl;
					retval = 0;
				} else {
					Log::warn << "Failed to restore inode " << ino << " to file "
					<< (outputdir + fname2) << ":"
					<< "Unable to set proper file size (" << tsize
					<< ")." << std::endl
					<< "Attempt to truncate returned error " << retval << ".";
					retval = EU_RESTORE_FAIL;
				}
			}
			else {
				Log::warn << "Failed to restore inode " << ino << " to file "
				<< (outputdir + fname2) << ":"
				<< "Some blocks were allocated." << std::endl;
				retval = EU_RESTORE_FAIL;
			}
		}
		else {
			Log::warn << "Failed to restore inode " << ino << " to file "
			<< (outputdir + fname2) << ":"
			<< "Could not open output file." << std::endl;
			retval = EU_RESTORE_FAIL;
		}
	}
	else {
		Log::info << "extundelete identified inode " << ino << " as "
		<< (outputdir + fname2) << ":"
		<< "The inode does not correspond to a regular file." << std::endl;
		retval = EU_RESTORE_FAIL;
	}

	delete inode;
	delete[] buf;
	return retval;
}

static void parse_inode_block(ext2_filsys fs, struct ext2_inode *inode, const char *buf, ext2_ino_t ino)
{
	int offset = (ino-1) % (block_size_/inode_size_);
	const char *inodebuf = buf + offset*inode_size_;
#ifdef WORDS_BIGENDIAN
	int n = 1 ;
	int hostorder = (*(char *) &n != 1);
	ext2fs_swap_inode_full(fs, (struct ext2_inode_large *) inode, (struct ext2_inode_large *) inodebuf, 0, EXT2_INODE_SIZE(fs->super));
#else
	memcpy(inode, inodebuf, EXT2_INODE_SIZE(fs->super) );
#endif
}

int get_journal_fs (ext2_filsys fs, ext2_filsys *jfs, std::string journal_filename) {

	errcode_t errcode;
	if (fs->super->s_journal_inum)
	{
		// Internal journal
		*jfs = fs;
	}
	else {
		// Read the journal superblock from an external journal.
		if(journal_filename.empty()) {
			Log::error << "Must specify the external journal with -j devicename" << std::endl;
			return EU_EXAMINE_FAIL;
		}
		io_manager io_mgr = unix_io_manager;
		errcode = ext2fs_open( journal_filename.c_str(),
			EXT2_FLAG_JOURNAL_DEV_OK, 0, 0, io_mgr, jfs);
		if (errcode) {
			Log::error << "Error opening external journal." << std::endl;
			return EU_EXAMINE_FAIL;
		}
	}
	return 0;	
}

int read_journal_superblock (ext2_filsys fs, ext2_filsys jfs,
		journal_superblock_t *journal_superblock) {

	errcode_t errcode;
	char *buf = new char[block_size_];
	// Read the journal superblock.
	if (fs->super->s_journal_inum)
	{
		// Read internal journal superblock
		struct ext2_inode *inode;
		inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
		ext2_ino_t journal_ino = fs->super->s_journal_inum;
		ext2fs_read_inode_full (fs, journal_ino, inode, EXT2_INODE_SIZE(fs->super));
		blk64_t blknum;
		errcode = ext2fs_bmap2(fs, journal_ino, inode, NULL, 0, 0, NULL, &blknum);
		if(errcode) {
			Log::error << "bmap returned " << errcode << std::endl;
			return EU_EXAMINE_FAIL;
		}
		read_block64(jfs, &blknum, 0, 0, 0, buf);
		delete inode;
	}
	else {
		// Read the journal superblock from an external journal.
		blk64_t jblk = jfs->super->s_first_data_block + 1;
		if (( errcode = read_block64(jfs, &jblk, 0, 0, 0, buf) )) {
			com_err("extundelete", errcode, "while reading journal superblock");
			return EU_EXAMINE_FAIL;
		}
	}

	// Convert buffer to journal superblock
	journal_superblock_to_cpu(buf);
	memcpy(journal_superblock, buf, sizeof(*journal_superblock));

	delete[] buf;
	// Sanity check to ensure there is no endianness problem.
	if(journal_superblock->s_header.h_magic != JFS_MAGIC_NUMBER) return EU_FS_ERR;
	return 0;
}

int print_inode(ext2_filsys fs, ext2_ino_t ino) {

		struct ext2_inode *inode;
		inode = (struct ext2_inode *) operator new(EXT2_INODE_SIZE(fs->super));
		ext2fs_read_inode_full (fs, ino, inode, EXT2_INODE_SIZE(fs->super));
		std::cout << "Contents of inode " << ino << ":" << std::endl;
		dump_hex_to(std::cout, reinterpret_cast<char const*> (inode), inode_size_);
		std::cout << std::endl;

		int allocated = extundelete_test_inode_bitmap(fs, ino);
		if (allocated)
			std::cout << "Inode is Allocated" << std::endl;
		else
			std::cout << "Inode is Unallocated" << std::endl;

		std::cout << *inode << std::endl;
		if (LINUX_S_ISDIR(inode->i_mode) && inode->i_blocks > 0)
			print_directory_inode(fs, inode, ino);
		delete inode;
		return 0;
}

int extundelete_make_outputdir(const char * const dirname, const char * const progname) {
	struct stat statbuf;
	errno = 0;
	if (stat(dirname, &statbuf) == -1)
	{
		if (errno != ENOENT)
		{
			int error = errno;
			Log::warn << std::flush;
			Log::error << progname << ": stat: " << dirname << ": "
			<< strerror(error) << std::endl;
			return EU_EXAMINE_FAIL;
		}
		else if (mkdir(dirname, 0755) == -1 && errno != EEXIST)
		{
			int error = errno;
			Log::warn << std::flush;
			Log::error << progname << ": failed to create output directory "
			<< dirname << ": " << strerror(error) << std::endl;
			return EU_EXAMINE_FAIL;
		}
		Log::info << "Writing output to directory " << dirname << std::endl;
	}
	else if (!S_ISDIR(statbuf.st_mode))
	{
		Log::warn << std::flush;
		Log::error << progname << ": " << dirname
		<< " exists but is not a directory!" << std::endl;
		return EU_EXAMINE_FAIL;
	}

	outputdir = std::string(dirname);
	return 0;
}

