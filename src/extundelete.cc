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
		- use bmap functions and file_read instead of block_iterate2
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

/* GNU headers */
#ifndef HAVE_GETOPT_H
#define getopt_long(a,b,c,d,e)  getopt((a),(b),(c))
struct option {
	const char *name;
	int has_arg;
	int *flag;
	int val;
};
#else
#include <getopt.h>
#endif

/* ext3/4 libraries */
#include <ext2fs/ext2fs.h>
#include "extundelete.h"
#include "extundelete-priv.h"
#include "block.h"

/* Definitions to allow extundelete compilation with old e2fsprogs */
#ifndef EXT4_EXTENTS_FL
#define EXT4_EXTENTS_FL                 0x00080000 /* Inode uses extents */
#endif

#ifndef HAVE_BLK64_T
typedef __u64          blk64_t;
#endif

#ifndef HAVE_EXT2FS_GET_GENERIC_BITMAP_START
uint32_t ext2fs_get_generic_bitmap_start(ext2fs_generic_bitmap bitmap)
{
	uint32_t *start = (uint32_t *) ((char *) bitmap + sizeof(errcode_t)
		+ sizeof(ext2_filsys));
	return *start;
}

uint32_t ext2fs_get_generic_bitmap_end(ext2fs_generic_bitmap bitmap)
{
	uint32_t *end = (uint32_t *) ((char *) bitmap + sizeof(errcode_t)
		+ sizeof(ext2_filsys) + sizeof(__u32) );
	return *end;
}
#endif

// extern variable definitions
std::string progname;
ext2_super_block super_block;
uint32_t block_size_;
uint32_t inodes_per_group_;
uint16_t inode_size_;
uint32_t inode_count_;
uint32_t block_count_;
ext2_group_desc* group_descriptor_table;
std::vector<uint32_t> tag_seq;
block_list_t tag_jblk;
block_list_t tag_fsblk;
block_list_t rvk_block;
journal_map_t journ_map;

std::string outputdir = "RECOVERED_FILES/";
bool commandline_superblock = false;
dgrp_t commandline_group = 0;
int commandline_inode_to_block = -1;
int commandline_inode = -1;
__s64 commandline_block = -1;
__s64 commandline_journal_block = -1;
int commandline_journal_transaction = -1;
bool commandline_journal = false;
bool commandline_dump_names = false;
bool commandline_directory = false;
long commandline_before = LONG_MAX;
long commandline_after = 0;
bool commandline_action = false;
std::string commandline_histogram;
int commandline_show_journal_inodes = -1;
std::string commandline_restore_file;
std::string commandline_restore_files;
std::string commandline_restore_directory;
std::string commandline_restore_inode;
bool commandline_restore_all = false;
bool commandline_show_hardlinks = false;
std::string commandline_journal_filename;
blk_t commandline_backup_superblock = 0;
blk_t commandline_block_size = 0;

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
	blk_t n;
	blk_t *blknum;
};

/* Returns 0 if block is unallocated; 1 if block is allocated;
 * 2 if block number is out of range
 * Use when a "bad block number" error should be treated like an
 * "allocated block" as an alternative to the ext2fs version.
*/
int extundelete_test_block_bitmap(ext2fs_block_bitmap block_map, blk_t blocknr)
{
	int allocated;
	if(blocknr == 0) return 0;
	if(blocknr < ext2fs_get_generic_bitmap_start(block_map) ||
		blocknr > ext2fs_get_generic_bitmap_end(block_map) )
	{
		allocated = 2;
	}
	else {
		allocated = ext2fs_test_block_bitmap(block_map, blocknr);
		if(allocated) allocated = 1;
	}
	return allocated;
}

// Returns the number of the first inode in the block.
ext2_ino_t block_to_inode(const ext2_filsys fs, blk_t block)
{
	//First, make a map of all the block group inode table locations
	std::map<blk_t, uint32_t> block_to_group_map;
	blk_t max_offset = (EXT2_INODES_PER_GROUP(fs->super) - 1) *
		EXT2_INODE_SIZE(fs->super);
	blk_t max_block = max_offset >> EXT2_BLOCK_SIZE_BITS(fs->super);
	for(uint32_t group = 0; group < fs->group_desc_count; group++ ) {
		blk_t block_nr = group_descriptor_table[group].bg_inode_table;
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

	uint32_t inode_table = group_descriptor_table[group].bg_inode_table;
	assert(block >= inode_table && (size_t)block_size_ * (block + 1) <= (size_t)block_size_ * inode_table + inodes_per_group_ * inode_size_);
	return 1 + group * inodes_per_group_ + (size_t)block_size_ * (block - inode_table) / inode_size_;
}

// Returns the number of blocks a non-sparse file would need for the data
inline blk64_t numdatablocks(const struct ext2_inode * const inode) {
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
inline blk64_t numblocks(const struct ext2_inode * const inode) {
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

#define JOURNAL_HAS_INCOMPAT_FEATURE(j,mask)                               \
        ((j)->s_header.h_blocktype == 4 &&                                  \
         ((j)->s_feature_incompat & ext2fs_cpu_to_be32((mask))))

size_t journ_tag_bytes(journal_superblock_t *jsb)
{
	if (JOURNAL_HAS_INCOMPAT_FEATURE(jsb, 2))
		return 12;
	else
		return 8;
}

// Changes a journal revoke header, as read from disk, to the same
// endianness as the computer this program is running on.
// FIXME: This function may fail if (for example) the partition was used on a
// 32-bit system, but we are running the program on a 64-bit cpu.
void journal_revoke_header_to_cpu(char *jrh)
{
	journal_header_to_cpu(jrh);
	char *rvkd = jrh + sizeof(journal_header_t);
	if (sizeof(int) == 2)
		be16_to_cpu( (uint16_t *) rvkd );
	else if (sizeof(int) == 4)
		be32_to_cpu( (uint32_t *) rvkd );
	else if (sizeof(int) == 8)
		be64_to_cpu( (uint64_t *) rvkd );
	else
		assert(sizeof(int) == 4);
}

// Changes a journal block tag, as read from disk, to the same
// endianness as the computer this program is running on.
void journal_block_tag_to_cpu(char *jbt, journal_superblock_t *jsb)
{
	int item = sizeof(uint32_t)/sizeof(char);
	be32_to_cpu( (uint32_t *) jbt );
	be32_to_cpu( (uint32_t *) &jbt[item*1] );
	if(journ_tag_bytes(jsb) > 8)
		be32_to_cpu( (uint32_t *) &jbt[item*2] );
}

// Changes a journal header, as read from disk, to the same
// endianness as the computer this program is running on.
void journal_header_to_cpu(char *jhead)
{
	int item = sizeof(uint32_t)/sizeof(char);
	be32_to_cpu( (uint32_t *) jhead );
	be32_to_cpu( (uint32_t *) &jhead[item*1] );
	be32_to_cpu( (uint32_t *) &jhead[item*2] );
}

// Changes the journal superblock, as read from disk, to the same
// endianness as the computer the program is running on.
void journal_superblock_to_cpu(char *jsb)
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


void print_version(void)
{
	std::cout << "extundelete version " << VERSION << std::endl;

	const char *ver;
	ext2fs_get_library_version (&ver, NULL);
	std::cout << "libext2fs version " << ver << std::endl;

	int n = 1 ;
	if(*(char *) &n == 1)  // True if the cpu is little endian.
		std::cout << "Processor is little endian." << std::endl;
	else
		std::cout << "Processor is big endian." << std::endl;
}

void print_usage(std::ostream& os)
{
  os << "Usage: " << progname << " [options] [--] device-file\n";
  os << "Options:\n";
  os << "  --version, -[vV]       Print version and exit successfully.\n";
  os << "  --help,                Print this help and exit successfully.\n";
  os << "  --superblock           Print contents of superblock in addition to the rest.\n";
  os << "                         If no action is specified then this option is implied.\n";
  os << "  --journal              Show content of journal.\n";
//  os << "  --show-path-inodes     Show the inode of each directory component in paths.\n";
//  os << "Filters:\n";
  os << "  --after dtime          Only process entries deleted on or after 'dtime'.\n";
  os << "  --before dtime         Only process entries deleted before 'dtime'.\n";
  os << "Actions:\n";
//  os << "  --inode-to-block ino   Print the block that contains inode 'ino'.\n";
  os << "  --inode ino            Show info on inode 'ino'.\n";
  os << "  --block blk            Show info on block 'blk'.\n";
//  os << "  --histogram=[atime|ctime|mtime|dtime|group]\n";
//  os << "                         Generate a histogram based on the given specs.\n";
//  os << "                         Using atime, ctime or mtime will change the\n";
//  os << "                         meaning of --after and --before to those times.\n";
//  os << "  --journal-block jblk   Show info on journal block 'jblk'.\n";
//  os << "  --journal-transaction seq\n";
//  os << "                         Show info on transaction with sequence number 'seq'.\n";
//  os << "  --dump-names           Write the path of files to stdout.\n";
//  os << "                         This implies --ls but suppresses it's output.\n";
//  os << "  --show-journal-inodes ino\n";
//  os << "                         Show copies of inode 'ino' still in the journal.\n";
  os << "  --restore-inode ino[,ino,...]\n";
  os << "                         Restore the file(s) with known inode number 'ino'.\n";
  os << "                         The restored files are created in ./RESTORED_FILES\n";
  os << "                         with their inode number as extension (ie, file.12345).\n";
  os << "  --restore-file 'path'  Will restore file 'path'. 'path' is relative to root\n";
  os << "                         of the partition and does not start with a '/' (it\n";
  os << "                         must be one of the paths returned by --dump-names).\n";
  os << "                         The restored file is created in the current\n";
  os << "                         directory as 'RECOVERED_FILES/path'.\n";
  os << "  --restore-files 'path' Will restore files which are listed in the file 'path'.\n";
  os << "                         Each filename should be in the same format as an option\n";
  os << "                         to --restore-file, and there should be one per line.\n";
  os << "  --restore-all          Attempts to restore everything.\n";
  os << "  -j journal             Reads an external journal from the named file.\n";
  os << "  -b blocknumber         Uses the backup superblock at blocknumber when opening\n";
  os << "                         the file system.\n";
  os << "  -B blocksize           Uses blocksize as the block size when opening the file\n";
  os << "                         system.  The number should be the number of bytes.\n";
}


// Main program implementation
int main(int argc, char* argv[])
{
	struct stat statbuf;
	int error = 0;
	ext2_filsys fs;
	io_manager io_mgr = unix_io_manager;
	errcode_t errcode;
	progname = argv[0];

	errcode = decode_options(argc, argv);
	if (errcode) {
		if (errcode == EU_STOP) return 0;
		std::cerr << "Error parsing command-line options." << std::endl;
		return EXIT_FAILURE;
	}

	// Sanity checks on the user.
	if (argc != 1) {
		if (argc == 0)
			std::cerr << progname << ": Missing device name. ";
		else
			std::cerr << progname << ": Too many non-options. ";

		std::cerr << "Use --help for a usage message." << std::endl;
		return EXIT_FAILURE;
	}

	// Ensure the file is a filesystem.
	errno = 0;
	if (stat (*argv, &statbuf) == -1) {
		error = errno;
		if (error != EOVERFLOW) {
			std::cout << std::flush;
			std::cerr << progname << ": stat \"" << *argv << "\": "
			<< strerror (error) << std::endl;
			return EXIT_FAILURE;
		}
	}
	if (error == 0) {
		if (S_ISDIR (statbuf.st_mode))
		{
			std::cerr << progname << ": \"" << *argv << "\" is a directory. You need "
			<< "to use the raw filesystem device (or a copy thereof)." << std::endl;
			return EXIT_FAILURE;
		}
		if (!S_ISBLK(statbuf.st_mode) && statbuf.st_size < 2048)
		{
			std::cerr << progname << ": \"" << *argv << "\" is too small to be a"
			<< " filesystem (" << statbuf.st_size << " bytes)." << std::endl;
			return EXIT_FAILURE;
		}
	}

	// Open the filesystem.
	errcode = ext2fs_open (*argv, 0, commandline_backup_superblock,
		commandline_block_size, io_mgr, &fs);

	if (errcode) {
		std::cout << std::flush;
		std::cerr << progname << ": failed to read-only open device \""
		<< *argv << "\": Error code " << errcode << std::endl;
		return EXIT_FAILURE; 
	}

	// Read constants from super block.
	errcode = load_super_block (fs);
	if (errcode == EU_FS_RECOVER) {
		char ans;
		std::cin.width (1);
		do {
			std::cout <<
			"If you decide to continue, extundelete may overwrite some of the deleted"
			<< std::endl <<
			"files and make recovering those files impossible.  You should unmount the"
			<< std::endl <<
			"file system and check it with fsck before using extundelete."
			<< std::endl <<
			"Would you like to continue? (y/n) " << std::endl;
			std::cin >> ans;
			if (ans == 'n')
				return 0;
		} while (ans != 'y');
	}
	else if (errcode) {
		std::cout << "Error: bad filesystem specified." << std::endl;
		return errcode;
	}

	errcode = examine_fs (fs);
	if (errcode) {
		std::cout << "Error: unable to examine filesystem." << std::endl;
		return errcode;
	}

	errcode = ext2fs_close (fs);
	if (errcode) {
		std::cerr << "Warning: Error closing filesystem; code " << errcode << std::endl;
	}
	// Sync here to ensure all recovered data is physically written to disk.
	sync();
	return 0;
}

//FIXME: Some of the string conversions are to long values that get stored as ints.
int decode_options(int& argc, char**& argv)
{
	int short_option;
	static int long_option;
	enum opts {
		opt_version,
		opt_superblock,
		opt_inode,
		opt_block,
		opt_after,
		opt_before,
		opt_histogram,
		opt_directory,
		opt_dump_names,
		opt_journal,
		opt_journal_block,
		opt_journal_transaction,
		opt_inode_to_block,
		opt_show_journal_inodes,
		opt_restore_file,
		opt_restore_files,
		opt_restore_directory,
		opt_restore_inode,
		opt_restore_all,
		opt_show_hardlinks,
		opt_help
	};
	struct option longopts[] = {
		{"help", 0, &long_option, opt_help},
		{"version", 0, &long_option, opt_version},
		{"superblock", 0, &long_option, opt_superblock},
		{"inode", 1, &long_option, opt_inode},
		{"block", 1, &long_option, opt_block},
		{"after", 1, &long_option, opt_after},
		{"before", 1, &long_option, opt_before},
		{"histogram", 1, &long_option, opt_histogram},
		{"directory", 0, &long_option, opt_directory},
		{"dump-names", 0, &long_option, opt_dump_names},
		{"journal", 0, &long_option, opt_journal},
		{"journal-block", 1, &long_option, opt_journal_block},
		{"journal-transaction", 1, &long_option, opt_journal_transaction},
		{"inode-to-block", 1, &long_option, opt_inode_to_block},
		{"show-journal-inodes", 1, &long_option, opt_show_journal_inodes},
		{"restore-inode", 1, &long_option, opt_restore_inode},
		{"restore-file", 1, &long_option, opt_restore_file},
		{"restore-files", 1, &long_option, opt_restore_files},
		{"restore-directory", 1, &long_option, opt_restore_directory},
		{"restore-all", 0, &long_option, opt_restore_all},
		{"show-hardlinks", 0, &long_option, opt_show_hardlinks},
		{NULL, 0, NULL, 0}
	};

	int exclusive1 = 0;
	int exclusive2 = 0;
	std::string hist_arg;

	while ((short_option = getopt_long(argc, argv, "j:vVb:B:", longopts, NULL)) != -1)
	{
		switch (short_option)
		{
		case 0:
			switch (long_option)
			{
			case opt_help:
				print_usage(std::cout);
				return EU_STOP;
			case opt_version:
				print_version();
				return EU_STOP;
			case opt_superblock:
				commandline_superblock = true;
				break;
			case opt_dump_names:
				commandline_dump_names = true;
				++exclusive1;
				++exclusive2;
				break;
			case opt_journal:
				commandline_journal = true;
				break;
			case opt_directory:
				commandline_directory = true;
				break;
			case opt_after:
				errno = 0;
				commandline_after = strtol(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --after " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				break;
			case opt_before:
				errno = 0;
				commandline_before = strtol(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --before " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				break;
			case opt_restore_inode:
				commandline_restore_inode = optarg;
				break;
			case opt_restore_file:
				commandline_restore_file = optarg;
				break;
			case opt_restore_files:
				commandline_restore_files = optarg;
				break;
			case opt_restore_directory:
				commandline_restore_directory = optarg;
				break;
			case opt_restore_all:
				commandline_restore_all = true;
				break;
			case opt_show_hardlinks:
				commandline_show_hardlinks = true;
				break;
			case opt_inode_to_block:
				errno = 0;
				commandline_inode_to_block = strtol(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --inode-to-block " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				if (commandline_inode_to_block < 1)
				{
					std::cout << std::flush;
					std::cerr << progname << ": --inode-to-block: inode "
					<< commandline_inode_to_block << " is out of range." << std::endl;
					return EU_DECODE_FAIL;
				}
				break;
			case opt_inode:
				errno = 0;
				commandline_inode = strtol(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --inode " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				if (commandline_inode < 1)
				{
					std::cout << std::flush;
					std::cerr << progname << ": --inode: inode " << commandline_inode
					<< " is out of range." << std::endl;
					return EU_DECODE_FAIL;
				}
				++exclusive1;
				++exclusive2;
				break;
			case opt_block:
				errno = 0;
				commandline_block = strtol(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --block " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				if (commandline_block < 0)
				{
					std::cout << std::flush;
					std::cerr << progname << ": --block: block " << commandline_block
					<< " is out of range." << std::endl;
					return EU_DECODE_FAIL;
				}
				++exclusive1;
				++exclusive2;
				break;
			case opt_show_journal_inodes:
				errno = 0;
				commandline_show_journal_inodes = strtol(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --show-journal-inodes " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				if (commandline_show_journal_inodes < 1)
				{
					std::cout << std::flush;
					std::cerr << progname << ": --show-journal-inodes: inode "
					<< commandline_show_journal_inodes << " is out of range."
					<< std::endl;
					return EU_DECODE_FAIL;
				}
				++exclusive1;
				++exclusive2;
				break;
			case opt_journal_transaction:
				errno = 0;
				commandline_journal_transaction = strtol(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --journal-transaction " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				break;
			case opt_histogram:
				commandline_histogram = optarg;
				break;
			}
			break;
		case 'j':
			commandline_journal_filename = std::string(optarg);
			break;
		case 'b':
			errno = 0;
			commandline_backup_superblock = strtoul(optarg, NULL, 10);
			if(errno) {
				std::cerr << "Invalid parameter: -b " << optarg << std::endl;
				return EU_DECODE_FAIL;
			}
			break;
		case 'B':
			errno = 0;
			commandline_block_size = strtoul(optarg, NULL, 10);
			if(errno) {
				std::cerr << "Invalid parameter: -B " << optarg << std::endl;
				return EU_DECODE_FAIL;
			}
			break;
		case 'v':
		case 'V':
			print_version();
			return EU_STOP;
		}
	}

	if (exclusive1 > 1)
	{
		std::cout << std::flush;
		std::cerr << progname << ": Only one of --group, --inode, --block, "
			<< "--journal-block, --dump-names or --show-journal-inodes may be "
			<< "specified." << std::endl;
		return EU_DECODE_FAIL;
	}
	if (exclusive2 > 1)
	{
		std::cout << std::flush;
		std::cerr << progname << ": Only one of --inode, --block, --search*, "
			<< "--journal-block, --dump-names or --show-journal-inodes may be "
			<< "specified." << std::endl;
		return EU_DECODE_FAIL;
	}
	bool outputwritten = false;
	commandline_action =
			(commandline_inode != -1 ||
			 commandline_block != -1 ||
			 commandline_journal_block != -1 ||
			 commandline_journal_transaction != -1 ||
			 commandline_dump_names ||
			 commandline_show_journal_inodes != -1 ||
			 !commandline_histogram.empty() ||
			 commandline_inode_to_block != -1 ||
			 !commandline_restore_inode.empty() ||
			 !commandline_restore_file.empty() ||
			 !commandline_restore_files.empty() ||
			 !commandline_restore_directory.empty() ||
			 commandline_restore_all ||
			 commandline_show_hardlinks);
	if (!commandline_action && !commandline_superblock)
	{
		std::cout << "No action specified; implying --superblock.\n";
		commandline_superblock = true;
		outputwritten = true;
	}
	if (commandline_before < LONG_MAX || commandline_after)
	{
		std::cout << "Only show and process deleted entries if they are deleted ";
		outputwritten = true;
		// date -d@1234567890 converts a value to a readable string (using GNU date)
		std::string after = to_string(commandline_after);
		std::string before = to_string(commandline_before);
		if (commandline_after)
			std::cout << "on or after " << after;
		if (commandline_before && commandline_after)
			std::cout << " and ";
		if (commandline_before)
			std::cout << "before " << before;
		std::cout << '.' << std::endl;
		if (commandline_before && commandline_after)
			assert(commandline_after < commandline_before);
	}
	if (outputwritten)
		std::cout << std::endl;

	argv += optind;
	argc -= optind;

	if (argc == 0)
	{
		print_usage(std::cerr);
		return EU_DECODE_FAIL;
	}
	return 0;
}


int load_super_block(ext2_filsys fs)
{
	int errcode = 0;
	// Frequently used constants.
	super_block = *(fs->super);
	uint32_t groups_ = fs->super->s_inodes_count / fs->super->s_inodes_per_group;
	block_size_ = EXT2_BLOCK_SIZE(fs->super);
	inodes_per_group_ = fs->super->s_inodes_per_group;
	inode_size_ = fs->super->s_inode_size;
	inode_count_ = fs->super->s_inodes_count;
	// FIXME: should use ext2fs_blocks_count instead
	block_count_ = fs->super->s_blocks_count;

	// Sanity checks.
	// ext2-based filesystem
	if (super_block.s_magic != 0xEF53) return EU_FS_ERR;
	assert(super_block.s_magic == 0xEF53);
	// All inodes belong to a group.
	if (groups_ * inodes_per_group_ != inode_count_) return EU_FS_ERR;
	assert(groups_ * inodes_per_group_ == inode_count_);
	// The inode bitmap has to fit in a single block.
	if (inodes_per_group_ > 8 * block_size_) return EU_FS_ERR;
	assert(inodes_per_group_ <= 8 * block_size_);
	// Each inode must fit within one block.
	if (inode_size_ > block_size_) return EU_FS_ERR;
	assert(inode_size_ <= block_size_);
	// inode_size must be a power of 2.
	assert(!((inode_size_ - 1) & inode_size_));
	// There should fit exactly an integer number of inodes in one block.
	if ((block_size_ / inode_size_) * inode_size_ != block_size_) return EU_FS_ERR;
	assert((block_size_ / inode_size_) * inode_size_ == block_size_);
	// File system must have a journal.
	if(!(super_block.s_feature_compat & EXT3_FEATURE_COMPAT_HAS_JOURNAL)) {
		std::cout << "ERROR: The specified device does not have a journal file.	"
			<< "This program only undeletes files from file systems with journals.";
		return EU_FS_ERR;
	}

	// superblock flags that don't matter for undeletion:
	// EXT2_FEATURE_COMPAT_DIR_PREALLOC, EXT2_FEATURE_COMPAT_RESIZE_INO,
	// EXT2_FEATURE_COMPAT_DIR_INDEX
	// FIXME: check the rest of the possible flags, too.
	if ((super_block.s_feature_compat & EXT2_FEATURE_COMPAT_IMAGIC_INODES))
		std::cout << "WARNING: Unknown file system feature: EXT2_FEATURE_COMPAT_IMAGIC_INODES\n";
	if ((super_block.s_feature_compat & EXT2_FEATURE_COMPAT_EXT_ATTR))
		std::cout << "WARNING: Extended attributes are not restored.\n";

	if ((super_block.s_feature_incompat & EXT2_FEATURE_INCOMPAT_COMPRESSION))
		std::cout << "WARNING: File systems with EXT2_FEATURE_INCOMPAT_COMPRESSION set (like this one) have not been tested.\n";
	if ((super_block.s_feature_incompat & EXT2_FEATURE_INCOMPAT_META_BG))
		std::cout << "WARNING: Unknown file system feature: EXT2_FEATURE_INCOMPAT_META_BG\n";
	if ((super_block.s_feature_incompat & EXT3_FEATURE_INCOMPAT_RECOVER))
	{
		std::cout << "WARNING: EXT3_FEATURE_INCOMPAT_RECOVER is set.\n"
		"The partition should be unmounted to undelete any files without further data loss.\n"
		"If the partition is not currently mounted, this message indicates \n"
		"it was improperly unmounted, and you should run fsck before continuing.\n";
		errcode = EU_FS_RECOVER;
	}
	if ((super_block.s_feature_incompat & EXT3_FEATURE_INCOMPAT_JOURNAL_DEV))
		std::cout << "WARNING: extundelete may have problems reading from an external journal.\n";

	// Initialize group_descriptor_table.
	//FIXME: may need to change to be compatible with newer file systems
	assert(EXT2_DESC_PER_BLOCK(&super_block) * sizeof(ext2_group_desc) == (size_t)block_size_);
	group_descriptor_table = new ext2_group_desc[groups_];
	for (uint32_t n = 0; n < fs->group_desc_count; n++)
	{
		group_descriptor_table[n] = fs->group_desc[n];
	}
	return errcode;
}

int print_entry(ext2_ino_t /*dir*/, int entry,
		struct ext2_dir_entry *dirent, int /*offset*/,
		int /*blocksize*/, char * /*buf*/, void * /*priv*/) {

	// ext2_filsys fs = (ext2_filsys) priv;
	struct ext2_dir_entry_2 *dirent2 = (struct ext2_dir_entry_2 *)dirent;
	if(!dirent2->inode) return 0;
	std::cout << std::string(dirent2->name, dirent2->name_len);
	for(unsigned int n = dirent2->name_len; n < 50; n++)
		std::cout << " ";
	std::cout << dirent2->inode;
	if(entry == DIRENT_DELETED_FILE ) {
		for(unsigned int n = to_string(dirent->inode).size(); n < 15; n++)
			std::cout << " ";
		std::cout << "Deleted";
	}
	std::cout << std::endl;

	return 0;
}

void print_directory_inode(ext2_filsys fs, struct ext2_inode *inode,
		ext2_ino_t ino)
{
	blk_t blocknr;
	int bmapflags = 0;
	char *buf = new char[block_size_];
	std::cout << "File name                                       ";
	std::cout << "| Inode number | Deleted status\n";
	for(blk_t n = 0; n < numdatablocks(inode); n++) {
		ext2fs_bmap(fs, ino, inode, NULL, bmapflags, n, &blocknr);
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
errcode_t read_block(ext2_filsys fs, blk_t *blocknr, e2_blkcnt_t blockcnt,
		blk_t /*ref_blk*/, int /*ref_offset*/, void *buf)
{
	errcode_t retval = io_channel_read_blk(fs->io, *blocknr, 1,
		reinterpret_cast<char *>(buf) + block_size_ * blockcnt);
	return retval;
}



// This function prints the data contained within the commandline_block
void classify_block(ext2_filsys fs)
{
	char* block = new char[block_size_];
	std::cout << "Block size: " << block_size_ << std::endl;

	blk_t blocknr = (blk_t) commandline_block;
	read_block(fs, &blocknr, 0, 0, 0, block);
	std::cout << "Contents of block " << commandline_block << ":" << std::endl;
	dump_hex_to(std::cout, block, block_size_);
	std::cout << std::endl;
	std::cout << "Block " << commandline_block << " is ";
	int allocated = extundelete_test_block_bitmap(fs->block_map, blocknr);
	if(!allocated) std::cout << "not ";
	std::cout << "allocated." << std::endl;
	std::cout << "Block " << blocknr <<" is in group " << commandline_group
	<< "." << std::endl;
	errcode_t retval = ext2fs_read_dir_block(fs, blocknr, block);
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
int get_block_nums(ext2_filsys /*fs*/, blk_t *blocknr, e2_blkcnt_t blockcnt,
		blk_t /*ref*/, int /*off*/, void *buf)
{
	blk_t *blkptr = reinterpret_cast<blk_t *>(buf);
	blkptr[blockcnt] = *blocknr;
	return 0;
}

// Store the nth block number in a buffer.
int get_nth_block_num(ext2_filsys /*fs*/, blk_t *blocknr, e2_blkcnt_t blockcnt,
		blk_t /*ref*/, int /*off*/, void *buf)
{
	struct nth_block *nth_blk = reinterpret_cast<nth_block *>(buf);
	if( (blk_t) blockcnt == nth_blk->n) {
		*(nth_blk->blknum) = *blocknr;
		return BLOCK_ABORT;
	}
	return 0;
}


bool is_journal(ext2_filsys fs, blk_t block)
{
	bool flag = false;
	ext2_ino_t ino = fs->super->s_journal_inum;
	struct ext2_inode *inode = new ext2_inode;
	errcode_t errcode;
	errcode = ext2fs_read_inode (fs, ino, inode);
	if(errcode){
		std::cout << "Warning: unable to read journal inode; code "
		<< errcode << std::endl;
		return flag;
	}
	blk_t *blocks = new blk_t[ numblocks(inode) ];

	errcode = ext2fs_block_iterate2 (fs, ino, 0, 0, get_block_nums, blocks);
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
int entry_iterate(ext2_ino_t /*dir*/, int entry,
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
int pair_names_with(ext2_filsys fs, ext2_filsys jfs, std::vector<ext2_ino_t>& inolist,
		ext2_ino_t ino, std::string dirname, int del,
		std::vector<ext2_ino_t>& parent_inos )
{
	// Look through the directory structure for what the file name of the inodes
	// should be.
	//int bmapflags = 0;
	if(inolist.size() == 0) { return 0; }

	errcode_t retval;
	struct ext2_inode *inode = new ext2_inode;
	char *buf = new char[block_size_];
	// If we are expecting a deleted directory block, then don't try to read
	// an inode in the file system -- it won't be the right one.
	if(del)
		retval = 1;
	else
		retval = ext2fs_read_inode(fs, ino, inode);

	if(retval == 0 && LINUX_S_ISDIR(inode->i_mode) ) {
		blk_t *blocks = new blk_t[ numdatablocks(inode) ];
		local_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_block_nums, blocks);
		//blk_t blocknr;
		//for(blk_t n = 0; n < numdatablocks(inode); n++) {
			//ext2fs_bmap(fs, ino, inode, buf, bmapflags, n, &blocknr);
			//blocks[n] = blocknr;
		//}

		for(unsigned long n = 0; n < numdatablocks(inode); n++) {
			blk_t blknum = blocks[n];
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
		struct ext2_inode *inode2 = new ext2_inode;
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

		for(blk_t n = 0; n < numdatablocks(inode); n++) {
			blk_t blknum = 0;
			struct nth_block nb = {n, &blknum};
			local_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_nth_block_num, &nb);
			//ext2fs_bmap(fs, ino, inode, buf, bmapflags, n, &blknum);
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
			blk_t blknum = *it;
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

/* FIXME: Add a function pointer parameter; should pass a function that displays
 * the status messages rather than sending them to stdout.
*/
int restore_directory(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t dirino, std::string dirname)
{
	std::vector<ext2_ino_t> recoverable_inodes;
	std::map<ext2_ino_t, uint32_t> deleted_inodes_map;
	std::map<ext2_ino_t, uint32_t>::iterator dit;
	block_list_t::reverse_iterator it;
	block_list_t::reverse_iterator jit;
	std::vector<uint32_t>::reverse_iterator sit;

	std::cout << "Searching for recoverable inodes in directory ";
	if(dirino == EXT2_ROOT_INO) std::cout << "/";
	std::cout << dirname << " ... " << std::endl;

	//First, make a map of all the block group inode table locations
	std::map<blk_t, int> block_to_group_map;
	blk_t max_offset = (EXT2_INODES_PER_GROUP(fs->super) - 1) *
		EXT2_INODE_SIZE(fs->super);
	blk_t max_block = max_offset >> EXT2_BLOCK_SIZE_BITS(fs->super);
	for(int group = 0; (unsigned int)group < fs->group_desc_count; group++ ) {
		blk_t block_nr = group_descriptor_table[group].bg_inode_table;
		block_to_group_map.insert(std::pair<blk_t, int>(block_nr, group));
	}

	// All the block numbers of deleted inode blocks
	char *buf = new char[block_size_];
	struct ext2_inode *inode = new ext2_inode;
	for ( it=tag_fsblk.rbegin(), jit=tag_jblk.rbegin(), sit=tag_seq.rbegin();
			it != tag_fsblk.rend();
			it++, jit++, sit++ )
	{
		blk_t blknum = *it;
		std::map<blk_t, int>::iterator bgit = block_to_group_map.lower_bound(blknum);
		if( bgit != block_to_group_map.begin() &&
				(bgit == block_to_group_map.end() || (*bgit).first != blknum) ) {
			bgit--;
		}

		// If the block contains inodes, find the deleted ones
		if ( blknum - (*bgit).first < max_block) {
			int group = (*bgit).second;
			blk_t blknum1 = group_descriptor_table[group].bg_inode_table;
			ext2_ino_t firstino = (blknum - blknum1) * block_size_ / inode_size_
				+ group * inodes_per_group_ + 1;
			read_journal_block(jfs, *jit, buf);
			for(ext2_ino_t ino = firstino; ino < firstino + block_size_/inode_size_ ;
					ino++ ) {
				parse_inode_block(inode, buf, ino);
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

	std::cout << recoverable_inodes.size() << " recoverable inodes found." << std::endl;
/*
	std::cout << "Deleted inodes:  ";
	for(std::list<ext2_ino_t>::iterator it2 = deleted_inodes.begin(); it2 != deleted_inodes.end(); it2++) {

		std::cout << (int) *it2 << "   " << std::flush;
	}
	std::cout << "Recoverable inodes:  ";
	for(std::vector<ext2_ino_t>::iterator it2 = recoverable_inodes.begin(); it2 != recoverable_inodes.end(); it2++) {

		std::cout << (int) *it2 << "   " << std::flush;
	}
//*/
	std::cout << "Looking through the directory structure for deleted files ... "
	<< std::endl;
	std::vector<ext2_ino_t>::size_type rsize = recoverable_inodes.size();
	int pnflag = !ext2fs_test_inode_bitmap(fs->inode_map, dirino);
	std::vector<ext2_ino_t> parent_inos(1, EXT2_ROOT_INO);
	pair_names_with(fs, jfs, recoverable_inodes, dirino, dirname, pnflag, parent_inos);
	std::cout << recoverable_inodes.size() << " recoverable inodes still lost." << std::endl;

	if(dirino == EXT2_ROOT_INO) {
		std::vector<ext2_ino_t>::iterator diit;
		for ( diit=recoverable_inodes.begin() ; diit != recoverable_inodes.end();
									 diit++ ) {
			std::ostringstream fname;
			fname << "file." << *diit;
			restore_inode(fs, jfs, *diit, fname.str());
		}
		if(rsize == 0)
			std::cout << "No files were undeleted." << std::endl;
	}
	else if(rsize == recoverable_inodes.size() ) {
		std::cout << "No files were undeleted." << std::endl;
	}
	return 0;
}

int examine_fs(ext2_filsys fs)
{
	errcode_t errcode;

	if (commandline_superblock && !commandline_journal)
	{
		// Print contents of superblock.
		std::cout << &super_block << std::endl;
	}

	if (commandline_action)
	{
		if (commandline_inode_to_block != -1)
			commandline_group = ext2fs_group_of_ino (fs, commandline_inode_to_block);
		std::cout << "Loading filesystem metadata ... " << std::flush;
		/* Note: for a 1 TB partition with 4k block size, these bitmaps
		 * require 40 MB memory.
		 */
		errcode = ext2fs_read_inode_bitmap(fs);
		errcode |= ext2fs_read_block_bitmap(fs);
		if (errcode) return errcode;
		std::cout << fs->super->s_inodes_count / fs->super->s_inodes_per_group
		<< " groups loaded." << std::endl;
	}

	// Check commandline options against superblock bounds.
	if (commandline_inode != -1)
	{
		if ((uint32_t)commandline_inode > inode_count_)
		{
			std::cout << std::flush;
			std::cerr << progname << ": --inode: inode " << commandline_inode 
			<< " is out of range. There are only " << inode_count_
			<< " inodes." << std::endl;
			return EU_EXAMINE_FAIL;
		}
		commandline_group = ext2fs_group_of_ino(fs, commandline_inode);
	}
	if (commandline_block != -1)
	{
		if (commandline_block >= fs->super->s_blocks_count || commandline_block == 0)
		{
			std::cout << std::flush;
			std::cerr << progname << ": --block: block " << commandline_block
			<< " is out of range." << std::endl
			<< "Valid block numbers are from 1 to "
			<< fs->super->s_blocks_count - 1 << std::endl;
			return EU_EXAMINE_FAIL;
		}
		commandline_group = ext2fs_group_of_blk(fs, commandline_block);
	}

	if (commandline_show_journal_inodes != -1)
	{
		if ((uint32_t)commandline_show_journal_inodes > inode_count_)
		{
			std::cout << std::flush;
			std::cerr << progname << ": --show-journal-inodes: inode "
			<< commandline_show_journal_inodes
			<< " is out of range. There are only " << inode_count_
			<< " inodes." << std::endl;
			return EU_EXAMINE_FAIL;
		}
		commandline_group = ext2fs_group_of_ino(fs, commandline_show_journal_inodes);
	}

	// Handle --inode
	if (commandline_inode != -1)
	{
		ext2_ino_t ino = commandline_inode;
		struct ext2_inode *inode = new ext2_inode;
		ext2fs_read_inode (fs, commandline_inode, inode);
		std::cout << "Contents of inode " << commandline_inode << ":" << std::endl;
		dump_hex_to(std::cout, reinterpret_cast<char const*> (inode), inode_size_);
		std::cout << std::endl;

		int allocated = ext2fs_test_inode_bitmap(fs->inode_map, ino);
		if (allocated)
			std::cout << "Inode is Allocated" << std::endl;
		else
			std::cout << "Inode is Unallocated" << std::endl;

		std::cout << "Group: " << commandline_group << std::endl;
		std::cout << *inode << std::endl;
		if (LINUX_S_ISDIR(inode->i_mode) && inode->i_blocks > 0)
			print_directory_inode(fs, inode, commandline_inode);
		delete inode;
	}

	// Handle --block
	if (commandline_block != -1 || (commandline_journal_block != -1 && commandline_journal))
	{
		classify_block(fs);
	}

	journal_superblock_t jsb;
	char *buf = new char[block_size_];
	journal_superblock_t *journal_superblock;
	ext2_filsys jfs;
	// Read the journal superblock.
	if (super_block.s_journal_inum)
	{
		// Read internal journal superblock
		jfs = fs;
		struct ext2_inode *inode = new ext2_inode;
		ext2_ino_t journal_ino = super_block.s_journal_inum;
		ext2fs_read_inode (fs, journal_ino, inode);
		blk_t blknum;
		errcode = ext2fs_bmap(fs, journal_ino, inode, NULL, 0, 0, &blknum);
		if(errcode) {
			std::cout << "bmap returned " << (int) errcode << std::endl;
			return EU_EXAMINE_FAIL;
		}
		read_block(jfs, &blknum, 0, 0, 0, buf);
		delete inode;
	}
	else {
		// Read the journal superblock from an external journal.
		if(commandline_journal_filename.empty()) {
			std::cout << "Must specify the external journal with -j devicename" << std::endl;
			return EU_EXAMINE_FAIL;
		}
		io_manager io_mgr = unix_io_manager;
		errcode = ext2fs_open( commandline_journal_filename.c_str(),
			EXT2_FLAG_JOURNAL_DEV_OK, 0, 0, io_mgr, &jfs);
		if (errcode) {
			std::cout << "Error opening external journal." << std::endl;
			return EU_EXAMINE_FAIL;
		}
		if ((errcode = io_channel_read_blk(jfs->io, jfs->super->s_first_data_block+1,
				-1024, buf) )) {
			com_err("extundelete", errcode, "while reading journal superblock");
			return EU_EXAMINE_FAIL;
		}
	}

	// Convert buffer to journal superblock
	journal_superblock_to_cpu(buf);
	journal_superblock = reinterpret_cast<journal_superblock_t *>(buf);
	jsb = *journal_superblock;
	if (commandline_superblock && commandline_journal)
	{
		std::cout << jsb << std::endl;
	}
	// Sanity check to ensure there is no endianness problem.
	assert(journal_superblock->s_header.h_magic == JFS_MAGIC_NUMBER);
	delete[] buf;

	if (commandline_journal_block != -1)
	{
		if (commandline_journal_block >= jsb.s_maxlen)
		{
			std::cout << std::flush;
			std::cerr << progname << ": --journal-block: block "
			<< commandline_journal_block
			<< " is out of range. There are only "<< jsb.s_maxlen
			<< " blocks in the journal." << std::endl;
			return EU_EXAMINE_FAIL;
		}
	}


	assert(jsb.s_header.h_magic == JFS_MAGIC_NUMBER);

	// Start recovery here.
	if (!commandline_restore_file.empty() ||
			!commandline_restore_files.empty() ||
			commandline_restore_all ||
			!commandline_restore_inode.empty() ||
			!commandline_restore_directory.empty() )
	{
		//Read the descriptors from the journal here
		errcode = init_journal(fs, jfs, &jsb);
		if (errcode) return EU_EXAMINE_FAIL;

		struct stat statbuf;
		errno = 0;
		if (stat(outputdir.c_str(), &statbuf) == -1)
		{
			if (errno != ENOENT)
			{
				int error = errno;
				std::cout << std::flush;
				std::cerr << progname << ": stat: " << outputdir << ": "
				<< strerror(error) << std::endl;
				return EU_EXAMINE_FAIL;
			}
			else if (mkdir(outputdir.c_str(), 0755) == -1 && errno != EEXIST)
			{
				int error = errno;
				std::cout << std::flush;
				std::cerr << progname << ": failed to create output directory "
				<< outputdir << ": " << strerror(error) << std::endl;
				return EU_EXAMINE_FAIL;
			}
			std::cout << "Writing output to directory " << outputdir << std::endl;
		}
		else if (!S_ISDIR(statbuf.st_mode))
		{
			std::cout << std::flush;
			std::cerr << progname << ": " << outputdir
			<< " exists but is not a directory!" << std::endl;
			return EU_EXAMINE_FAIL;
		}
	}
	// Handle --dump-names and --restore-all
	if (commandline_restore_all || commandline_dump_names)
		errcode = restore_directory (fs, jfs, EXT2_ROOT_INO, "");
	// Handle --restore-directory
	if (!commandline_restore_directory.empty()) {
		errcode = restore_file (fs, jfs, commandline_restore_directory.c_str());
	}
	// Handle --restore-file
	if (!commandline_restore_file.empty()) {
		errcode = restore_file(fs, jfs, commandline_restore_file);
	}
	// Handle --restore-files
	if (!commandline_restore_files.empty()) {
		std::ifstream infile;
		// Hopefully this is long enough
		unsigned int namelen = 2560;
		char *name = new char[namelen];

		infile.open (commandline_restore_files.c_str(), std::ifstream::in);

		while (infile.good()) {
			infile.getline (name, namelen);
			if(strlen(name) > 0)
				errcode = restore_file (fs, jfs, std::string(name) );
		}
		infile.close();
		delete[] name;
	}
	// Handle --restore-inode
	if (!commandline_restore_inode.empty())
	{
		std::istringstream is (commandline_restore_inode);
		int inodenr;
		char comma;
		while(is >> inodenr)
		{
			std::ostringstream oss;
			oss << "file." << inodenr;
			restore_inode (fs, jfs, inodenr, oss.str());
			is >> comma;
		};
	}
/*
  // Handle --histogram
  if (commandline_histogram)
  {
    std::cout << '\n';
    if (commandline_deleted || commandline_histogram == hist_dtime)
      std::cout << "Only showing deleted entries.\n";
    if (commandline_histogram == hist_atime ||
        commandline_histogram == hist_ctime ||
	commandline_histogram == hist_mtime ||
	commandline_histogram == hist_dtime)
      hist_init(commandline_after, commandline_before);
    // Run over all (requested) groups.
    for (int group = 0, ibase = 0; group < groups_; ++group, ibase += inodes_per_group_)
    {
      // Run over all inodes.
      for (int bit = 0, inode_number = ibase + 1; bit < inodes_per_group_; ++bit, ++inode_number)
      {
	ext2_inode * inode(get_inode(inode_number));
	if (commandline_deleted && !inode->is_deleted())
	  continue;
	if ((commandline_histogram == hist_dtime || commandline_histogram == hist_group) && !inode->has_valid_dtime())
          continue;
	if (commandline_directory && !is_directory(inode))
	  continue;
	if (commandline_allocated || commandline_unallocated)
	{
	  bitmap_ptr bmp = get_bitmap_mask(bit);
	  bool allocated = (inode_bitmap[group][bmp.index] & bmp.mask);
	  if (commandline_allocated && !allocated)
	    continue;
	  if (commandline_unallocated && allocated)
	    continue;
	}
	time_t xtime = 0;
	if (commandline_histogram == hist_dtime)
	  xtime = inode->dtime();
	else if (commandline_histogram == hist_atime)
	{
	  xtime = inode->atime();
	  if (xtime == 0)
	    continue;
        }
	else if (commandline_histogram == hist_ctime)
	{
	  xtime = inode->ctime();
	  if (xtime == 0)
	    continue;
	}
	else if (commandline_histogram == hist_mtime)
	{
	  xtime = inode->mtime();
	  if (xtime == 0)
	    continue;
        }
	if (xtime && commandline_after <= xtime && xtime < commandline_before)
	  hist_add(xtime);
	if (commandline_histogram == hist_group)
	{
	  if (commandline_after && commandline_after > (time_t)inode->dtime())
	    continue;
	  if (commandline_before && (time_t)inode->dtime() >= commandline_before)
	    continue;
	  hist_add(group);
        }
      }
    }
    hist_print();
  }

  // Handle --search-inode
  if (commandline_search_inode != -1)
  {
    std::cout << "Inodes refering to block " << commandline_search_inode << ':' << std::flush;
    for (uint32_t inode = 1; inode <= inode_count_; ++inode)
    {
      ext2_inode * ino = get_inode(inode);
      find_block_data_st data;
      data.block_looking_for = commandline_search_inode;
      data.found_block = false;
      bool reused_or_corrupted_indirect_block2 = iterate_over_all_blocks_of(ino, find_block_action, &data);
      assert(!reused_or_corrupted_indirect_block2);
      if (data.found_block)
        std::cout << ' ' << inode << std::flush;
    }
    std::cout << '\n';
  }

  // Handle --show-journal-inodes
  if (commandline_show_journal_inodes != -1)
    show_journal_inodes(commandline_show_journal_inodes);

//*/
	if(super_block.s_journal_inum == 0)
		ext2fs_close(jfs);
	return 0;
}

/* Read a single block from the journal */
int read_journal_block(ext2_filsys fs, blk_t n, char *buf)
{
	if(super_block.s_journal_inum) {
		read_block(fs, &n, 0, 0, 0, buf);
	}
	else {
		// External journal
		errcode_t retval;
		if((retval = io_channel_read_blk(fs->io, n, 1, buf) )) {
			com_err("extundelete", retval, "while reading journal block");
			exit(1);
		}
	}
	return 0;
}

/*
 * Read contents of journal file into global variables
 * FIXME: When libext2fs gets 64-bit support, change this to read 64-bit
 * block numbers.
*/
int init_journal(ext2_filsys fs, ext2_filsys jfs, journal_superblock_t *jsb)
{
	// Minimally validate input
	assert(fs->super->s_inodes_count != 0);
	assert(jsb->s_blocksize != 0);

	// Find the block range used by the journal.
	//assert(super_block.s_journal_inum);
	struct ext2_inode *journal_inode = new ext2_inode;
	ext2_ino_t journal_ino = super_block.s_journal_inum;
	ext2fs_read_inode (fs, journal_ino, journal_inode);

	// Load the journal descriptors into memory.
	std::cout << "Loading journal descriptors ... " << std::flush;

	// Apparently, some bug exists that allocates one too many journal blocks,
	// so add one to the number of data blocks expected to prevent a memory
	// error during block_iterate2.
	blk_t *blocks = new blk_t[ 1 + jsb->s_maxlen ];
	blocks[ jsb->s_maxlen ] = 0;
	if(super_block.s_journal_inum) {
		ext2fs_block_iterate2 (fs, journal_ino, BLOCK_FLAG_DATA_ONLY, 0, get_block_nums, blocks);
	}
	else
		for(blk_t n = 0; n < jsb->s_maxlen; n++)
			blocks[n] = n;

	char *buf = new char[ block_size_];
	char *descbuf = new char[ block_size_];
	uint32_t number_of_descriptors = 0;
	std::vector<triad<uint32_t,uint32_t,uint32_t> > j_tags;

	for (blk_t n = 0; n < jsb->s_maxlen; n++)
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
			#ifdef JDEBUG
				std::cout << std::endl << "Block " << blocks[n];
				std::cout << ": start sequence " << seq;
			#endif
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

						j_tags.push_back(triad<uint32_t,uint32_t,uint32_t>
							(seq,blocks[m],jbt->t_blocknr) );
					#ifdef JDEBUG
						std::cout << std::endl << "Journalled block " << blocks[m];
						std::cout << " is a copy of block " << jbt->t_blocknr;
					#endif
						m++;
					}
					else
					{
						j_tags.push_back(triad<uint32_t,uint32_t,uint32_t>
							(seq,blocks[n],jbt->t_blocknr) );
					#ifdef JDEBUG
						std::cout << std::endl << "Journalled block " << blocks[n];
						std::cout << " is a copy of block " << jbt->t_blocknr;
					#endif
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
			#ifdef JDEBUG
				std::cout << std::endl << "Commit block " << blocks[n] << ": ";
				std::cout << "Sequence " << descriptor->h_sequence;
			#endif
				break;
			}
			case JFS_REVOKE_BLOCK:
			{
				journal_revoke_header_t* rvk =
					reinterpret_cast<journal_revoke_header_t*>(buf);
				journal_revoke_header_to_cpu( (char *)rvk );
			#ifdef JDEBUG
				std::cout << std::endl;
				std::cout << "Revoke block " << blocks[n] << ":";
				std::cout << " (size " << rvk->r_count << ") ";
			#endif
				int64_t init_count = sizeof(journal_revoke_header_t);
				for (int64_t count = init_count; count < rvk->r_count;
						count += sizeof(blk_t))
				{
					uint32_t *block = (uint32_t *) &buf[count];
					be32_to_cpu(block);
					rvk_block.push_back(*block);
				#ifdef JDEBUG
					if (count != init_count)
						std::cout << ", ";
					std::cout << *block;
				#endif
				}
				break;
			}
			case JFS_SUPERBLOCK_V1:
			{
			#ifdef JDEBUG
				std::cout << std::endl << "Found Journal Superblock (v1)";
			#endif
				break;
			}
			case JFS_SUPERBLOCK_V2:
			{
			#ifdef JDEBUG
				std::cout << std::endl << "Found Journal Superblock (v2)";
			#endif
				break;
			}
			default:
			{
				std::cout << std::flush;
				std::cerr << "WARNING: Unexpected blocktype ("
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
	for(std::vector<triad<uint32_t,uint32_t,uint32_t> >::iterator it = j_tags.begin();
			it != j_tags.end(); it++) {
		tag_seq.push_back( it->first );
		tag_jblk.push_back( it->second );
		tag_fsblk.push_back( it->third );
		// journ_map order is (fsblk, (jblk, seq))
		block_pair_t val = block_pair_t(it->second, it->first);
		journal_map_item point = journal_map_item(it->third, val);
		journ_map.insert( point );
	}

	std::cout << number_of_descriptors << " descriptors loaded." << std::endl;

#ifdef JDEBUG
	block_list_t::iterator it;
	std::cout << "rvk_block contains:";
	// All the block numbers of deleted directory blocks
	for ( it=rvk_block.begin() ; it != rvk_block.end(); it++ )
		std::cout << " " << *it;
	std::cout << std::endl;
	//std::cout << "tag_seq contains:";
	//for ( it=tag_seq.begin() ; it != tag_seq.end(); it++ )
	//	std::cout << " " << *it;
	//std::cout << std::endl;
	std::cout << "tag_jblk contains:";
	for ( it=tag_jblk.begin() ; it != tag_jblk.end(); it++ )
		std::cout << " " << *it;
	std::cout << std::endl;
	std::cout << "tag_fsblk contains:";
	for ( it=tag_fsblk.begin() ; it != tag_fsblk.end(); it++ )
		std::cout << " " << *it;

	std::cout << std::endl;
#endif

	delete journal_inode;
	delete[] blocks;
	return 0;
}

// Modifies the inode number of match_struct priv
// to set the inode of the directory entry
int match_name(ext2_dir_entry *dirent, int /*off*/, int /*blksize*/,
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

int match_name2(ext2_ino_t /*dir*/, int /*entry*/,
		struct ext2_dir_entry *dirent, int /*offset*/,
		int /*blocksize*/, char * /*buf*/, void *priv)
{
	return match_name(dirent, 0, 0, 0, priv);
}

bool compare_sequence(block_pair_t a, block_pair_t b) {
	if(a.second < b.second) return true;
	else return false;
}

// Returns block number of a copy of blknum that resides in the journal
blk_t journ_get_dir_block(ext2_filsys /*fs*/, blk_t blknum, void * /*buf*/)
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

ext2_ino_t find_inode(ext2_filsys fs, ext2_filsys jfs, struct ext2_inode *inode,
		std::string curr_part, blk_t *blocks, int search_flags)
{
	char *buf = new char[block_size_];
	blk_t blocknr;
	ext2_ino_t ino2 = 0;
	struct match_struct tmp = {0, curr_part};
	struct match_struct *priv = &tmp;
	ext2_ino_t *new_ino = new ext2_ino_t;

	for (uint32_t n = 0; n < numdatablocks(inode); ++n)
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
int restore_file(ext2_filsys fs, ext2_filsys jfs, const std::string& fname)
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
	struct ext2_inode *inode = new ext2_inode;
	retval = recover_inode(fs, jfs, ino, inode, 0);
	blk_t *blocks = NULL;
	if (retval==0) {
		//A bad inode may cause way too much memory to be allocated
		try {
			//FIXME: find a way to do this without allocating that big chunk in blocks variable
			blocks = new blk_t[ numdatablocks(inode) ];
			local_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_block_nums, blocks);
		//blk_t blocknr;
		//for(blk_t n = 0; n < numdatablocks(inode); n++) {
			//ext2fs_bmap(fs, ino, inode, buf, bmapflags, n, &blocknr);
			//blocks[n] = blocknr;
		//}
		}
		catch (std::exception& error) {
			delete[] blocks;
			blocks = NULL;
		}
	}

	char *buf = new char[ block_size_];

	struct ext2_inode *inode2 = new ext2_inode;
	ext2fs_read_inode (fs, ino, inode2);
	blk_t *blocks2 = new blk_t[ numdatablocks(inode2) ];
	ext2fs_block_iterate2 (fs, ino, BLOCK_FLAG_DATA_ONLY, 0, get_block_nums, blocks2);

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

	//std::cout << "Post directory block search inode number: " << ino << std::endl;
	//std::cout << "Next file part: " << curr_part << std::endl;
	//std::cout << "File name and place: " << fname << "   " << place << std::endl;

	char *buf2 = new char[ block_size_];
	while(curr_part != "") {
		ino2 = 0;
		retval = recover_inode(fs, jfs, ino, inode, 0);
		if(retval != 0) break;

		blk_t *blocks3 = NULL;
		//A bad inode may cause way too much memory to be allocated
		try {
			blocks3 = new blk_t[ numdatablocks(inode) ];
			local_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_block_nums, blocks3);

		//blk_t blocknr;
		//for(blk_t n = 0; n < numdatablocks(inode); n++) {
			//ext2fs_bmap(fs, ino, inode, NULL, bmapflags, n, &blocknr);
			//blocks3[n] = blocknr;
		//}

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

	// std::cout << "Post-revoke search current inode number: " << ino << std::endl;
	// std::cout << "Next file part: " << curr_part << std::endl;

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
		std::cout << "Failed to restore file " << fname << std::endl;
		std::cout << "Could not find correct inode number past inode " << ino
		<< "." << std::endl;
		return EU_RESTORE_FAIL;
	}
}

inline errcode_t inode_is_valid(const struct ext2_inode * const inode)
{
	/* FIXME: Could also check that at least one block pointer is nonzero */
	/* FIXME: Could also check that file size and block count is consistent */
	/* Remember this must account for both files and directories */
	return
		inode->i_dtime == 0 &&
		numblocks(inode) > 0 &&
		numdatablocks(inode) > 0 &&
		inode->i_blocks <= super_block.s_blocks_count &&
		inode->i_blocks > 0 &&
		inode->i_links_count > 0;
}

// Use ver = 0 to get previous behavior
errcode_t recover_inode(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t ino,
		struct ext2_inode *&inode, int ver)
{
	if ((ino == 0) || (ino > fs->super->s_inodes_count))
		return EXT2_ET_BAD_INODE_NUM;
	int group = ext2fs_group_of_ino(fs, ino);
	blk_t blknum1 = group_descriptor_table[group].bg_inode_table;
	blk_t blknum2 = (ino - 1 - group * inodes_per_group_) * inode_size_ / block_size_;
	blk_t blknum = blknum1 + blknum2;

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
	std::cout << "oldblks contains:";
	for ( oit=oldblks2.begin() ; oit != oldblks2.end(); oit++ )
		std::cout << " " << (*oit).first;
	std::cout << std::endl;
//*/
	bool found = false;
	// If the inode is not allocated, we can just pick the first valid inode
	bool deletedfound = !ext2fs_test_inode_bitmap(fs->inode_map, ino) &&
		commandline_before == LONG_MAX &&
		commandline_after == 0;

	std::list<block_pair_t>::reverse_iterator rit;
	char *buf = new char[block_size_];
	for ( rit=oldblks2.rbegin() ; rit != oldblks2.rend(); ++rit ) {
		read_journal_block(jfs, ((*rit).first), buf);
		parse_inode_block(inode, buf, ino);
		if (inode->i_dtime != 0 && ((int64_t) (inode->i_dtime)) >= commandline_after &&
				((int64_t) (inode->i_dtime)) <= commandline_before)
		{
			deletedfound = true;
			continue;
		}
		if( deletedfound && inode_is_valid(inode)) {
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

int write_block(ext2_filsys fs, blk_t *blocknr, e2_blkcnt_t blockcnt,
		blk_t /*ref_blk*/, int /*ref_offset*/, void *buf)
{
	std::fstream *file;
	file = (((struct filebuf *)(buf))->file);
	char *charbuf = ((struct filebuf *)buf)->buf;
	int allocated = extundelete_test_block_bitmap(fs->block_map, *blocknr);
	if(allocated == 0) {
		std::streampos pos = blockcnt * block_size_;
		(*file).seekp( pos );
		io_channel_read_blk(fs->io, *blocknr, 1, reinterpret_cast<char *>(charbuf) );
		(*file).write (charbuf, block_size_);
		return 0;
	}
	else {
		if(allocated == 1)
			std::cout << "Block " << *blocknr << " is allocated." << std::endl;
		else
			std::cout << "Block " << *blocknr << " is out of range." << std::endl;

		//FIXME: should probably return (BLOCK_ABORT | BLOCK_ERROR)
		return -1;
	}
}

void sanitize_file_name(std::string& str )
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

int restore_inode(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t ino, const std::string& dname)
{
	errcode_t retval;
	struct ext2_inode *inode = new ext2_inode;
	std::string fname (dname);
	sanitize_file_name(fname);

	retval = recover_inode(fs, jfs, ino, inode, 0);
	if( retval ) {
		std::cout << "Unable to restore inode " << ino << " (" << fname;
		std::cout << "): No undeleted copies found in the journal." << std::endl;
		delete inode;
		return EU_RESTORE_FAIL;
	}
	blk_t blocknum = 0;
	//retval = ext2fs_bmap(fs, ino, inode, NULL, 0, 0, &blocknum);
	struct nth_block nb = {0, &blocknum};
	retval = local_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, get_nth_block_num, &nb);
	if( retval) {
		std::cout << "Unable to restore inode " << ino << " (" << fname;
		std::cout << "): No data found." << std::endl;
		delete inode;
		return EU_RESTORE_FAIL;
	}
	if (blocknum != 0) {
		int allocated = extundelete_test_block_bitmap(fs->block_map, blocknum);
		if(allocated) {
			std::cout << "Unable to restore inode " << ino << " (" << fname;
			std::cout << "): Space has been reallocated." << std::endl;
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
	int flag = 0;
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
			flag = local_block_iterate3 (fs, *inode, BLOCK_FLAG_DATA_ONLY, NULL, write_block, &bufstruct);
/*
			unsigned int numbytes;
			unsigned int bytesread = 0;
			int bmapflags = 0;
			blk_t n = 0;
			ext2_file_t infile;
			ext2fs_file_open2(fs, ino, inode, 0, &infile);
			blk_t blocknr;
			do {
				ext2fs_bmap(fs, ino, inode, buf, bmapflags, n, &blocknr);
				int allocated = extundelete_test_block_bitmap(fs->block_map, blocknr);
				if(!allocated) {
					ext2fs_file_read (infile, buf, block_size_, &numbytes);
					bytesread += numbytes;
					file.write (buf, numbytes);
				}
				else {
					flag = -1;
					break;
				}
				n++;
			} while( (numbytes == block_size_) && (bytesread < EXT2_I_SIZE(inode)) );

			ext2fs_file_close(infile);
*/
			file.close();

			if(!flag) {
				if (truncate( (outputdir + fname2).c_str(), EXT2_I_SIZE(inode)) == 0) {
					std::cout << "Restored inode " << ino << " to file ";
					std::cout << (outputdir + fname2) << std::endl;
					retval = 0;
				} else {
					std::cout << "Failed to restore inode " << ino << " to file ";
					std::cout << (outputdir + fname2) << ":";
					std::cout << "Unable to set proper file size." << std::endl;
					retval = EU_RESTORE_FAIL;
				}
			}
			else {
				std::cout << "Failed to restore inode " << ino << " to file ";
				std::cout << (outputdir + fname2) << ":";
				std::cout << "Some blocks were allocated." << std::endl;
				retval = EU_RESTORE_FAIL;
			}
		}
		else {
			std::cout << "Failed to restore inode " << ino << " to file ";
			std::cout << (outputdir + fname2) << ":";
			std::cout << "Could not open output file." << std::endl;
			retval = EU_RESTORE_FAIL;
		}
	}
	else {
		std::cout << "Failed to restore inode " << ino << " to file ";
		std::cout << (outputdir + fname2) << ":";
		std::cout << "Inode does not correspond to a regular file." << std::endl;
		retval = EU_RESTORE_FAIL;
	}

	delete inode;
	delete[] buf;
	return retval;
}

void parse_inode_block(struct ext2_inode *inode, const char *buf, ext2_ino_t ino)
{
	int offset = (ino-1) % (block_size_/inode_size_);
	const char *inodebuf = buf + offset*inode_size_;
	int item = sizeof(uint16_t)/sizeof(char);
	inode->i_mode = le16_to_cpu( (uint16_t *) inodebuf );
	inode->i_uid = le16_to_cpu( (uint16_t *) &inodebuf[item*1] );
	inode->i_size = le32_to_cpu( (uint32_t *) &inodebuf[item*2] );
	inode->i_atime = le32_to_cpu( (uint32_t *) &inodebuf[item*4] );
	inode->i_ctime = le32_to_cpu( (uint32_t *) &inodebuf[item*6] );
	inode->i_mtime = le32_to_cpu( (uint32_t *) &inodebuf[item*8] );
	inode->i_dtime = le32_to_cpu( (uint32_t *) &inodebuf[item*10] );
	inode->i_gid = le16_to_cpu( (uint16_t *) &inodebuf[item*12] );
	inode->i_links_count = le16_to_cpu( (uint16_t *) &inodebuf[item*13] );
	inode->i_blocks = le32_to_cpu( (uint32_t *) &inodebuf[item*14] );
	inode->i_flags = le32_to_cpu( (uint32_t *) &inodebuf[item*16] );
	// The next part of the structure was renamed in e2fsprogs 1.40 (2007).
	// We skip using it for compatibility with old e2fsprogs for now.
	// inode->i_reserved1 = le32_to_cpu( (uint32_t *) &inodebuf[item*18] );
	//FIXME: Double-check that this results in the correct inode with extents.
	if(inode->i_flags & EXT4_EXTENTS_FL) {
		/* Extent data are byte-swapped on access, not on read from disk */
		for(int n=0; n < EXT2_N_BLOCKS; n++) {
			inode->i_block[n] = *( (uint32_t *) &inodebuf[item*(20 + 2*n)] );
		}
	}
	else {
		for(int n=0; n < EXT2_N_BLOCKS; n++) {
			inode->i_block[n] = le32_to_cpu( (uint32_t *) &inodebuf[item*(20 + 2*n)] );
		}
	}
	inode->i_generation = le32_to_cpu( (uint32_t *) &inodebuf[item*48] );
	//FIXME: file_acl is a block number of the extended attributes: we
	// should restore that block along with the file.
	inode->i_file_acl = le32_to_cpu( (uint32_t *) &inodebuf[item*50] );
	inode->i_dir_acl = le32_to_cpu( (uint32_t *) &inodebuf[item*52] );
	inode->i_faddr = le32_to_cpu( (uint32_t *) &inodebuf[item*54] );
	//inode->i_frag = inodebuf[item*56];
	//inode->i_fsize = inodebuf[item*56 + sizeof(inode->i_frag)];
	// Gap in useful info of 16 bits here.
  //FIXME: need to change behavior depending on the fs operating system
	inode->osd2.linux2.l_i_uid_high = le16_to_cpu( (uint16_t *) &inodebuf[item*58] );
	inode->osd2.linux2.l_i_gid_high = le16_to_cpu( (uint16_t *) &inodebuf[item*60] );
	inode->osd2.linux2.l_i_reserved2 = le32_to_cpu( (uint32_t *) &inodebuf[item*62] );
}
