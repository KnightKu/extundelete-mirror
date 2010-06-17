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
		classify_block(fs, commandline_block, commandline_group);
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
