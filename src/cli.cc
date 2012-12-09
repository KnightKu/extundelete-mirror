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

#ifndef EXT2_FLAG_64BITS
#define EXT2_FLAG_64BITS 0x20000
#endif

static std::string progname;
static std::string commandline_histogram;
static std::string commandline_journal_filename;
std::string commandline_restore_directory;
static std::string commandline_restore_file;
static std::string commandline_restore_files;
static std::string commandline_restore_inode;
static std::string commandline_fsname;

static bool commandline_action = false;
static bool commandline_journal = false;
static bool commandline_restore_all = false;
static bool commandline_superblock = false;

static ext2_ino_t commandline_inode = 0;
static ext2_ino_t commandline_inode_to_block = 0;
static ext2_ino_t commandline_show_journal_inodes = 0;

static blk_t commandline_backup_superblock = 0;
static blk64_t commandline_block = 0;
static blk_t commandline_block_size = 0;
static blk_t commandline_journal_block = 0;

static __u32 commandline_journal_transaction = 0;

long commandline_before = LONG_MAX;
long commandline_after = 0;


static void print_version(void)
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


static void print_usage(std::ostream& os, std::string cmd)
{
  os << "Usage: " << cmd << " [options] [--] device-file\n";
  os << "Options:\n";
  os << "  --version, -[vV]       Print version and exit successfully.\n";
  os << "  --help,                Print this help and exit successfully.\n";
  os << "  --superblock           Print contents of superblock in addition to the rest.\n";
  os << "                         If no action is specified then this option is implied.\n";
  os << "  --journal              Show content of journal.\n";
  os << "  --after dtime          Only process entries deleted on or after 'dtime'.\n";
  os << "  --before dtime         Only process entries deleted before 'dtime'.\n";
  os << "Actions:\n";
  os << "  --inode ino            Show info on inode 'ino'.\n";
  os << "  --block blk            Show info on block 'blk'.\n";
  os << "  --restore-inode ino[,ino,...]\n";
  os << "                         Restore the file(s) with known inode number 'ino'.\n";
  os << "                         The restored files are created in ./RECOVERED_FILES\n";
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
  os << "  --log 0                Make the program silent.\n";
  os << "  --log filename         Logs all messages to filename.\n";
  os << "--log D1=0,D2=filename   Custom control of log messages with comma-separated\n";
  os << "   Examples below:       list of options.  Dn must be one of info, warn, or\n";
  os << "   --log info,error      error.  Omission of the '=name' results in messages\n";
  os << "   --log warn=0          with the specified level to be logged to the console.\n";
  os << "   --log error=filename  If the parameter is '=0', logging for the specified\n";
  os << "                         level will be turned off.  If the parameter is\n";
  os << "                         '=filename', messages with that level will be written\n";
  os << "                         to filename.\n";
}


static errcode_t examine_fs(ext2_filsys fs)
{
	errcode_t errcode;

	if (commandline_superblock && !commandline_journal) {
		// Print contents of superblock.
		std::cout << fs->super << std::endl;
	}

	if (commandline_action)
	{
		Log::info << "Loading filesystem metadata ... " << std::flush;
		/* Note: for a 1 TB partition with 4k block size, these bitmaps
		 * require 40 MB memory. */
		errcode = ext2fs_read_inode_bitmap(fs);
		errcode |= ext2fs_read_block_bitmap(fs);
		if (errcode) return errcode;
		Log::info << fs->super->s_inodes_count / fs->super->s_inodes_per_group
		<< " groups loaded." << std::endl;
	}

	// Check commandline options against superblock bounds.
	if (commandline_inode != 0)
	{
		if ((uint32_t)commandline_inode > fs->super->s_inodes_count)
		{
			std::cout << std::flush;
			std::cerr << progname << ": --inode: inode " << commandline_inode 
			<< " is out of range. There are only " << fs->super->s_inodes_count
			<< " inodes." << std::endl;
			return EU_EXAMINE_FAIL;
		}
	}
	if (commandline_block != 0)
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
	}

	if (commandline_show_journal_inodes != 0)
	{
		if ((uint32_t)commandline_show_journal_inodes > fs->super->s_inodes_count)
		{
			std::cout << std::flush;
			std::cerr << progname << ": --show-journal-inodes: inode "
			<< commandline_show_journal_inodes
			<< " is out of range. There are only " << fs->super->s_inodes_count
			<< " inodes." << std::endl;
			return EU_EXAMINE_FAIL;
		}
	}

	// Handle --inode
	if (commandline_inode != 0)
	{
		std::cout << "Group: " << ext2fs_group_of_ino(fs, commandline_inode)
		<< std::endl;
		print_inode(fs, commandline_inode);
	}

	// Handle --block
	if (commandline_block != 0 || (commandline_journal_block != 0 && commandline_journal))
	{
		classify_block(fs, commandline_block);
	}

	ext2_filsys jfs = NULL;
	errcode = get_journal_fs(fs, &jfs, commandline_journal_filename);
	if (errcode) {
		std::cout << "Error opening journal." << std::endl;
		return errcode;
	}

	journal_superblock_t jsb;
	journal_superblock_t *journal_superblock = &jsb;
	errcode = read_journal_superblock(fs, jfs, journal_superblock);
	if (errcode) {
		std::cout << "Error reading journal superblock." << std::endl;
		return errcode;
	}

	if (commandline_superblock && commandline_journal)
	{
		std::cout << jsb << std::endl;
	}

	if (commandline_journal_block != 0)
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

		errcode = extundelete_make_outputdir("RECOVERED_FILES/", progname.c_str());
		if (errcode) return EU_EXAMINE_FAIL;
	}
	// Handle --restore-all
	if (commandline_restore_all)
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
  if (commandline_show_journal_inodes != 0)
    show_journal_inodes(commandline_show_journal_inodes);

//*/
	if(fs->super->s_journal_inum == 0)
		ext2fs_close(jfs);
	return 0;
}

//FIXME: Some of the string conversions are to long values that get stored as ints.
static int decode_options(int& argc, char**& argv)
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
		opt_help,
		opt_log
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
		{"log", 1, &long_option, opt_log},
		{NULL, 0, NULL, 0}
	};

	std::string hist_arg;

	while ((short_option = getopt_long(argc, argv, "j:vVb:B:", longopts, NULL)) != -1)
	{
		switch (short_option)
		{
		case 0:
			switch (long_option)
			{
			case opt_help:
				print_usage(std::cout, progname);
				return EU_STOP;
			case opt_version:
				print_version();
				return EU_STOP;
			case opt_superblock:
				commandline_superblock = true;
				break;
			case opt_journal:
				commandline_journal = true;
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
			case opt_inode_to_block:
				errno = 0;
				commandline_inode_to_block = strtoul(optarg, NULL, 10);
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
				commandline_inode = strtoul(optarg, NULL, 10);
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
				break;
			case opt_block:
				errno = 0;
				commandline_block = strtoul(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --block " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				if (commandline_block < 1)
				{
					std::cout << std::flush;
					std::cerr << progname << ": --block: block " << commandline_block
					<< " is out of range." << std::endl;
					return EU_DECODE_FAIL;
				}
				break;
			case opt_show_journal_inodes:
				errno = 0;
				commandline_show_journal_inodes = strtoul(optarg, NULL, 10);
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
				break;
			case opt_journal_transaction:
				errno = 0;
				commandline_journal_transaction = strtoul(optarg, NULL, 10);
				if(errno) {
					std::cerr << "Invalid parameter: --journal-transaction " << optarg << std::endl;
					return EU_DECODE_FAIL;
				}
				break;
			case opt_histogram:
				commandline_histogram = optarg;
				break;
			case opt_log:
				std::string logopts = optarg;
				while(true) {
					if( ! logopts.substr(0,5).compare("debug") ) {
						if( logopts[5] == '=' ) {
							size_t pos = logopts.find_first_of(',');
							std::string fname(logopts.substr(6, pos-6));
							if( fname.compare("0") ) {
								Log::dfile.open(fname);
								Log::debug.rdbuf(Log::dfile.rdbuf());
							} else {
								Log::debug.rdbuf(0);
							}
							logopts.erase(0, pos);
							//FIXME: close the file when the program ends
						} else {
							Log::debug.rdbuf(std::cout.rdbuf());
							logopts.erase(0,5);
						}
					} else if( ! logopts.substr(0,4).compare("info") ) {
						if( logopts[4] == '=' ) {
							size_t pos = logopts.find_first_of(',');
							std::string fname(logopts.substr(5, pos-5));
							if( fname.compare("0") ) {
								Log::ifile.open(fname);
								Log::info.rdbuf(Log::ifile.rdbuf());
							} else {
								Log::info.rdbuf(0);
							}
							logopts.erase(0, pos);
							//FIXME: close the file when the program ends
						} else {
							Log::info.rdbuf(std::cout.rdbuf());
							logopts.erase(0,4);
						}
					} else if( ! logopts.substr(0,4).compare("warn") ) {
						if( logopts[4] == '=' ) {
							size_t pos = logopts.find_first_of(',');
							std::string fname(logopts.substr(5, pos-5));
							if( fname.compare("0") ) {
								Log::wfile.open(fname);
								Log::warn.rdbuf(Log::wfile.rdbuf());
							} else {
								Log::warn.rdbuf(0);
							}
							logopts.erase(0, pos);
							//FIXME: close the file when the program ends
						} else {
							Log::warn.rdbuf(std::cout.rdbuf());
							logopts.erase(0,4);
						}
					} else if( ! logopts.substr(0,5).compare("error") ) {
						if( logopts[5] == '=' ) {
							size_t pos = logopts.find_first_of(',');
							std::string fname(logopts.substr(6, pos-6));
							if( fname.compare("0") ) {
								Log::efile.open(fname);
								Log::error.rdbuf(Log::efile.rdbuf());
							} else {
								Log::error.rdbuf(0);
							}
							logopts.erase(0, pos);
							//FIXME: close the file when the program ends
						} else {
							Log::error.rdbuf(std::cout.rdbuf());
							logopts.erase(0,5);
						}

					} else {
						if( logopts.compare("0") ) {
							Log::efile.open(logopts);
							Log::info.rdbuf(Log::efile.rdbuf());
							Log::warn.rdbuf(Log::efile.rdbuf());
							Log::error.rdbuf(Log::efile.rdbuf());
							//FIXME: close the file when the program ends
						} else {
							Log::info.rdbuf(0);
							Log::warn.rdbuf(0);
							Log::error.rdbuf(0);
						}
						logopts.clear();
					}
					if(logopts.empty()) break;
					logopts.erase(0,1);
				}
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

	commandline_action =
			(commandline_inode != 0 ||
			 commandline_block != 0 ||
			 commandline_journal_block != 0 ||
			 commandline_journal_transaction != 0 ||
			 commandline_show_journal_inodes != 0 ||
			 !commandline_histogram.empty() ||
			 commandline_inode_to_block != 0 ||
			 !commandline_restore_inode.empty() ||
			 !commandline_restore_file.empty() ||
			 !commandline_restore_files.empty() ||
			 !commandline_restore_directory.empty() ||
			 commandline_restore_all);
	if (!commandline_action && !commandline_superblock)
	{
		std::cout << "No action specified; implying --superblock.\n";
		commandline_superblock = true;
	}
	if (commandline_before < LONG_MAX || commandline_after)
	{
		std::cout << "Only show and process deleted entries if they are deleted ";
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

	argv += optind;
	argc -= optind;

	// Sanity checks on the user.
	if (argc == 0)
	{
		std::cerr << progname << ": Missing device name." << std::endl;
		print_usage(std::cerr, progname);
		return EU_DECODE_FAIL;
	}
	if (argc > 1) {
		std::cerr << progname << ": Some unrecognized options were found. ";
		std::cerr << "Use --help for a usage message." << std::endl;
		return EU_DECODE_FAIL;
	}

	return 0;
}

static errcode_t init_fs(const char* fsname, ext2_filsys *ret_fs) {
	struct stat statbuf;
	int error = 0;
	errcode_t errcode;
	io_manager io_mgr = unix_io_manager;

	// Ensure the file is a filesystem.
	errno = 0;
	if (stat (fsname, &statbuf) == -1) {
		error = errno;
		if (error != EOVERFLOW) {
			std::cout << std::flush;
			std::cerr << progname << ": stat \"" << fsname << "\": "
			<< strerror (error) << std::endl;
			return EU_FS_ERR;
		}
	}
	if (error == 0) {
		if (S_ISDIR (statbuf.st_mode))
		{
			std::cerr << progname << ": \"" << fsname << "\" is a directory. You need "
			<< "to use the raw filesystem device (or a copy thereof)." << std::endl;
			return EU_FS_ERR;
		}
		if (!S_ISBLK(statbuf.st_mode) && statbuf.st_size < 2048)
		{
			std::cerr << progname << ": \"" << fsname << "\" is too small to be a"
			<< " filesystem (" << statbuf.st_size << " bytes)." << std::endl;
			return EU_FS_ERR;
		}
	}

	// Open the filesystem.
	errcode = ext2fs_open (fsname, EXT2_FLAG_64BITS, commandline_backup_superblock,
		commandline_block_size, io_mgr, ret_fs);
	return errcode;
}

// Main program implementation
int main(int argc, char* argv[])
{
	ext2_filsys fs;
	errcode_t errcode;

	progname = argv[0];

	errcode = decode_options(argc, argv);
	if (errcode) {
		if (errcode == EU_STOP) return 0;
		std::cerr << "Error parsing command-line options." << std::endl;
		return EXIT_FAILURE;
	}

	errcode = init_fs(*argv, &fs);
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
