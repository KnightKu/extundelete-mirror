/*
 * extundelelete -- An ext3 and ext4 file system undelete tool
 * Main header file for extundelelete
*/

#ifndef EXTUNDELETE_H
#define EXTUNDELETE_H
#include <iostream>
#include <iomanip>
#include <list>
#include <sstream>
#include <vector>

#include <sys/types.h>
#include <ext2fs/ext2fs.h>
#include "kernel-jbd.h"

#define SEARCH_JOURNAL 1

#define EU_RESTORE_FAIL 1
#define EU_DECODE_FAIL  1
#define EU_STOP         2
#define EU_FS_ERR     1
#define EU_FS_RECOVER 2
#define EU_EXAMINE_FAIL 1
// Global enumerations and structs

/*
enum hist_type {
  hist_none = 0,	// No histogram.
  hist_atime,		// Request histogram of access times.
  hist_ctime,		// Request histogram of file modification times.
  hist_mtime,		// Request histogram of inode modification times.
  hist_dtime,		// Request histogram of deletion times.
  hist_group		// Request histogram of deletions per group.
};
*/

// Converts input to a hexadecimal number. Returns a string to be
// sent to std::cout. Width is the minimum number of digits to return.
template<typename T>
std::string tohex(T os, int width)
{
  std::ostringstream oss;
  oss << std::hex << std::setfill('0') << std::setw(width);
  oss << os << std::dec;
  return oss.str();
}

template <class T>
inline std::string to_string (const T& t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
}


struct match_struct
{
  ext2_ino_t *ret_ino;
  std::string curr_name;
};

struct filebuf
{
  std::fstream *file;
  char *buf;
};


// Function declarations
// Helper function declarations
void print_usage(std::ostream& os);
void print_version(void);
void journal_header_to_cpu(char *);

// Main implementation function declarations
int decode_options(int& argc, char**& argv);
int examine_fs(ext2_filsys fs);
int load_super_block(ext2_filsys fs);
int init_journal(ext2_filsys fs, ext2_filsys jfs, journal_superblock_t *jsb);
int restore_file(ext2_filsys fs, ext2_filsys jfs, const std::string& fname);
int restore_inode(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t ino, const std::string& dname);
void parse_inode_block(struct ext2_inode *inode, const char *buf, ext2_ino_t ino);
errcode_t recover_inode(ext2_filsys fs, ext2_filsys jfs, ext2_ino_t ino,
		struct ext2_inode *&inode, int ver);
int pair_names_with(ext2_filsys fs, ext2_filsys jfs, std::vector<ext2_ino_t>& inolist,
		ext2_ino_t ino, std::string dirname, int del, std::vector<ext2_ino_t>& parent_inos);
int read_journal_block(ext2_filsys fs, blk_t n, char *buf);

// From insertionops.cc
std::ostream& operator<<(std::ostream& os, const ext2_super_block* const s_block);
std::ostream& operator<<(std::ostream& os, journal_header_t const& journal_header);
std::ostream& operator<<(std::ostream& os, const ext2_inode& inode);
std::ostream& operator<<(std::ostream& os, const ext2_group_desc& group_desc);
std::ostream& operator<<(std::ostream& os, const journal_revoke_header_t journal_revoke_header);
std::ostream& operator<<(std::ostream& os, journal_superblock_t const& journal_super_block);


#endif //EXTUNDELETE_H
