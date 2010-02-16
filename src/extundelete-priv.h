#ifndef EXTUNDELETEPRIV_H
#define EXTUNDELETEPRIV_H
#include <climits>
#include <cstring>
#include <iomanip>
#include <list>
#include <map>
#include <stdint.h>
#include <utility>

#include <sys/types.h>
#include <ext2fs/ext2fs.h>

// Global variables
#ifndef VERSION
std::string VERSION="0.1.9";
#endif
std::string progname;
std::string outputdir = "RECOVERED_FILES/";

// The superblock.
ext2_super_block super_block;
// Frequently used constant values from the superblock.
uint32_t groups_;
uint32_t block_size_;
uint32_t block_size_log_;
uint32_t inodes_per_group_;
uint16_t inode_size_;
uint32_t inode_count_;
uint32_t block_count_;
ext2_group_desc* group_descriptor_table;

// Information from journal
typedef std::vector<blk_t>  block_list_t;
std::vector<uint32_t> tag_seq;
block_list_t tag_jblk;
block_list_t tag_fsblk;
block_list_t rvk_block;
/*
 * journ_map is meant to contain the
 * (file system block number, (journal block number, sequence number)),
 * in that order, for each descriptor in the journal.
 * block_pair_t is (journal block number, sequence number).
*/
typedef std::pair<blk_t, uint32_t>   block_pair_t;
typedef std::pair<blk_t, block_pair_t>  journal_map_item;
typedef std::multimap<blk_t, block_pair_t>  journal_map_t;
journal_map_t journ_map;

// Commandline options.
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

#endif //EXTUNDELETEPRIV_H
