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
#define VERSION "0.1.9";
#endif
extern std::string progname;
extern std::string outputdir;

// The superblock.
extern ext2_super_block super_block;
// Frequently used constant values from the superblock.
extern uint32_t groups_;
extern uint32_t block_size_;
extern uint32_t block_size_log_;
extern uint32_t inodes_per_group_;
extern uint16_t inode_size_;
extern uint32_t inode_count_;
extern uint32_t block_count_;
extern ext2_group_desc* group_descriptor_table;

// Information from journal
typedef std::vector<blk_t>  block_list_t;
extern std::vector<uint32_t> tag_seq;
extern block_list_t tag_jblk;
extern block_list_t tag_fsblk;
extern block_list_t rvk_block;
/*
 * journ_map is meant to contain the
 * (file system block number, (journal block number, sequence number)),
 * in that order, for each descriptor in the journal.
 * block_pair_t is (journal block number, sequence number).
*/
typedef std::pair<blk_t, uint32_t>   block_pair_t;
typedef std::pair<blk_t, block_pair_t>  journal_map_item;
typedef std::multimap<blk_t, block_pair_t>  journal_map_t;
extern journal_map_t journ_map;

// Commandline options.
extern bool commandline_superblock;
extern dgrp_t commandline_group;
extern int commandline_inode_to_block;
extern int commandline_inode;
extern __s64 commandline_block;
extern __s64 commandline_journal_block;
extern int commandline_journal_transaction;
extern bool commandline_journal;
extern bool commandline_dump_names;
extern bool commandline_directory;
extern long commandline_before;
extern long commandline_after;
extern bool commandline_action;
extern std::string commandline_histogram;
extern int commandline_show_journal_inodes;
extern std::string commandline_restore_file;
extern std::string commandline_restore_files;
extern std::string commandline_restore_directory;
extern std::string commandline_restore_inode;
extern bool commandline_restore_all;
extern bool commandline_show_hardlinks;
extern std::string commandline_journal_filename;
extern blk_t commandline_backup_superblock;
extern blk_t commandline_block_size;

#endif //EXTUNDELETEPRIV_H
