/*
 * This file was modified from e2fsprogs 1.41.4
 * Use this file when compiling against a newer version of ext2fs headers.
 * block.c --- iterate over all blocks in an inode
 * extent.c --- work with extents
 *
 * Copyright (C) 1993, 1994, 1995, 1996 Theodore Ts'o.
 *
 * %Begin-Header%
 * This file may be redistributed under the terms of the GNU Public
 * License.
 * %End-Header%
 */

/*
 This is a workaround to allow compilation, but the one line that uses
 this constant will never run because we open the fs read-only.
*/ 
#define EXT4INO  0
#include "config.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#ifndef EXT2_FLAT_INCLUDES
#define EXT2_FLAT_INCLUDES 0
#endif

#include <ext2fs/ext2fs.h>
#include "block.h"

struct block_context {
	ext2_filsys	fs;
	int (*func)(ext2_filsys	fs,
		    blk_t	*blocknr,
		    e2_blkcnt_t	bcount,
		    blk_t	ref_blk,
		    int		ref_offset,
		    void	*priv_data);
	e2_blkcnt_t	bcount;
	int		bsize;
	int		flags;
	errcode_t	errcode;
	char	*ind_buf;
	char	*dind_buf;
	char	*tind_buf;
	void	*priv_data;
};


#ifdef BLOCK_FLAG_READ_ONLY

#include <ext2fs/ext3_extents.h>
struct extent_path {
        char            *buf;
        int             entries;
        int             max_entries;
        int             left;
        int             visit_num;
        int             flags;
        blk64_t         end_blk;
        void            *curr;
};


struct ext2_extent_handle {
        errcode_t               magic;
        ext2_filsys             fs;
        ext2_ino_t              ino;
        struct ext2_inode       *inode;
        int                     type;
        int                     level;
        int                     max_depth;
        struct extent_path      *path;
};

/* This function is a local copy of ext2fs_extent_open2()
 * from e2fsprogs-1.41.10
 */
errcode_t local_ext2fs_extent_open(ext2_filsys fs, ext2_ino_t ino,
                                    struct ext2_inode *inode,
                                    ext2_extent_handle_t *ret_handle)
{
        struct ext2_extent_handle       *handle;
        errcode_t                       retval;
        int                             i;
        struct ext3_extent_header       *eh;

        EXT2_CHECK_MAGIC(fs, EXT2_ET_MAGIC_EXT2FS_FILSYS);

        if (!inode)
                if ((ino == 0) || (ino > fs->super->s_inodes_count))
                        return EXT2_ET_BAD_INODE_NUM;

        retval = ext2fs_get_mem(sizeof(struct ext2_extent_handle), &handle);
        if (retval)
                return retval;
        memset(handle, 0, sizeof(struct ext2_extent_handle));

        retval = ext2fs_get_mem(sizeof(struct ext2_inode), &handle->inode);
        if (retval)
                goto errout;

        handle->ino = ino;
        handle->fs = fs;

        if (inode) {
                memcpy(handle->inode, inode, sizeof(struct ext2_inode));
        }
        else {
                retval = ext2fs_read_inode(fs, ino, handle->inode);
                if (retval)
                        goto errout;
        }
        eh = (struct ext3_extent_header *) &handle->inode->i_block[0];

        for (i=0; i < EXT2_N_BLOCKS; i++)
                if (handle->inode->i_block[i])
                        break;
        if (i >= EXT2_N_BLOCKS) {
                eh->eh_magic = ext2fs_cpu_to_le16(EXT3_EXT_MAGIC);
                eh->eh_depth = 0;
                eh->eh_entries = 0;
                i = (sizeof(handle->inode->i_block) - sizeof(*eh)) /
                        sizeof(struct ext3_extent);
                eh->eh_max = ext2fs_cpu_to_le16(i);
                handle->inode->i_flags |= EXT4_EXTENTS_FL;
        }

        if (!(handle->inode->i_flags & EXT4_EXTENTS_FL)) {
                retval = EXT2_ET_INODE_NOT_EXTENT;
                goto errout;
        }

        retval = ext2fs_extent_header_verify(eh, sizeof(handle->inode->i_block));
        if (retval)
                goto errout;

        handle->max_depth = ext2fs_le16_to_cpu(eh->eh_depth);
        handle->type = ext2fs_le16_to_cpu(eh->eh_magic);

        retval = ext2fs_get_mem(((handle->max_depth+1) *
                                 sizeof(struct extent_path)),
                                &handle->path);
        memset(handle->path, 0,
               (handle->max_depth+1) * sizeof(struct extent_path));
        handle->path[0].buf = (char *) handle->inode->i_block;

        handle->path[0].left = handle->path[0].entries =
                ext2fs_le16_to_cpu(eh->eh_entries);
        handle->path[0].max_entries = ext2fs_le16_to_cpu(eh->eh_max);
        handle->path[0].curr = 0;
        handle->path[0].end_blk =
                ((((__u64) handle->inode->i_size_high << 32) +
                  handle->inode->i_size + (fs->blocksize - 1))
                 >> EXT2_BLOCK_SIZE_BITS(fs->super));
        handle->path[0].visit_num = 1;
        handle->level = 0;
        handle->magic = EXT2_ET_MAGIC_EXTENT_HANDLE;

        *ret_handle = handle;
        return 0;

errout:
        ext2fs_extent_free(handle);
        return retval;
}



#define check_for_ro_violation_return(ctx, ret)				\
	do {								\
		if (((ctx)->flags & BLOCK_FLAG_READ_ONLY) &&		\
		    ((ret) & BLOCK_CHANGED)) {				\
			(ctx)->errcode = EXT2_ET_RO_BLOCK_ITERATE;	\
			ret |= BLOCK_ABORT | BLOCK_ERROR;		\
			return ret;					\
		}							\
	} while (0)

#define check_for_ro_violation_goto(ctx, ret, label)			\
	do {								\
		if (((ctx)->flags & BLOCK_FLAG_READ_ONLY) &&		\
		    ((ret) & BLOCK_CHANGED)) {				\
			(ctx)->errcode = EXT2_ET_RO_BLOCK_ITERATE;	\
			ret |= BLOCK_ABORT | BLOCK_ERROR;		\
			goto label;					\
		}							\
	} while (0)

static int block_iterate_ind(blk_t *ind_block, blk_t ref_block,
			     int ref_offset, struct block_context *ctx)
{
	int	ret = 0, changed = 0;
	int	i, flags, limit, offset;
	blk_t	*block_nr;

	limit = ctx->fs->blocksize >> 2;
	if (!(ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY))
		ret = (*ctx->func)(ctx->fs, ind_block,
				   BLOCK_COUNT_IND, ref_block,
				   ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	if (!*ind_block || (ret & BLOCK_ABORT)) {
		ctx->bcount += limit;
		return ret;
	}
	if (*ind_block >= ctx->fs->super->s_blocks_count ||
	    *ind_block < ctx->fs->super->s_first_data_block) {
		ctx->errcode = EXT2_ET_BAD_IND_BLOCK;
		ret |= BLOCK_ERROR;
		return ret;
	}
	ctx->errcode = ext2fs_read_ind_block(ctx->fs, *ind_block,
					     ctx->ind_buf);
	if (ctx->errcode) {
		ret |= BLOCK_ERROR;
		return ret;
	}

	block_nr = (blk_t *) ctx->ind_buf;
	offset = 0;
	if (ctx->flags & BLOCK_FLAG_APPEND) {
		for (i = 0; i < limit; i++, ctx->bcount++, block_nr++) {
			flags = (*ctx->func)(ctx->fs, block_nr, ctx->bcount,
					     *ind_block, offset,
					     ctx->priv_data);
			changed	|= flags;
			if (flags & BLOCK_ABORT) {
				ret |= BLOCK_ABORT;
				break;
			}
			offset += sizeof(blk_t);
		}
	} else {
		for (i = 0; i < limit; i++, ctx->bcount++, block_nr++) {
			if (*block_nr == 0)
				continue;
			flags = (*ctx->func)(ctx->fs, block_nr, ctx->bcount,
					     *ind_block, offset,
					     ctx->priv_data);
			changed	|= flags;
			if (flags & BLOCK_ABORT) {
				ret |= BLOCK_ABORT;
				break;
			}
			offset += sizeof(blk_t);
		}
	}
	check_for_ro_violation_return(ctx, changed);
	if (changed & BLOCK_CHANGED) {
		ctx->errcode = ext2fs_write_ind_block(ctx->fs, *ind_block,
						      ctx->ind_buf);
		if (ctx->errcode)
			ret |= BLOCK_ERROR | BLOCK_ABORT;
	}
	if ((ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY) &&
	    !(ret & BLOCK_ABORT))
		ret |= (*ctx->func)(ctx->fs, ind_block,
				    BLOCK_COUNT_IND, ref_block,
				    ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	return ret;
}

static int block_iterate_dind(blk_t *dind_block, blk_t ref_block,
			      int ref_offset, struct block_context *ctx)
{
	int	ret = 0, changed = 0;
	int	i, flags, limit, offset;
	blk_t	*block_nr;

	limit = ctx->fs->blocksize >> 2;
	if (!(ctx->flags & (BLOCK_FLAG_DEPTH_TRAVERSE |
			    BLOCK_FLAG_DATA_ONLY)))
		ret = (*ctx->func)(ctx->fs, dind_block,
				   BLOCK_COUNT_DIND, ref_block,
				   ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	if (!*dind_block || (ret & BLOCK_ABORT)) {
		ctx->bcount += limit*limit;
		return ret;
	}
	if (*dind_block >= ctx->fs->super->s_blocks_count ||
	    *dind_block < ctx->fs->super->s_first_data_block) {
		ctx->errcode = EXT2_ET_BAD_DIND_BLOCK;
		ret |= BLOCK_ERROR;
		return ret;
	}
	ctx->errcode = ext2fs_read_ind_block(ctx->fs, *dind_block,
					     ctx->dind_buf);
	if (ctx->errcode) {
		ret |= BLOCK_ERROR;
		return ret;
	}

	block_nr = (blk_t *) ctx->dind_buf;
	offset = 0;
	if (ctx->flags & BLOCK_FLAG_APPEND) {
		for (i = 0; i < limit; i++, block_nr++) {
			flags = block_iterate_ind(block_nr,
						  *dind_block, offset,
						  ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	} else {
		for (i = 0; i < limit; i++, block_nr++) {
			if (*block_nr == 0) {
				ctx->bcount += limit;
				continue;
			}
			flags = block_iterate_ind(block_nr,
						  *dind_block, offset,
						  ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	}
	check_for_ro_violation_return(ctx, changed);
	if (changed & BLOCK_CHANGED) {
		ctx->errcode = ext2fs_write_ind_block(ctx->fs, *dind_block,
						      ctx->dind_buf);
		if (ctx->errcode)
			ret |= BLOCK_ERROR | BLOCK_ABORT;
	}
	if ((ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY) &&
	    !(ret & BLOCK_ABORT))
		ret |= (*ctx->func)(ctx->fs, dind_block,
				    BLOCK_COUNT_DIND, ref_block,
				    ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	return ret;
}

static int block_iterate_tind(blk_t *tind_block, blk_t ref_block,
			      int ref_offset, struct block_context *ctx)
{
	int	ret = 0, changed = 0;
	int	i, flags, limit, offset;
	blk_t	*block_nr;

	limit = ctx->fs->blocksize >> 2;
	if (!(ctx->flags & (BLOCK_FLAG_DEPTH_TRAVERSE |
			    BLOCK_FLAG_DATA_ONLY)))
		ret = (*ctx->func)(ctx->fs, tind_block,
				   BLOCK_COUNT_TIND, ref_block,
				   ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	if (!*tind_block || (ret & BLOCK_ABORT)) {
		ctx->bcount += limit*limit*limit;
		return ret;
	}
	if (*tind_block >= ctx->fs->super->s_blocks_count ||
	    *tind_block < ctx->fs->super->s_first_data_block) {
		ctx->errcode = EXT2_ET_BAD_TIND_BLOCK;
		ret |= BLOCK_ERROR;
		return ret;
	}
	ctx->errcode = ext2fs_read_ind_block(ctx->fs, *tind_block,
					     ctx->tind_buf);
	if (ctx->errcode) {
		ret |= BLOCK_ERROR;
		return ret;
	}

	block_nr = (blk_t *) ctx->tind_buf;
	offset = 0;
	if (ctx->flags & BLOCK_FLAG_APPEND) {
		for (i = 0; i < limit; i++, block_nr++) {
			flags = block_iterate_dind(block_nr,
						   *tind_block,
						   offset, ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	} else {
		for (i = 0; i < limit; i++, block_nr++) {
			if (*block_nr == 0) {
				ctx->bcount += limit*limit;
				continue;
			}
			flags = block_iterate_dind(block_nr,
						   *tind_block,
						   offset, ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	}
	check_for_ro_violation_return(ctx, changed);
	if (changed & BLOCK_CHANGED) {
		ctx->errcode = ext2fs_write_ind_block(ctx->fs, *tind_block,
						      ctx->tind_buf);
		if (ctx->errcode)
			ret |= BLOCK_ERROR | BLOCK_ABORT;
	}
	if ((ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY) &&
	    !(ret & BLOCK_ABORT))
		ret |= (*ctx->func)(ctx->fs, tind_block,
				    BLOCK_COUNT_TIND, ref_block,
				    ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	return ret;
}

errcode_t local_block_iterate3(ext2_filsys fs,
				struct ext2_inode inode, /* ext2_ino_t ino, */
				int	flags,
				char *block_buf,
				int (*func)(ext2_filsys fs,
					    blk_t	*blocknr,
					    e2_blkcnt_t	blockcnt,
					    blk_t	ref_blk,
					    int		ref_offset,
					    void	*priv_data),
				void *priv_data)
{
	int	i;
	int	r, ret = 0;
	/* struct ext2_inode inode; */
	errcode_t	retval;
	struct block_context ctx;
	int	limit;

	EXT2_CHECK_MAGIC(fs, EXT2_ET_MAGIC_EXT2FS_FILSYS);

	/*ctx.errcode = ext2fs_read_inode(fs, ino, &inode);
	if (ctx.errcode)
		return ctx.errcode;
	*/

	/*
	 * Check to see if we need to limit large files
	 */
	if (flags & BLOCK_FLAG_NO_LARGE) {
		if (!LINUX_S_ISDIR(inode.i_mode) &&
		    (inode.i_size_high != 0))
			return EXT2_ET_FILE_TOO_BIG;
	}

	limit = fs->blocksize >> 2;

	ctx.fs = fs;
	ctx.func = func;
	ctx.priv_data = priv_data;
	ctx.flags = flags;
	ctx.bcount = 0;
	if (block_buf) {
		ctx.ind_buf = block_buf;
	} else {
		retval = ext2fs_get_array(3, fs->blocksize, &ctx.ind_buf);
		if (retval)
			return retval;
	}
	ctx.dind_buf = ctx.ind_buf + fs->blocksize;
	ctx.tind_buf = ctx.dind_buf + fs->blocksize;

	/*
	 * Iterate over the HURD translator block (if present)
	 */
	if ((fs->super->s_creator_os == EXT2_OS_HURD) &&
	    !(flags & BLOCK_FLAG_DATA_ONLY)) {
		if (inode.osd1.hurd1.h_i_translator) {
			ret |= (*ctx.func)(fs,
					   &inode.osd1.hurd1.h_i_translator,
					   BLOCK_COUNT_TRANSLATOR,
					   0, 0, priv_data);
			if (ret & BLOCK_ABORT)
				goto abort_exit;
			check_for_ro_violation_goto(&ctx, ret, abort_exit);
		}
	}

	if (inode.i_flags & EXT4_EXTENTS_FL) {
		ext2_extent_handle_t	handle;
		struct ext2fs_extent	extent;
		e2_blkcnt_t		blockcnt = 0;
		blk_t			blk, new_blk;
		int			op = EXT2_EXTENT_ROOT;
		int			uninit;
		unsigned int		j;

		ctx.errcode = local_ext2fs_extent_open(fs, 0, &inode, &handle);
		if (ctx.errcode)
			goto abort_exit;

		while (1) {
			ctx.errcode = ext2fs_extent_get(handle, op, &extent);
			if (ctx.errcode) {
				if (ctx.errcode != EXT2_ET_EXTENT_NO_NEXT)
					break;
				ctx.errcode = 0;
				if (!(flags & BLOCK_FLAG_APPEND))
					break;
				blk = 0;
				r = (*ctx.func)(fs, &blk, blockcnt,
						0, 0, priv_data);
				ret |= r;
				check_for_ro_violation_goto(&ctx, ret,
							    extent_errout);
				if (r & BLOCK_CHANGED) {
					ctx.errcode =
						ext2fs_extent_set_bmap(handle,
						       (blk64_t) blockcnt++,
						       (blk64_t) blk, 0);
					if (ctx.errcode || (ret & BLOCK_ABORT))
						break;
					continue;
				}
				break;
			}

			op = EXT2_EXTENT_NEXT;
			blk = extent.e_pblk;
			if (!(extent.e_flags & EXT2_EXTENT_FLAGS_LEAF)) {
				if (ctx.flags & BLOCK_FLAG_DATA_ONLY)
					continue;
				if ((!(extent.e_flags &
				       EXT2_EXTENT_FLAGS_SECOND_VISIT) &&
				     !(ctx.flags & BLOCK_FLAG_DEPTH_TRAVERSE)) ||
				    ((extent.e_flags &
				      EXT2_EXTENT_FLAGS_SECOND_VISIT) &&
				     (ctx.flags & BLOCK_FLAG_DEPTH_TRAVERSE))) {
					ret |= (*ctx.func)(fs, &blk,
							   -1, 0, 0, priv_data);
					if (ret & BLOCK_CHANGED) {
						extent.e_pblk = blk;
						ctx.errcode =
				ext2fs_extent_replace(handle, 0, &extent);
						if (ctx.errcode)
							break;
					}
				}
				continue;
			}
			uninit = 0;
			if (extent.e_flags & EXT2_EXTENT_FLAGS_UNINIT)
				uninit = EXT2_EXTENT_SET_BMAP_UNINIT;
			for (blockcnt = extent.e_lblk, j = 0;
			     j < extent.e_len;
			     blk++, blockcnt++, j++) {
				new_blk = blk;
				r = (*ctx.func)(fs, &new_blk, blockcnt,
						0, 0, priv_data);
				ret |= r;
				check_for_ro_violation_goto(&ctx, ret,
							    extent_errout);
				if (r & BLOCK_CHANGED) {
					ctx.errcode =
						ext2fs_extent_set_bmap(handle,
						       (blk64_t) blockcnt,
						       (blk64_t) new_blk,
						       uninit);
					if (ctx.errcode)
						goto extent_errout;
				}
				if (ret & BLOCK_ABORT)
					break;
			}
		}

	extent_errout:
		ext2fs_extent_free(handle);
		ret |= BLOCK_ERROR | BLOCK_ABORT;
		goto errout;
	}

	/*
	 * Iterate over normal data blocks
	 */
	for (i = 0; i < EXT2_NDIR_BLOCKS ; i++, ctx.bcount++) {
		if (inode.i_block[i] || (flags & BLOCK_FLAG_APPEND)) {
			ret |= (*ctx.func)(fs, &inode.i_block[i],
					    ctx.bcount, 0, i, priv_data);
			if (ret & BLOCK_ABORT)
				goto abort_exit;
		}
	}
	check_for_ro_violation_goto(&ctx, ret, abort_exit);
	if (inode.i_block[EXT2_IND_BLOCK] || (flags & BLOCK_FLAG_APPEND)) {
		ret |= block_iterate_ind(&inode.i_block[EXT2_IND_BLOCK],
					 0, EXT2_IND_BLOCK, &ctx);
		if (ret & BLOCK_ABORT)
			goto abort_exit;
	} else
		ctx.bcount += limit;
	if (inode.i_block[EXT2_DIND_BLOCK] || (flags & BLOCK_FLAG_APPEND)) {
		ret |= block_iterate_dind(&inode.i_block[EXT2_DIND_BLOCK],
					  0, EXT2_DIND_BLOCK, &ctx);
		if (ret & BLOCK_ABORT)
			goto abort_exit;
	} else
		ctx.bcount += limit * limit;
	if (inode.i_block[EXT2_TIND_BLOCK] || (flags & BLOCK_FLAG_APPEND)) {
		ret |= block_iterate_tind(&inode.i_block[EXT2_TIND_BLOCK],
					  0, EXT2_TIND_BLOCK, &ctx);
		if (ret & BLOCK_ABORT)
			goto abort_exit;
	}

abort_exit:
	if (ret & BLOCK_CHANGED) {
		retval = ext2fs_write_inode(fs, EXT4INO, &inode);
		if (retval) {
			if (!block_buf)
				ext2fs_free_mem(&ctx.ind_buf);
			return retval;
		}
	}
errout:
	if (!block_buf)
		ext2fs_free_mem(&ctx.ind_buf);

	return (ret & BLOCK_ERROR) ? ctx.errcode : 0;
}

/*
 * Emulate the old ext2fs_block_iterate function!
 */

struct xlate {
	int (*func)(ext2_filsys	fs,
		    blk_t	*blocknr,
		    int		bcount,
		    void	*priv_data);
	void *real_private;
};

#ifdef __TURBOC__
 #pragma argsused
#endif
static int xlate_func(ext2_filsys fs, blk_t *blocknr, e2_blkcnt_t blockcnt,
		      blk_t ref_block EXT2FS_ATTR((unused)),
		      int ref_offset EXT2FS_ATTR((unused)),
		      void *priv_data)
{
	struct xlate *xl = (struct xlate *) priv_data;

	return (*xl->func)(fs, blocknr, (int) blockcnt, xl->real_private);
}

errcode_t ext2fs_block_iterate(ext2_filsys fs,
			       ext2_ino_t ino,
			       int	flags,
			       char *block_buf,
			       int (*func)(ext2_filsys fs,
					   blk_t	*blocknr,
					   int	blockcnt,
					   void	*priv_data),
			       void *priv_data)
{
	struct xlate xl;

	xl.real_private = priv_data;
	xl.func = func;

	return ext2fs_block_iterate2(fs, ino, BLOCK_FLAG_NO_LARGE | flags,
				     block_buf, xlate_func, &xl);
}

#else

#define check_for_ro_violation_return(ctx, ret)				\
	do {								\
		if ( ((ret) & BLOCK_CHANGED) ) {				\
			(ctx)->errcode = 9;	\
			ret |= BLOCK_ABORT | BLOCK_ERROR;		\
			return ret;					\
		}							\
	} while (0)

#define check_for_ro_violation_goto(ctx, ret, label)			\
	do {								\
		if ( ((ret) & BLOCK_CHANGED) ) {				\
			(ctx)->errcode = 9;	\
			ret |= BLOCK_ABORT | BLOCK_ERROR;		\
			goto label;					\
		}							\
	} while (0)

static int block_iterate_ind(blk_t *ind_block, blk_t ref_block,
			     int ref_offset, struct block_context *ctx)
{
	int	ret = 0, changed = 0;
	int	i, flags, limit, offset;
	blk_t	*block_nr;

	limit = ctx->fs->blocksize >> 2;
	if (!(ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY))
		ret = (*ctx->func)(ctx->fs, ind_block,
				   BLOCK_COUNT_IND, ref_block,
				   ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	if (!*ind_block || (ret & BLOCK_ABORT)) {
		ctx->bcount += limit;
		return ret;
	}
	if (*ind_block >= ctx->fs->super->s_blocks_count ||
	    *ind_block < ctx->fs->super->s_first_data_block) {
		ctx->errcode = EXT2_ET_BAD_IND_BLOCK;
		ret |= BLOCK_ERROR;
		return ret;
	}
	ctx->errcode = ext2fs_read_ind_block(ctx->fs, *ind_block,
					     ctx->ind_buf);
	if (ctx->errcode) {
		ret |= BLOCK_ERROR;
		return ret;
	}

	block_nr = (blk_t *) ctx->ind_buf;
	offset = 0;
	if (ctx->flags & BLOCK_FLAG_APPEND) {
		for (i = 0; i < limit; i++, ctx->bcount++, block_nr++) {
			flags = (*ctx->func)(ctx->fs, block_nr, ctx->bcount,
					     *ind_block, offset,
					     ctx->priv_data);
			changed	|= flags;
			if (flags & BLOCK_ABORT) {
				ret |= BLOCK_ABORT;
				break;
			}
			offset += sizeof(blk_t);
		}
	} else {
		for (i = 0; i < limit; i++, ctx->bcount++, block_nr++) {
			if (*block_nr == 0)
				continue;
			flags = (*ctx->func)(ctx->fs, block_nr, ctx->bcount,
					     *ind_block, offset,
					     ctx->priv_data);
			changed	|= flags;
			if (flags & BLOCK_ABORT) {
				ret |= BLOCK_ABORT;
				break;
			}
			offset += sizeof(blk_t);
		}
	}
	check_for_ro_violation_return(ctx, changed);
	if (changed & BLOCK_CHANGED) {
		ctx->errcode = ext2fs_write_ind_block(ctx->fs, *ind_block,
						      ctx->ind_buf);
		if (ctx->errcode)
			ret |= BLOCK_ERROR | BLOCK_ABORT;
	}
	if ((ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY) &&
	    !(ret & BLOCK_ABORT))
		ret |= (*ctx->func)(ctx->fs, ind_block,
				    BLOCK_COUNT_IND, ref_block,
				    ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	return ret;
}

static int block_iterate_dind(blk_t *dind_block, blk_t ref_block,
			      int ref_offset, struct block_context *ctx)
{
	int	ret = 0, changed = 0;
	int	i, flags, limit, offset;
	blk_t	*block_nr;

	limit = ctx->fs->blocksize >> 2;
	if (!(ctx->flags & (BLOCK_FLAG_DEPTH_TRAVERSE |
			    BLOCK_FLAG_DATA_ONLY)))
		ret = (*ctx->func)(ctx->fs, dind_block,
				   BLOCK_COUNT_DIND, ref_block,
				   ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	if (!*dind_block || (ret & BLOCK_ABORT)) {
		ctx->bcount += limit*limit;
		return ret;
	}
	if (*dind_block >= ctx->fs->super->s_blocks_count ||
	    *dind_block < ctx->fs->super->s_first_data_block) {
		ctx->errcode = EXT2_ET_BAD_DIND_BLOCK;
		ret |= BLOCK_ERROR;
		return ret;
	}
	ctx->errcode = ext2fs_read_ind_block(ctx->fs, *dind_block,
					     ctx->dind_buf);
	if (ctx->errcode) {
		ret |= BLOCK_ERROR;
		return ret;
	}

	block_nr = (blk_t *) ctx->dind_buf;
	offset = 0;
	if (ctx->flags & BLOCK_FLAG_APPEND) {
		for (i = 0; i < limit; i++, block_nr++) {
			flags = block_iterate_ind(block_nr,
						  *dind_block, offset,
						  ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	} else {
		for (i = 0; i < limit; i++, block_nr++) {
			if (*block_nr == 0) {
				ctx->bcount += limit;
				continue;
			}
			flags = block_iterate_ind(block_nr,
						  *dind_block, offset,
						  ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	}
	check_for_ro_violation_return(ctx, changed);
	if (changed & BLOCK_CHANGED) {
		ctx->errcode = ext2fs_write_ind_block(ctx->fs, *dind_block,
						      ctx->dind_buf);
		if (ctx->errcode)
			ret |= BLOCK_ERROR | BLOCK_ABORT;
	}
	if ((ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY) &&
	    !(ret & BLOCK_ABORT))
		ret |= (*ctx->func)(ctx->fs, dind_block,
				    BLOCK_COUNT_DIND, ref_block,
				    ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	return ret;
}

static int block_iterate_tind(blk_t *tind_block, blk_t ref_block,
			      int ref_offset, struct block_context *ctx)
{
	int	ret = 0, changed = 0;
	int	i, flags, limit, offset;
	blk_t	*block_nr;

	limit = ctx->fs->blocksize >> 2;
	if (!(ctx->flags & (BLOCK_FLAG_DEPTH_TRAVERSE |
			    BLOCK_FLAG_DATA_ONLY)))
		ret = (*ctx->func)(ctx->fs, tind_block,
				   BLOCK_COUNT_TIND, ref_block,
				   ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	if (!*tind_block || (ret & BLOCK_ABORT)) {
		ctx->bcount += limit*limit*limit;
		return ret;
	}
	if (*tind_block >= ctx->fs->super->s_blocks_count ||
	    *tind_block < ctx->fs->super->s_first_data_block) {
		ctx->errcode = EXT2_ET_BAD_TIND_BLOCK;
		ret |= BLOCK_ERROR;
		return ret;
	}
	ctx->errcode = ext2fs_read_ind_block(ctx->fs, *tind_block,
					     ctx->tind_buf);
	if (ctx->errcode) {
		ret |= BLOCK_ERROR;
		return ret;
	}

	block_nr = (blk_t *) ctx->tind_buf;
	offset = 0;
	if (ctx->flags & BLOCK_FLAG_APPEND) {
		for (i = 0; i < limit; i++, block_nr++) {
			flags = block_iterate_dind(block_nr,
						   *tind_block,
						   offset, ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	} else {
		for (i = 0; i < limit; i++, block_nr++) {
			if (*block_nr == 0) {
				ctx->bcount += limit*limit;
				continue;
			}
			flags = block_iterate_dind(block_nr,
						   *tind_block,
						   offset, ctx);
			changed |= flags;
			if (flags & (BLOCK_ABORT | BLOCK_ERROR)) {
				ret |= flags & (BLOCK_ABORT | BLOCK_ERROR);
				break;
			}
			offset += sizeof(blk_t);
		}
	}
	check_for_ro_violation_return(ctx, changed);
	if (changed & BLOCK_CHANGED) {
		ctx->errcode = ext2fs_write_ind_block(ctx->fs, *tind_block,
						      ctx->tind_buf);
		if (ctx->errcode)
			ret |= BLOCK_ERROR | BLOCK_ABORT;
	}
	if ((ctx->flags & BLOCK_FLAG_DEPTH_TRAVERSE) &&
	    !(ctx->flags & BLOCK_FLAG_DATA_ONLY) &&
	    !(ret & BLOCK_ABORT))
		ret |= (*ctx->func)(ctx->fs, tind_block,
				    BLOCK_COUNT_TIND, ref_block,
				    ref_offset, ctx->priv_data);
	check_for_ro_violation_return(ctx, ret);
	return ret;
}

errcode_t local_block_iterate3(ext2_filsys fs,
				struct ext2_inode inode, /*ext2_ino_t ino,*/
				int	flags,
				char *block_buf,
				int (*func)(ext2_filsys fs,
					    blk_t	*blocknr,
					    e2_blkcnt_t	blockcnt,
					    blk_t	ref_blk,
					    int		ref_offset,
					    void	*priv_data),
				void *priv_data)
{
	int	i;
	int	/*r,*/ ret = 0;
	/*struct ext2_inode inode;*/
    ext2_ino_t ino = 0;
	errcode_t	retval;
	struct block_context ctx;
	int	limit;

	EXT2_CHECK_MAGIC(fs, EXT2_ET_MAGIC_EXT2FS_FILSYS);

	/* ctx.errcode = ext2fs_read_inode(fs, ino, &inode);
	if (ctx.errcode)
		return ctx.errcode;
    */
	/*
	 * Check to see if we need to limit large files
	 */
	if (flags & BLOCK_FLAG_NO_LARGE) {
		if (!LINUX_S_ISDIR(inode.i_mode) &&
		    (inode.i_size_high != 0))
			return EXT2_ET_FILE_TOO_BIG;
	}

	limit = fs->blocksize >> 2;

	ctx.fs = fs;
	ctx.func = func;
	ctx.priv_data = priv_data;
	ctx.flags = flags;
	ctx.bcount = 0;
	if (block_buf) {
		ctx.ind_buf = block_buf;
	} else {
		retval = ext2fs_get_array(3, fs->blocksize, &ctx.ind_buf);
		if (retval)
			return retval;
	}
	ctx.dind_buf = ctx.ind_buf + fs->blocksize;
	ctx.tind_buf = ctx.dind_buf + fs->blocksize;

	/*
	 * Iterate over the HURD translator block (if present)
	 */
	if ((fs->super->s_creator_os == EXT2_OS_HURD) &&
	    !(flags & BLOCK_FLAG_DATA_ONLY)) {
		if (inode.osd1.hurd1.h_i_translator) {
			ret |= (*ctx.func)(fs,
					   &inode.osd1.hurd1.h_i_translator,
					   BLOCK_COUNT_TRANSLATOR,
					   0, 0, priv_data);
			if (ret & BLOCK_ABORT)
				goto abort_exit;
			check_for_ro_violation_goto(&ctx, ret, abort_exit);
		}
	}


	/*
	 * Iterate over normal data blocks
	 */
	for (i = 0; i < EXT2_NDIR_BLOCKS ; i++, ctx.bcount++) {
		if (inode.i_block[i] || (flags & BLOCK_FLAG_APPEND)) {
			ret |= (*ctx.func)(fs, &inode.i_block[i],
					    ctx.bcount, 0, i, priv_data);
			if (ret & BLOCK_ABORT)
				goto abort_exit;
		}
	}
	check_for_ro_violation_goto(&ctx, ret, abort_exit);
	if (inode.i_block[EXT2_IND_BLOCK] || (flags & BLOCK_FLAG_APPEND)) {
		ret |= block_iterate_ind(&inode.i_block[EXT2_IND_BLOCK],
					 0, EXT2_IND_BLOCK, &ctx);
		if (ret & BLOCK_ABORT)
			goto abort_exit;
	} else
		ctx.bcount += limit;
	if (inode.i_block[EXT2_DIND_BLOCK] || (flags & BLOCK_FLAG_APPEND)) {
		ret |= block_iterate_dind(&inode.i_block[EXT2_DIND_BLOCK],
					  0, EXT2_DIND_BLOCK, &ctx);
		if (ret & BLOCK_ABORT)
			goto abort_exit;
	} else
		ctx.bcount += limit * limit;
	if (inode.i_block[EXT2_TIND_BLOCK] || (flags & BLOCK_FLAG_APPEND)) {
		ret |= block_iterate_tind(&inode.i_block[EXT2_TIND_BLOCK],
					  0, EXT2_TIND_BLOCK, &ctx);
		if (ret & BLOCK_ABORT)
			goto abort_exit;
	}

abort_exit:
	if (ret & BLOCK_CHANGED) {
		retval = ext2fs_write_inode(fs, ino, &inode);
		if (retval) {
			if (!block_buf)
				ext2fs_free_mem(&ctx.ind_buf);
			return retval;
		}
	}
/*errout:*/
	if (!block_buf)
		ext2fs_free_mem(&ctx.ind_buf);

	return (ret & BLOCK_ERROR) ? ctx.errcode : 0;
}

/*
 * Emulate the old ext2fs_block_iterate function!
 */

struct xlate {
	int (*func)(ext2_filsys	fs,
		    blk_t	*blocknr,
		    int		bcount,
		    void	*priv_data);
	void *real_private;
};

#ifdef __TURBOC__
 #pragma argsused
#endif
static int xlate_func(ext2_filsys fs, blk_t *blocknr, e2_blkcnt_t blockcnt,
		      blk_t ref_block EXT2FS_ATTR((unused)),
		      int ref_offset EXT2FS_ATTR((unused)),
		      void *priv_data)
{
	struct xlate *xl = (struct xlate *) priv_data;

	return (*xl->func)(fs, blocknr, (int) blockcnt, xl->real_private);
}

errcode_t ext2fs_block_iterate(ext2_filsys fs,
			       ext2_ino_t ino,
			       int	flags,
			       char *block_buf,
			       int (*func)(ext2_filsys fs,
					   blk_t	*blocknr,
					   int	blockcnt,
					   void	*priv_data),
			       void *priv_data)
{
	struct xlate xl;

	xl.real_private = priv_data;
	xl.func = func;

	return ext2fs_block_iterate2(fs, ino, BLOCK_FLAG_NO_LARGE | flags,
				     block_buf, xlate_func, &xl);
}



#endif /* Choose between old and new ext2fs libs */

/*
 * This function checks to see whether or not a potential deleted
 * directory entry looks valid.  What we do is check the deleted entry
 * and each successive entry to make sure that they all look valid and
 * that the last deleted entry ends at the beginning of the next
 * undeleted entry.  Returns 1 if the deleted entry looks valid, zero
 * if not valid.
 */
static int extundelete_validate_entry(ext2_filsys fs, char *buf,
				 unsigned int offset,
				 unsigned int final_offset)
{
	struct ext2_dir_entry *dirent;
	int	rec_len;
	int dirent_min_len = 12;

	while (offset < final_offset && offset <= fs->blocksize - dirent_min_len) {
		dirent = (struct ext2_dir_entry *)(buf + offset);
		rec_len = (dirent->rec_len || fs->blocksize < 65536) ?
			dirent->rec_len : 65536;
		offset += rec_len;
		if ((rec_len < 8) ||
		    ((rec_len % 4) != 0) ||
		    (((dirent->name_len & 0xFF)+8) > rec_len))
			return 0;
	}
	return (offset == final_offset);
}

int extundelete_process_dir_block(ext2_filsys fs,
			     blk_t	*blocknr,
			     e2_blkcnt_t blockcnt,
			     blk_t	ref_block EXT2FS_ATTR((unused)),
			     int	ref_offset EXT2FS_ATTR((unused)),
			     void	*priv_data)
{
	struct dir_context *ctx = (struct dir_context *) priv_data;
	unsigned int	offset = 0;
	unsigned int	next_real_entry = 0;
	int		ret = 0;
	int		changed = 0;
	int		do_abort = 0;
	int		rec_len, entry, size;
	struct ext2_dir_entry *dirent;
	int dirent_min_len = 12;

	if (blockcnt < 0)
		return 0;

	entry = blockcnt ? DIRENT_OTHER_FILE : DIRENT_DOT_FILE;

	ctx->errcode = ext2fs_read_dir_block(fs, *blocknr, ctx->buf);
	if (ctx->errcode)
		return BLOCK_ABORT;

	while (offset <= fs->blocksize - dirent_min_len) {
		dirent = (struct ext2_dir_entry *) (ctx->buf + offset);
		rec_len = (dirent->rec_len || fs->blocksize < 65536) ?
			dirent->rec_len : 65536;
		if (((offset + rec_len) > fs->blocksize) ||
		    (rec_len < 8) ||
		    ((rec_len % 4) != 0) ||
		    (((dirent->name_len & 0xFF)+8) > rec_len)) {
			ctx->errcode = EXT2_ET_DIR_CORRUPTED;
			return BLOCK_ABORT;
		}
		if (!dirent->inode &&
		    !(ctx->flags & DIRENT_FLAG_INCLUDE_EMPTY))
			goto next;

		ret = (ctx->func)(ctx->dir,
				  (next_real_entry > offset) ?
				  DIRENT_DELETED_FILE : entry,
				  dirent, offset,
				  fs->blocksize, ctx->buf,
				  ctx->priv_data);
		if (entry < DIRENT_OTHER_FILE)
			entry++;

		if (ret & DIRENT_CHANGED) {
			rec_len = (dirent->rec_len || fs->blocksize < 65536) ?
				dirent->rec_len : 65536;
			changed++;
		}
		if (ret & DIRENT_ABORT) {
			do_abort++;
			break;
		}
next:
 		if (next_real_entry == offset)
			next_real_entry += rec_len;

 		if (ctx->flags & DIRENT_FLAG_INCLUDE_REMOVED) {
			size = ((dirent->name_len & 0xFF) + 11) & ~3;

			if (rec_len != size)  {
				unsigned int final_offset;

				final_offset = offset + rec_len;
				offset += size;
				while (offset < final_offset  &&
				       !extundelete_validate_entry(fs, ctx->buf,
							      offset,
							      final_offset))
					offset += 4;
				continue;
			}
		}
		offset += rec_len;
	}

	if (changed) {
		ctx->errcode = ext2fs_write_dir_block(fs, *blocknr, ctx->buf);
		if (ctx->errcode)
			return BLOCK_ABORT;
	}
	if (do_abort)
		return BLOCK_ABORT;
	return 0;
}

