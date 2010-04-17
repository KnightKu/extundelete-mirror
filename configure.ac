#                                               -*- Autoconf -*-
# Process this file with "./autogen.sh" to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(extundelete, 0.2.0, extundelete.sourceforge.net)
echo "Configuring ${PACKAGE_STRING}"
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CONFIG_SRCDIR([src/extundelete.cc])
AC_CONFIG_HEADER([config.h])

# Checks for programs and options necessary during compilation.
CFLAGS=$CFLAGS
CXXFLAGS=$CXXFLAGS
AC_PROG_CXX
AC_PROG_CC

AC_LANG([C])
AX_CHECK_COMPILER_FLAGS([-g], [EU_CFLAGS="$EU_CFLAGS -g"], [EU_CFLAGS="$EU_CFLAGS"])
AX_CHECK_COMPILER_FLAGS([-O2], [EU_CFLAGS="$EU_CFLAGS -O2"], [EU_CFLAGS="$EU_CFLAGS"])
AX_CHECK_COMPILER_FLAGS([-W], [EU_WCFLAGS="$EU_WCFLAGS -W"], [EU_WCFLAGS="$EU_WCFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wall], [EU_WCFLAGS="$EU_WCFLAGS -Wall"], [EU_WCFLAGS="$EU_WCFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wredundant-decls], [EU_WCFLAGS="$EU_WCFLAGS -Wredundant-decls"], [EU_WCFLAGS="$EU_WCFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wshadow], [EU_WCFLAGS="$EU_WCFLAGS -Wshadow"], [EU_WCFLAGS="$EU_WCFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wpointer-arith], [EU_WCFLAGS="$EU_WCFLAGS -Wpointer-arith"], [EU_WCFLAGS="$EU_WCFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wwrite-strings], [EU_WCFLAGS="$EU_WCFLAGS -Wwrite-strings"], [EU_WCFLAGS="$EU_WCFLAGS"])
AX_CHECK_COMPILER_FLAGS([-pg], [EU_CPROF="-pg"], [EU_CPROF=""])
AX_CHECK_COMPILER_FLAGS([-O3], [EU_OPTFLAGS="$EU_OPTFLAGS -O3"], [EU_OPTFLAGS="$EU_OPTFLAGS"])

AC_LANG([C++])
AX_CHECK_COMPILER_FLAGS([-g], [EU_CXXFLAGS="$EU_CXXFLAGS -g"], [EU_CXXFLAGS="$EU_CXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-O2], [EU_CXXFLAGS="$EU_CXXFLAGS -O2"], [EU_CXXFLAGS="$EU_CXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-W], [EU_WCXXFLAGS="$EU_WCXXFLAGS -W"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wall], [EU_WCXXFLAGS="$EU_WCXXFLAGS -Wall"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wredundant-decls], [EU_WCXXFLAGS="$EU_WCXXFLAGS -Wredundant-decls"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wshadow], [EU_WCXXFLAGS="$EU_WCXXFLAGS -Wshadow"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wpointer-arith], [EU_WCXXFLAGS="$EU_WCXXFLAGS -Wpointer-arith"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Wwrite-strings], [EU_WCXXFLAGS="$EU_WCXXFLAGS -Wwrite-strings"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Woverloaded-virtual], [EU_WCXXFLAGS="$EU_WCXXFLAGS -Woverloaded-virtual"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-Weffc++], [EU_WCXXFLAGS="$EU_WCXXFLAGS -Weffc++"], [EU_WCXXFLAGS="$EU_WCXXFLAGS"])
AX_CHECK_COMPILER_FLAGS([-pg], [EU_CXXPROF="-pg"], [EU_CXXPROF=""])
AX_CHECK_COMPILER_FLAGS([-O3], [EU_OPTXXFLAGS="$EU_OPTXXFLAGS -O3"], [EU_OPTXXFLAGS="$EU_OPTXXFLAGS"])


AC_LANG([C])
AC_SUBST([EU_CFLAGS])
AC_SUBST([EU_WCFLAGS])
AC_SUBST([EU_OPTFLAGS])
AC_SUBST([EU_CXXFLAGS])
AC_SUBST([EU_WCXXFLAGS])
AC_SUBST([EU_OPTXXFLAGS])

# Checks for libraries.
AC_CHECK_LIB([ext2fs], [ext2fs_bmap], [],[
           AC_MSG_ERROR([Can't find ext2fs library])] )
AC_CHECK_FUNCS([ext2fs_get_generic_bitmap_start])
AC_CHECK_LIB([com_err], [com_err], [],[
           AC_MSG_ERROR([Can't find com_err library])] )

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h string.h unistd.h utime.h sys/types.h getopt.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_SYS_LARGEFILE
AC_CHECK_TYPES([blk64_t], [], [], [#include <ext2fs/ext2fs.h>])

# Checks for library functions.
AC_FUNC_MEMCMP
AC_FUNC_STAT
AC_CHECK_FUNCS([memset mkdir strerror strtol strtoul])

echo "Writing generated files to disk"
AC_CONFIG_FILES([Makefile
                 src/Makefile])
AC_OUTPUT
