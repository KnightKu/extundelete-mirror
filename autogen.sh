#! /bin/sh

# Clean up the package directories
make -s maintainer-clean 2> /dev/null ||
  rm -f aclocal.m4 config.h.in

# Rebuild the automatically generated files
autoreconf -i -f

# Make configure run silently
sed -i 's,^silent=$,silent=yes,' configure

# Remove cache files
rm -rf autom4te.cache
