SUBDIRS = src
AM_MAKEFLAGS = -s
EXTRA_DIST = LICENSE README autogen.sh

profile:
	$(MAKE) RECURSIVE_TARGETS=profile-recursive $(AM_MAKEFLAGS) $(@:profile=profile-recursive)

jdebug:
	$(MAKE) RECURSIVE_TARGETS=jdebug-recursive $(AM_MAKEFLAGS) $(@:jdebug=jdebug-recursive)

nodebug:
	$(MAKE) RECURSIVE_TARGETS=nodebug-recursive $(AM_MAKEFLAGS) $(@:nodebug=nodebug-recursive)

profile-am: Makefile config.h
jdebug-am: Makefile config.h
nodebug-am: Makefile config.h

package: clean
	-rm -f aclocal.m4
	-touch -r config.h.in aclocal.m4
	$(MAKE) $(AM_MAKEFLAGS) $(@:package=dist-bzip2)

clean-local:
	-rm -f config.h.in~
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -f aclocal.m4 config.h.in Makefile.in configure
	-rm -f compile depcomp install-sh missing

.PHONY:	profile profile-am jdebug jdebug-am nodebug nodebug-am package clean-local maintainer-clean-local
