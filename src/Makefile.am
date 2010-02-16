AM_CFLAGS = -DPROG="\"$(PACKAGE)\"" -DVERSION="\"$(VERSION)\""

bin_PROGRAMS = extundelete

extundelete_CFLAGS = @EU_CFLAGS@ @EU_WCFLAGS@ $(AM_CFLAGS)
extundelete_CXXFLAGS = @EU_CXXFLAGS@ @EU_WCXXFLAGS@ $(AM_CFLAGS)
extundelete_SOURCES = extundelete.cc  block.c  insertionops.cc  block.h  extundelete.h  extundelete-priv.h  jfs_compat.h  kernel-jbd.h

EXTRA_PROGRAMS = extundelete-prof  extundelete-jdebug  extundelete-nodebug

extundelete_prof_CFLAGS = $(extundelete_CFLAGS) @EU_CPROF@
extundelete_prof_CXXFLAGS = $(extundelete_CXXFLAGS) @EU_CXXPROF@
extundelete_prof_SOURCES = $(extundelete_SOURCES)
extundelete_prof_LDFLAGS = -pg

extundelete_jdebug_CFLAGS = -DJDEBUG $(extundelete_CFLAGS)
extundelete_jdebug_CXXFLAGS = -DJDEBUG  $(extundelete_CXXFLAGS)
extundelete_jdebug_SOURCES = $(extundelete_SOURCES)

extundelete_nodebug_CFLAGS = -DNDEBUG @EU_OPTFLAGS@ @EU_WCFLAGS@  $(AM_CFLAGS)
extundelete_nodebug_CXXFLAGS = -DNDEBUG @EU_OPTXXFLAGS@ @EU_WCXXFLAGS@ $(AM_CFLAGS)
extundelete_nodebug_SOURCES = $(extundelete_SOURCES)

profile:	extundelete-prof
	-mv extundelete-prof extundelete

jdebug:	extundelete-jdebug
	-mv extundelete-jdebug extundelete

nodebug:	extundelete-nodebug
	-mv extundelete-nodebug extundelete

clean-local:
	-rm -f $(EXTRA_PROGRAMS)

maintainer-clean-local:
	-rm -f Makefile.in

.PHONY:	profile jdebug nodebug clean-local extundelete maintainer-clean-local
