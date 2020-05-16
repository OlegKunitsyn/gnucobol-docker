#
# Makefile gnucobol/cobc
#
# Copyright (C) 2003-2012, 2014-2018 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Simon Sobisch
#
# This file is part of GnuCOBOL.
#
# The GnuCOBOL compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# GnuCOBOL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.

bin_PROGRAMS = cobc
cobc_SOURCES = cobc.c cobc.h ppparse.y pplex.c parser.y scanner.c config.c \
	reserved.c error.c tree.c tree.h field.c typeck.c codegen.c \
	config.def flag.def warning.def codeoptim.def ppparse.def codeoptim.c

#cobc_SOURCES = cobc.c cobc.h ppparse.y pplex.l parser.y scanner.l config.c

AM_CPPFLAGS = -I$(top_srcdir)
AM_CFLAGS = $(CODE_COVERAGE_CFLAGS)
cobc_LDADD = $(COBC_LIBS) $(top_builddir)/libcob/libcob.la $(top_builddir)/lib/libsupport.a \
	$(CODE_COVERAGE_LIBS)

dist_man_MANS = cobc.1
COBC = $(top_builddir)/cobc/cobc$(EXEEXT)

# sources that needs to be built *before* any other file
# note: we actually want their header files...
BUILT_SOURCES = ppparse.c parser.c
EXTRA_DIST = pplex.l scanner.l ppparse.h parser.h

MAINTAINERCLEANFILES = pplex.c scanner.c ppparse.h parser.h cobc.1
CLEANFILES = parser.output ppparse.output

# Add rules for code-coverage testing, as provided by AX_CODE_COVERAGE
# FIXME: needs GNU make, even if not enabled
#@ CODE_COVERAGE_RULES @
CODE_COVERAGE_BRANCH_COVERAGE=1
CODE_COVERAGE_LCOV_OPTIONS =  --no-external

cobc.1: cobc.c config.def flag.def warning.def $(top_srcdir)/configure.ac | cobc$(EXEEXT)
	$(HELP2MAN) --output=$@ --info-page=$(PACKAGE) $(COBC)

.l.c:
	LC_CTYPE=C $(LEX) -o$@ $<
