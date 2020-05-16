#
# Makefile gnucobol/doc
#
# Copyright (C) 2003-2012, 2015-2018 Free Software Foundation, Inc.
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

if COB_MAKE_RUN_BINARIES
COBCINFOSH = . $(top_builddir)/tests/atconfig && . $(top_builddir)/tests/atlocal && \
	GREP=$(GREP) $(top_srcdir)/doc/cobcinfo.sh
else
COBCINFOSH = GREP=$(GREP) $(top_srcdir)/doc/cobcinfo.sh
endif
	
info_TEXINFOS = gnucobol.texi
GENINCLUDES = cbhelp.tex cbrese.tex cbintr.tex cbsyst.tex cbmnem.tex \
	 cbconf.tex cbchelp.tex cbrunt.tex
gnucobol_TEXINFOS = $(GENINCLUDES) fdl.texi

if COB_MAKE_RUN_BINARIES
cbhelp.tex: $(top_srcdir)/cobc/cobc.c $(top_srcdir)/configure.ac \
	$(top_srcdir)/cobc/flag.def $(top_srcdir)/cobc/warning.def \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@

cbchelp.tex: $(top_srcdir)/bin/cobcrun.c $(top_srcdir)/configure.ac \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@
endif

cbconf.tex: $(top_srcdir)/config/default.conf \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@

cbrunt.tex: $(top_srcdir)/config/runtime.cfg \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@

if COB_MAKE_RUN_BINARIES
cbrese.tex cbintr.tex cbsyst.tex cbmnem.tex: \
	$(top_srcdir)/cobc/cobc.c $(top_srcdir)/cobc/reserved.c \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@
else
cbhelp.tex cbchelp.tex cbrese.tex cbintr.tex cbsyst.tex cbmnem.tex: \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) "fixtimestamps"
endif

EXTRA_DIST = gnucobol.pdf cobcinfo.sh

AM_MAKEINFOHTMLFLAGS = --no-headers --no-split
CLEANFILES = *.aux  *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr *.vrs
MAINTAINERCLEANFILES = $(GENINCLUDES)
TEXI2DVI = texi2dvi -I $(srcdir)
