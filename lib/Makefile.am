#
# Makefile gnucobol/lib
#
# Copyright (C) 2002-2012, 2017, 2020 Free Software Foundation, Inc.
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
# along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.

noinst_HEADERS = gettext.h
noinst_LTLIBRARIES = libsupport.la

AM_CPPFLAGS = -I$(top_srcdir)
libsupport_la_SOURCES = dummymac.c
libsupport_la_LDFLAGS = -static
libsupport_la_LIBADD = @LIBOBJS@
