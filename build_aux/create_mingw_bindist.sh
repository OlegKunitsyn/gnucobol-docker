#!/bin/sh
# create_mingw_bindist.sh gnucobol
#
# Copyright (C) 2016-2019 Free Software Foundation, Inc.
# Written by Simon Sobisch
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


# This shell script needs to be sourced from Makefile processing,
# otherwise set EXTSRCDIR and EXTBUILDDIR before calling this script
# AND make sure EXTBUILDDIR exists with the right content

# Check we're in a MinGW environment
if test ! -d "/mingw/bin"; then
	echo "binary mingw dist packages can only be created from MSYS/MinGW"
	echo "directory /mingw/bin not found"
	exit 99
fi

# Check necessary vars:
if test "x$EXTBUILDDIR" = "x"; then
	EXTBUILDDIR="."
	echo "EXTBUILDDIR" not set, "." assumed
fi
if test "x$EXTSRCDIR" = "x"; then
	EXTSRCDIR="."
	echo "EXTSRCDIR" not set, "." assumed
fi
if test ! -f "$EXTBUILDDIR/config.log"; then
	echo "$EXTBUILDDIR/config.log" does not exist, aborting $0
	exit 5
fi
if test ! -f "$EXTSRCDIR/configure"; then
	echo "$EXTSRCDIR/configure" does not exist, aborting $0
	exit 5
fi

# Create folder
target_dir=$EXTBUILDDIR/GnuCOBOL_mingw
echo
echo "Building MinGW dist package for GnuCOBOL"
echo "target: $target_dir"
echo
if test -e "$target_dir"; then
	fdate=$(date +%F_%h%m%S)
	echo "target directory already exist - renaming it to $target_dir-$fdate"
	mv "$target_dir" "$target_dir-$fdate"
	if test -e "$target_dir"; then
		echo "cannot move old target directory" && exit 98
	fi
fi
mkdir "$target_dir" || (echo "cannot create target directory" && exit 97)
pushd "$target_dir" 1>/dev/null
if test "$target_dir" != "$(pwd)"; then
   target_dir="$(pwd)"
	echo "target (resolved): $target_dir"
fi
popd 1>/dev/null

echo && echo copying MinGW files...
echo "  bin..."
cp -pr "/mingw/bin"          "$target_dir/"
echo "  include..."
cp -pr "/mingw/include"      "$target_dir/"
echo "  lib..."
cp -pr "/mingw/lib"          "$target_dir/"
echo "  libexec..."
cp -pr "/mingw/libexec"      "$target_dir/"

echo && echo copying GnuCOBOL files...
cp -pr "$EXTBUILDDIR/extras" "$target_dir/"
cp -pr "$EXTSRCDIR/copy"     "$target_dir/"
cp -pr "$EXTSRCDIR/config"   "$target_dir/"

cp -p "$EXTBUILDDIR/cobc/.libs/cobc.exe" "$target_dir/bin/"
cp -p "$EXTBUILDDIR/bin/.libs/cobcrun.exe" "$target_dir/bin/"
cp -p $EXTBUILDDIR/libcob/.libs/libcob*.dll  "$target_dir/bin/"
cp -p $EXTBUILDDIR/libcob/.libs/libcob.*  "$target_dir/lib/"
mkdir "$target_dir/include/libcob"
cp -p $EXTSRCDIR/libcob.h    "$target_dir/include/"
cp -p $EXTSRCDIR/libcob/*.h  "$target_dir/include/libcob"
cp -p $EXTSRCDIR/libcob/*.def  "$target_dir/include/libcob"

echo && echo copying docs...

pushd "$EXTSRCDIR" 1>/dev/null
cp README "$target_dir/README.txt"

for file in             \
	"ChangeLog"         \
	"NEWS"              \
	"THANKS"            \
	"COPYING"           \
	"COPYING.LESSER"    \
	"COPYING.DOC"    \
	"COPYING.DOC"
do
   sed -e 's/\r*$/\r/' "$file" > "$target_dir/$(basename "$file").txt"
done
sed -e 's/\r*$/\r/' "bin/ChangeLog" > "$target_dir/ChangeLog_bin.txt"
sed -e 's/\r*$/\r/' "cobc/ChangeLog" > "$target_dir/ChangeLog_cobc.txt"
sed -e 's/\r*$/\r/' "libcob/ChangeLog" > "$target_dir/ChangeLog_libcob.txt"

# copy manpages and translations
#cp bin/cobcrun.1
#cp cobc/cobc.1
##cp libcob/libcob.3

popd 1>/dev/null

pushd "$EXTBUILDDIR" 1>/dev/null
sed -e 's/\r*$/\r/' "config.log" > "$target_dir/config.log"
if test -f "tests/testsuite.log"; then
	sed -e 's/\r*$/\r/' "tests/testsuite.log" > "$target_dir/testsuite.log"
else
	echo "WARNING: GnuCOBOL testsuite results not found!"
fi
if test -f "tests/cobol85/summary.log"; then
	sed -e 's/\r*$/\r/' "tests/cobol85/summary.log" > "$target_dir/NIST_summary.log"
else
	echo "WARNING: NIST results not found!"
fi

if test -f "doc/gnucobol.pdf"; then
	cp -p "doc/gnucobol.pdf"   "$target_dir/GnuCOBOL.pdf"
else
	if test -f "$EXTSRCDIR/doc/gnucobol.pdf"; then
		cp -p "$EXTSRCDIR/doc/gnucobol.pdf"   "$target_dir/GnuCOBOL.pdf"
	else
		echo "WARNING: GnuCOBOL.pdf will be missing"
	fi
fi
popd 1>/dev/null


echo && echo stripping binaries...
rm -rf "$target_dir/bin_stripped"
cp -rp "$target_dir/bin" "$target_dir/bin_stripped"
rm -rf "$target_dir/lib_stripped"
cp -rp "$target_dir/lib" "$target_dir/lib_stripped"
pushd "$target_dir/bin_stripped" 1>/dev/null
strip -p --strip-debug --strip-unneeded *.dll *.exe 2>/dev/null
cd "../lib_stripped"
strip -p --strip-debug --strip-unneeded *.a         2>/dev/null
popd 1>/dev/null

cat >$target_dir/set_env.cmd <<'_FEOF'
@echo off

echo.
echo Setting environment for GnuCOBOL 3.1 with MinGW binaries
echo (GCC 4.8.1, BDB 6.1.23, PDcurses 3.4, MPIR 2.7.0)

:: Check if called already
:: if yes, check if called from here - exit, in any other case 
:: raise warning and reset env vars
if not "%COB_MAIN_DIR%" == "" (
	echo.
	if "%COB_MAIN_DIR%" == "%~dp0" (
	   echo Information: batch was called alread from "%COB_MAIN_DIR%"
	   echo              skipping environment setting...
	   goto :cobcver
	) else (
	   echo Warning: batch was called before from "%COB_MAIN_DIR%"
	   echo          resetting COB_CFLAGS, COB_LDFLAGS 
	   set COB_CFLAGS=
	   set COB_LDLAGS=
	)
)

:: Get the main dir from the batch's position (only works in NT environments)
set COB_MAIN_DIR=%~dp0

:: settings for cobc
set COB_CONFIG_DIR=%COB_MAIN_DIR%config
set COB_COPY_DIR=%COB_MAIN_DIR%copy
set COB_CFLAGS=-I"%COB_MAIN_DIR%include" %COB_CFLAGS%
set COB_LDFLAGS=-L"%COB_MAIN_DIR%lib" %COB_LDFLAGS%

:: settings for libcob
rem the following won't work in GnuCOBOL 1.1 if there are spaces in COB_MAIN_DIR
set COB_LIBRARY_PATH=%COB_MAIN_DIR%extras

:: Add the bin path of GnuCOBOL (including GCC) to PATH for further references
set PATH=%COB_MAIN_DIR%bin;%PATH%

:: Compiler version output
:cobcver
echo.
cobc --version

_FEOF

sed -i -e 's/\r*$/\r/' "$target_dir/set_env.cmd"


cat >$target_dir/BUGS.txt <<'_FEOF'

Known bugs found in this distribution, which are normally not in GnuCOBOL n.n:

* NONE

_FEOF

sed -i -e 's/\r*$/\r/' "$target_dir/BUGS.txt"


# only add to README
cat >>$target_dir/README.txt <<'_FEOF'

This package (MinGW based) is intended for testing purposes on Windows systems
and has everything needed to run the compiler and runtime, including GCC 4.8.1
as C compiler.
Other components are BDB 6.1.23, PDcurses 3.4, MPIR 2.7.0 (gmpcompat, without
any processor optimization).

It is NOT optimized and may have some minor bugs other binaries created from the
source tarball don't have.

Important: See BUGS.txt for possible known issues in this distribution!

For running GnuCOBOL simply open a command prompt and call set_env.cmd in this
folder once. You can use cobc/cobcrun in the command prompt afterwards.

_FEOF

sed -i -e 's/\r*$/\r/' "$target_dir/README.txt"

echo && echo FINISHED
echo && echo make sure to adjust set_env.cmd, README.txt and BUGS.txt
