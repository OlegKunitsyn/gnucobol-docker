#!/bin/sh
# cobcinfo.sh gnucobol/doc
#
# Copyright (C) 2010,2012, 2016-2019 Free Software Foundation, Inc.
# Written by Roger While, Simon Sobisch
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

# use GREP from configure, passed when called from Makefile
GREP_ORIG="$GREP";
if test "x$GREP" = "x"; then GREP=grep; fi
if test "x$SED" = "x" ; then SED=sed  ; fi

# default to POSIX, Solaris for example uses "tail +"
if test "x$TAIL_START" = "x"; then TAIL_START="tail -n +"; fi
#if test "x$TAIL_LAST" = "x"; then TAIL_LAST="tail -n "; fi

if test "$1" != "fixtimestamps"; then

   # test for grep -A
   $GREP -A2 test /dev/null 2>/dev/null 1>&2
   if test "$?" -ne 1; then
      GREP=ggrep
      $GREP -A2 test /dev/null 2>/dev/null 1>&2
      if test "$?" -ne 1; then
         echo "error: grep not working, re-run with GREP=/path/to/gnu-grep"
         echo "       GREP is currently \"$GREP_ORIG\""
         exit 1
      fi
   fi

   if test "x$COBC" = "x"; then
      echo 'WARNING: $COBC not set, defaulting to "cobc"'
      COBC=cobc
   fi
   if test "x$COBCRUN" = "x"; then
      echo 'WARNING: $COBCRUN not set, defaulting to "cobcrun"'
      COBCRUN=cobcrun
   fi
   
   # test for working executables
   $COBC    -V 2>/dev/null 1>&2
   ret=$?
   if test "$ret" -ne 0; then
     echo "error: cobc is not working, re-run with COBC=/path/to/cobc"
     echo "       and ensure that its dependencies can be found."
     echo "       COBC is currently \"$COBC\""
     exit $ret
   fi
   $COBCRUN -V 2>/dev/null 1>&2
   if test "$ret" -ne 0; then
     echo "error: cobcrun is not working, re-run with COBCRUN=/path/to/cobcrun"
     echo "       and ensure that its dependencies can be found."
     echo "       COBCRUN is currently \"$COBCRUN\""
     exit $ret
   fi

fi

# Make sure to source atconfig/atlocal before running this shell
# to use the currently compiled version of cobc
# (is done in the makefile for automated calls)

# Function for creating the actual file and check
_create_file () {
  echo "$0: creating $1"
  case "$1" in
	"cbhelp.tex")
		rm -rf $1
		$COBC -q --help | $GREP -E "ptions.*:" | cut -d: -f1 | \
		while read section; do
			header_found=""
#			FIXME: re-adjust as this currently results in two SIGSEGV
#			       which are just hidden here (and shouldn't)
			$COBC -q --help               2>/dev/null | \
				$GREP    -A2000 "$section" | \
				$GREP -E -B2000 "^$" -m 1  | \
				$SED -e 's/^\t/D~/g'            \
				     -e 's/\t/~/g'              \
				     -e 's/* NOT \(.\+\)/; @emph{not \1}/g' \
				     -e 's/* ALWAYS \(.\+\)/; @emph{always \1}/g' \
				     -e 's/* /; /g' \
				     -e 's/^    \+/D~/g'         \
				     -e 's/^ \+//g'                \
				     -e 's/  \+/~/g'               \
				     -e 's/<\([^>]\+\)>/@var{\1}/g'| \
			while IFS='~' read -r name desc; do
				if test -z "$name"; then continue; fi
				if test -z "$header_found"; then
					header_found=1
					echo "@section $section"   >>$1
					echo "@table @code"        >>$1
				else
					if test "$name" != "D"; then
						echo "@item @code{$name}"  >>$1
					fi
					echo "$desc"      | \
					$SED -e 's/ \(-[Wfv][a-z-]*\)/ @option{\1}/g'       \
					     -e 's/\([ (]\)\([A-Z][A-Z -]*[A-Z]\)/\1@code{\2}/g' \
					     -e 's/^\([A-Z][A-Z -]*[A-Z]\)/@code{\1}/g' \
					     -e 's/@code{\(IBM\|ANSI\|ISO\|NIST\)}/\1/g'   >>$1
				fi
			done
			echo "@end table"          >>$1
		done
		;;
	"cbchelp.tex")
		rm -rf $1
		header_found=""
		$COBCRUN -q --help                | \
			$GREP -E -A2000 -E "ptions.*:" | \
			$GREP -E -B2000 "^$" -m 1       | \
			$SED -e 's/^    \+/D~/g'         \
			     -e 's/^ \+//g'               \
			     -e 's/  \+/~/g'               \
			     -e 's/<\([^>]\+\)>/@var{\1}/g'| \
		while IFS='~' read -r name desc; do
			if test -z "$name"; then continue; fi
			if test -z "$header_found"; then
				header_found=1
				echo "@table @code"        >>$1
			else
				if test "$name" != "D"; then
					echo "@item @code{$name}"  >>$1
				fi
				echo "$desc"          | \
				$SED -e 's/ -M/ @option{-M}/g' \
				    -e 's/\(COB[A-Z_]\+\)/@env{\1}/g' >>$1
			fi
		done
		echo "@end table"          >>$1
		;;
	"cbrese.tex")
		echo "@section Common reserved words"   >$1
		echo "@multitable @columnfractions .40 .20 .40"  >>$1
		echo "@headitem Reserved word @tab Implemented @tab Aliases" >>$1
		$COBC -q --list-reserved | \
			$GREP -E -B9999 "^$" -m 2 | \
			$SED -e 's/  \+/;/g' \
			     -e 's/ (Context sensitive/\t(C\/S/g' \
			     -e 's/(aliased with \([^)]*\))/;@code{\1}/g' \
			     -e 's/ ;/;/g' | \
		while IFS=';' read -r name impl aliases; do
			if test -z "$name"; then continue; fi
			if test -z "$header_found"; then
				header_found=1
			else
				echo "@item @code{$name} @tab $impl @tab $aliases"  >>$1
			fi
		done
		echo "@end multitable" >>$1

		needs_comma=""
		header_found=""
		$COBC -q --list-reserved    | \
			$GREP    -A50 "Extra"   | \
			$GREP -E -B50 "^$" -m 1 | \
		while read line; do
			if test -z "$line"; then continue; fi
			if test -z "$header_found"; then
				header_found=1
				echo "@section $line"  >>$1
			else
				if test -z "$needs_comma"; then needs_comma=1
				else printf ", " >>$1; fi
				printf "@code{%s}" "$line" >>$1
			fi
		done
		printf "\n\n" >>$1

		header_found=""
		echo "@section Internal registers"  >>$1
		echo "@multitable @columnfractions .40 .20 .40"  >>$1
		echo "@headitem Register @tab Implemented @tab Definition" >>$1
		$COBC -q --list-reserved     | \
			$GREP -A100 "registers"  | \
			$SED -e 's/  \+/~/g'     | \
		while IFS='~' read -r name impl definition; do
			if test -z "$name"; then continue; fi
			if test -z "$header_found"; then
				header_found=1
			else
				echo "@item @code{$name} @tab $impl @tab @code{$definition}"  >>$1
			fi
		done
		echo "@end multitable" >>$1
		;;
	"cbintr.tex")
		echo "@multitable @columnfractions .40 .20 .40"  >$1
		$COBC -q --list-intrinsics | \
			$SED -e 's/  \+/\t/g'   | \
		while IFS='~' read -r name impl params; do
			if test -z "$name"; then continue; fi
			if test -z "$header_found"; then
				header_found=1
				echo "@headitem $name @tab $impl @tab $params"  >>$1
			else
				echo "@item @code{$name} @tab $impl @tab $params"  >>$1
			fi
		done
		echo "@end multitable" >>$1
		;;
	"cbsyst.tex")
		echo "@multitable @columnfractions .40 .20"  >$1
		$COBC -q --list-system     | \
			$SED -e 's/  \+/~/g'   | \
		while IFS='~' read -r name params; do
			if test -z "$name"; then continue; fi
			if test -z "$header_found"; then
				header_found=1
				echo "@headitem $name @tab $params"  >>$1
			else
				echo "@item @code{$name} @tab $params"  >>$1
			fi
		done
		echo "@end multitable" >>$1
		;;
	"cbmnem.tex")
		system_names="device feature switch"
		section_prefix="System names"
		rm -rf $1
		for section in $system_names; do
			needs_comma=""
			echo "@section $section_prefix: $section"   >>$1
			$COBC -q --list-mnemonics | \
				$GREP "$section" | cut -d' ' -f1 |\
			while read name; do
				if test -z "$needs_comma"; then needs_comma=1
				else printf ", " >>$1; fi
				printf "@code{%s}" "$name" >>$1
			done
			printf "\n\n" >>$1
		done
		;;
	"cbconf.tex")
		lines=2
		$GREP -A9999 "https://www.gnu.org/licenses/" \
		  "$confdir/default.conf" \
		| $SED -e 's/\r//g'  \
		       -e 's/# \?TO-\?DO.*//g'  \
		| $TAIL_START$lines >$1
		;;
	"cbrunt.tex")
		# First section, as it is formatted different
		$GREP -A400 -m1 "##" "$confdir/runtime.cfg" | \
			$GREP -B400 -m2 "##" | \
			cut -b2- | \
			$SED -e 's/\r//g'  \
			     -e 's/^#$//g'  \
			     -e 's/^#\( .*\)/@section\1\n/g' \
			     -e 's/^ //g' \
			     -e 's/{/@{/g' \
			     -e 's/}/@}/g' \
			     -e 's/\(Example:\)  \(.*\)$/\n\1 @code{\2}/g' \
			     -e 's/  \([^ ][^(]*\)  \([,.]\)/ @code{\1}\2/g' \
			     -e 's/  \([^ ][^(]*\)  / @code{\1} /g' \
			     -e 's/  \([^ ][^(]*\)$/ @code{\1}/g' \
			     -e 's/^$/@\*/g'          > $1
		lines=`cat $1 | wc -l`
		lines=`expr 20 + $lines`
		# All other sections
		echo "@verbatim"               >>$1
		$TAIL_START$lines "$confdir/runtime.cfg" | \
			cut -b2- | \
			$SED -e 's/\r//g' \
			     -e 's/# \?TO-\?DO.*$//g'  \
			     -e 's/^#\( .*\)/@end verbatim\n@section\1\n@verbatim/g' \
			     -e 's/^ //g'           >>$1
		echo "@end verbatim"           >>$1
		;;
  esac
}

docdir="`dirname $0`"
confdir="$docdir/../config"
created_texfiles="cbhelp.tex cbchelp.tex cbrese.tex cbintr.tex cbsyst.tex"
created_texfiles="$created_texfiles cbmnem.tex cbconf.tex cbrunt.tex"


# for old systems that don't support this POSIX parameter expansion:
#case "$1" in
# otherwise: only use filename (strip possible path)
case "${1##*/}" in
	"")
		for file in $created_texfiles; do
			_create_file $file
		done
		;;
	"help")
		_create_file "cbhelp.tex"
		_create_file "cbchelp.tex"
		;;
	"lists")
		_create_file "cbrese.tex"
		_create_file "cbintr.tex"
		_create_file "cbsyst.tex"
		_create_file "cbmnem.tex"
		;;
	"conf")
		_create_file "cbconf.tex"
		_create_file "cbrunt.tex"
		;;
	"cbhelp.tex")
		_create_file "cbhelp.tex"
		;;
	"cbchelp.tex")
		_create_file "cbchelp.tex"
		;;
	"cbrese.tex")
		_create_file "cbrese.tex"
		;;
	"cbintr.tex")
		_create_file "cbintr.tex"
		;;
	"cbsyst.tex")
		_create_file "cbsyst.tex"
		;;
	"cbmnem.tex")
		_create_file "cbmnem.tex"
		;;
	"cbconf.tex")
		_create_file "cbconf.tex"
		;;
	"cbrunt.tex")
		_create_file "cbrunt.tex"
		;;
	"fixtimestamps")
		echo $0: touch tex-includes
		for file in $created_texfiles; do
			echo " touch $file"
			touch $file
		done
		if test "$2" != "includes"; then
			echo $0: touch tex-results
			for file in $docdir/gnucobol.*; do
				if test "$file" = "$docdir/gnucobol.texi"; then continue; fi
				echo " touch $file"
				touch $file
			done
		fi
		;;
	*)
		echo "$0: ERROR: called with unsupported option $1"
		exit 1;
esac
