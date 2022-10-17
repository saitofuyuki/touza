#!/bin/sh
# Maintainer: SAITO Fuyuki
# Created: Jun 7 2020
# Time-stamp: <2022/10/17 15:22:20 fuyuki autogen.sh>

# Copyright (C) 2020, 2021
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

DRY=T
OPTS=''
CLEAR=''
VERBOSE=''
while test $# -gt 0
do
  case $1 in
  -v) VERBOSE=-v;;
  -c) CLEAR=T;;
  -y) DRY=;;
  -I) OPTS="$OPTS $1 $2"; shift;;
  *)  break;;
  esac
  shift
done

thisd="$(dirname "$0")"
thisd="$(realpath $thisd)" || exit $?

if test x"$DRY" != x; then
  echo "$0: dry-run"
  echo "If you are sure, run as $0 -y [ARGS...]"
fi

run ()
{
  if test x"$DRY" = x; then
    echo ": $*"
    "$@"; err=$?
  else
    echo "dry-run: $*"
    err=0
  fi
  return $err
}

stdm4d=
stdm4f=mt_init.m4
for d in "m4c" "$@"
do
  test -f "$d/$stdm4f" && stdm4d="$d" && break
done
if test x"$stdm4d" = x; then
  echo "$0: not found $stdm4f." >&2
  echo "$0: rerun as $0 SEARCH-PATH" >&2
  exit 1
fi
stdm4d="$(realpath $stdm4d)" || exit $?

for dir in jmz .
do
  cd "$thisd/$dir" || exit $?
  TMP=
  # fake automake
  for t in COPYING
  do
    test ! -f "$t" && TMP="$TMP $t"
  done
  echo "# Create $TMP temporally to fake automake."
  # shellcheck disable=SC2086
  test x"$TMP" != x && run touch $TMP

  trap 'run rm -f $TMP; exit $err' 1 2 3 15

  if test x$CLEAR != x; then
    run rm -rf aclocal.m4 autom4te.cache || exit $?
  fi
  run libtoolize || exit $?
  run aclocal $OPTS -I "$stdm4d" || exit $?
  run autoheader || exit $?

  # All we need is the file INSTALL
  alibd="$(automake --print-libdir)"
  for t in INSTALL
  do
    t=$alibd/$t
    if test -f "$t"; then
      run cp "$t" . || exit $?
    fi
  done

  run automake --add-missing --gnu || exit $?
  echo "# automake rerun"
  # shellcheck disable=SC2086
  run rm -f $TMP
  run automake --foreign || exit $?

  run autoconf $VERBOSE || exit $?
done
