#!/bin/sh
# Time-stamp: <2024/07/27 10:16:32 fuyuki param.sh>
# create param.py

# usage param.sh [-o OUTPUT] CLASS INPUT

while test $# -gt 0
do
  case $1 in 
  -o) out=$2; shift;;
  *)  break;;
  esac
  shift
done

test x"$out" != x &&  exec > $out
while test $# -gt 0
do
  class=$1 in=$2; shift 2 || exit $?
  test ! -f "$in" && echo "Not exist input source = $in" >&2 && exit 1
  echo "class $class:"
  sed -n -e '/^# *define *\([A-Z][A-Za-z0-9_]*\) *\([^ ]\+\)/s//    \1 = \2/p' $in
  echo
  shift
done

exit 0
