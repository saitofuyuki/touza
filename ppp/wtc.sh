#!/bin/sh
# Maintainer: SAITO Fuyuki
# Time-stamp: <2025/05/10 15:10:54 fuyuki wtc.sh>

# Copyright (C) 2022
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

this=$0
base=$(basename $this)
root=${base%.*}
mpirun=

main ()
{
  exp=0
  rainbow=0
  # options
  while test $# -gt 0
  do
    case $1 in
    (-r) rainbow=$2; shift;;
    (-t) exp=$2; shift;;
    (-d) testd=$2; shift;;
    (-M) mpirun=$2; shift;;
    (-*) echo "unknown argument $1" >&2; return 1;;
    (*)  break
    esac
    shift
  done
  # parameters (executables)
  test x${testd-} = x && testd=out$root.$$
  mkdir -p $testd
  test x${mpirun:-} = x && mpirun=mpirun

  ntotal=0
  app=$testd/app
  spl=$testd/spl
  exec 3> $app
  exec 4> $spl
  while test $# -gt 0
  do
    x=$1
    shift
    if test $x = /; then
      echo " &nmwspl isplr=$ntotal, /" >&4
      continue
    fi
    test ! -x $x && echo "cannot run $x" >&2 && return 1
    n=1
    test ! -x $1 && n=$1 && shift
    echo "-np $n $x" >&3
    cp $x $testd || return $?
    ntotal=$(expr $ntotal + $n)
  done
  test $x != / && echo " &nmwspl isplr=$ntotal, /" >&4
  exec 3>&-
  exec 4>&-
  if test ${rainbow} -gt 0; then
    cp $app $app.old
    j=0
    while test $j -lt $rainbow
    do
      cat $app.old
      j=$(expr $j + 1)
    done > $app
    cp $spl $spl.old
    j=0
    o=0
    r=0
    while test $j -lt $rainbow
    do
      while read -r l
      do
        s=${l%%,*}; s=${s##*=}
        s=$(expr $o + $s)
        r=$(expr $r + 1)
        echo " &nmwspl isplr=$s, /"
      done < $spl.old
      o=$(expr $o + $ntotal)
      j=$(expr $j + 1)
    done > $spl
    rainbow=$r
  fi

  run $exp $testd $app $spl $rainbow
}

run ()
{
  exp=$1; testd=$2; shift 2 || return $?
  afile=$1 sfile=$2; shift 2 || return $?

  afile=$(realpath --relative-to=$testd $afile)
  sfile=$(realpath --relative-to=$testd $sfile)

  oldpwd=$pwd
  cd $testd || return $?
  case $exp in
  0|1) if test $rainbow -ge 1; then
         r=0
         while test $r -lt $rainbow
         do
           rr=$(printf "%03d" $r)
           cp $sfile SYSIN.CL$rr
           r=$(expr $r + 1)
         done
         test $exp -eq 0 && cp $sfile SYSIN
       else
         cp $sfile SYSIN
       fi;;
    esac

  $mpirun --oversubscribe --tag-output --app $afile > log.out 2> log.err
  cd $oldpwd
  return 0
}


main "$@"; err=$?
exit $err
