#!/usr/bin/bash -f
# Time-stamp: <2020/11/04 21:22:18 fuyuki wta.sh>

main ()
{
  run_ta a               || return $?
  run_ta a b             || return $?
  run_ta a b c           || return $?
  run_ta a b c d         || return $?
  run_ta a b c d e       || return $?
  run_ta a     Y=y       || return $?
  run_ta a     Y=y X=x   || return $?
  run_ta a b   X=x       || return $?

  run_ta a Y=y b   || return $?
  run_ta a Y=y b d || return $?
  run_ta a X=x c   || return $?

  run_ta Y=y a b  || return $?
  run_ta a =b =c =d || return $?

  run_ta a b c Y=x X=x   || return $?

  run_ta a V=v U=u b          || return $?
  run_ta a V=v U=u Y=y X=  b  || return $?
  run_ta a V=v U=u Y=y X=x b  || return $?
}


run_ta ()
{
  local args=("$@")
  local cmd=./test_std_arg
  echo "### $cmd ${args[*]}"
  $cmd "${args[@]}"
  return 0
}


main "$@"; err=$?
exit $err
