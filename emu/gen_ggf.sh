#!/usr/bin/zsh -f
# Time-stamp: <2023/11/29 12:51:23 fuyuki gen_ggf.sh>

thisd=$0:h

MAXC=100
PFX=6

parse_I1 ()
{
  local order=$1; shift || return $?
  local logf=$1; shift || return $?

  local ve1="eps" ve2="eps2" vec="epsc"
  local vc="C1"

  local -A C=() D=()
  local o= d= cc=() c=
  sed -ne '/C1:/s///p' $logf |\
    while read o d cc
    do
      C[$o]="$cc"
      D[$o]="$d"
    done
  local maxo=$((order + 1))
  local opr=
  local sfx=".0_KTGT"
  print - "!!! $D[-1]"
  o=0
  local CC=()
  while [[ $o -le $maxo ]]
  do
    d=$D[$o]
    ## print - $o $d $cc
    if [[ $o -eq 0 ]]; then
      fput "$vec = 1$sfx"
    else
      fput "$vec = $vec * $ve1"
    fi
    CC=(${=C[$o]})
    [[ $o -eq 0 ]] && CC[1]=0
    horner "${vc}($o)" $ve2 $vec $d $CC || return $?
    let o++
  done
  return 0
}

parse_I1p ()
{
  local order=$1; shift || return $?
  local logf=$1; shift || return $?

  local ve1="eps" ve2="eps2" vec="epsc"
  local vc="C1p"

  local -A C=() D=()
  local o= d= cc=() c=
  sed -ne '/C1p:/s///p' $logf |\
    while read o d cc
    do
      C[$o]="$cc"
      D[$o]="$d"
    done
  local maxo=$((order + 1))
  local opr=
  local sfx=".0_KTGT"
  print - "!!! $D[-1]"

  o=1  # count from 1
  local CC=()
  while [[ $o -le $maxo ]]
  do
    d=$D[$o]
    ## print - $o $d $cc
    if [[ $o -eq 1 ]]; then
      fput "$vec = $ve1"
    else
      fput "$vec = $vec * $ve1"
    fi
    CC=(${=C[$o]})
    [[ $o -eq 0 ]] && CC[1]=0
    horner "${vc}($o)" $ve2 $vec $d $CC || return $?
    let o++
  done
  return 0
}

parse_I2 ()
{
  local order=$1; shift || return $?
  local logf=$1; shift || return $?
  local vk=$1; shift || return $?
  local vc=$1; shift || return $?

  local ve1="eps" ve2="eps2" vec="epsc"

  local -A C=() D=()
  local o= d= cc=() c=
  sed -ne "/${vk}:/s///p" $logf |\
    while read o d cc
    do
      C[$o]="$cc"
      D[$o]="$d"
    done
  local maxo=$((order + 1))
  local opr=
  local sfx=".0_KTGT"
  print - "!!! $D[-1]"
  local CC=()
  o=0
  while [[ $o -le $maxo ]]
  do
    d=$D[$o]
    ## print - $o $d $cc
    if [[ $o -eq 0 ]]; then
      fput "$vec = 1$sfx"
    else
      fput "$vec = $vec * $ve1"
    fi
    CC=(${=C[$o]})
    [[ $o -eq 0 ]] && CC[1]=0
    horner "${vc}($o)" $ve2 $vec $d $CC || return $?
    let o++
  done
  return 0
}

parse_I3 ()
{
  local order=$1; shift || return $?
  local logf=$1; shift || return $?

  local ve1="eps" ve2="eps2" vec="epsc"
  local vc="C3"

  local -A CD=()
  parse_2 $logf CD C3 || return $?

  local maxo=$((order + 0))
  local opr=
  local sfx=".0_KTGT"
  local o=0 v= CC=()
  while [[ $o -le $maxo ]]
  do
    oo=0
    CC=()
    while [[ $oo -le $maxo ]]
    do
      cc=(${=CD[$o,$oo]})
      v="${vc}($oo,$o)"
      CC+=("$v")
      horner "$v" fiii 1 $cc
      let oo++
    done
    # horner "hoge" eps 1 1 "${(@)CC}"
    let o++
  done
  return 0
}

parse_I4a ()
{
  local order=$1; shift || return $?
  local logf=$1; shift || return $?

  local ve1="eps" ve2="eps2" vec="epsc"
  local vc="C4a"

  local -A CD=()
  parse_2 $logf CD C4 || return $?

  local maxo=$((order + 0))
  local opr=
  local sfx=".0_KTGT"
  local o=0 v= CC=()
  while [[ $o -le $maxo ]]
  do
    oo=0
    CC=()
    while [[ $oo -le $maxo ]]
    do
      cc=(${=CD[$o,$oo]})
      v="${vc}($oo,$o)"
      CC+=("$v")
      horner "$v" ep2 1 $cc
      let oo++
    done
    # horner "hoge" eps 1 1 "${(@)CC}"
    let o++
  done
  return 0
}

parse_I4b ()
{
  local order=$1; shift || return $?
  local logf=$1; shift || return $?

  local ve1="eps" ve2="eps2" vec="epsc"
  local vc="C4b"

  local -A CD=()
  parse_2 $logf CD C4n || return $?

  local maxo=$((order + 0))
  local opr=
  local sfx=".0_KTGT"
  local o=0 v= CC=()
  while [[ $o -le $maxo ]]
  do
    oo=0
    CC=()
    while [[ $oo -le $maxo ]]
    do
      cc=(${=CD[$o,$oo]})
      v="${vc}($oo,$o)"
      CC+=("$v")
      horner "$v" fiii 1 $cc
      let oo++
    done
    # horner "hoge" eps 1 1 "${(@)CC}"
    let o++
  done
  return 0
}

# parse_f FILE VAR KEY
#   return VAR[i,j]="D C...."
parse_2 ()
{
  local logf=$1; shift || return $?
  local _var=$1; shift || return $?
  local k=$1; shift || return $?
  local i= j= cc=()
  [[ $_var != _C ]] && local -A _C=()
  sed -ne "/${k}:/s///p" $logf |\
    while read i j cc
    do
      cc=($=cc)
      # maxima --very-quiet --batch-string "ezgcd(${(j:,:)cc});" >&2
      _C[$i,$j]="$cc"
    done
  [[ $_var != _C ]] && set -A $_var "${(@kv)_C}"
  return 0
}

horner ()
{
  local v=$1; shift || return $?
  local f=$1 ffin=$2; shift 2 || return $?
  local d=$1; shift || return $?

  local c= opr=
  for c in "${(Oa)@}"
  do
    if [[ -z $opr ]]; then
      opr="$c"
      if is_integer "$c"; then
        [[ "$c" -ne 0 ]] && opr="$opr$sfx"
      fi
    else
      if is_integer "$c"; then
       if [[ $c -gt 0 ]]; then
         opr="($opr * $f + $c$sfx)"
       else
         c=$((-c))
         opr="($opr * $f - $c$sfx)"
       fi
      else
         opr="($opr * $f + $c)"
      fi
    fi
  done
  local div=
  [[ $d -ne 1 ]] && div=" / $d$sfx"

  if [[ $ffin -eq 1 ]]; then
    fput "${v} = $opr$div"
  else
    fput "${v} = ($opr * $ffin)$div"
  fi
}

is_integer ()
{
  [[ "$1" =~ ^[-+]?[0-9]+$ ]]
}

fput ()
{
   local line="$*"
   local tab="$(printf "%${PFX}s" ' ')"
   local pr=
   local maxc=$((MAXC - PFX))

   if [[ $#line -le $maxc ]]; then
     print - "$tab$line"
   else
     local x=
     local pfx="$tab"
     while [[ $#line -gt $maxc ]]
     do
       pr=${line: :$maxc}
       x=${pr[(I)[*/]]}
       let x--
       print - "$pfx${pr: :$x} &"
       line=${line: $x}
       pfx="$tab & "
     done
     print - "$pfx${line}"
   fi
}

main ()
{
  local order=7
  local odir=$thisd/ggf

  while [[ $# -gt 0 ]]
  do
    case $1 in
    (-O) order=$2; shift;;
    (-o) odir=$2; shift;;
    (-*) print -u2 - "Unknown argument $1"; return 1;;
    (*)  break;;
    esac
    shift
  done

  [[ -z $order ]] && order=5
  mkdir -p $odir || return $?

  local gmax=$thisd/geodesic.max
  local logf=log.ggf-$order
  maxima --batch-string "order:$order; batchload(\"$gmax\");" | sed -ne '/^C/p' > $logf

  parse_I1  $order $logf > $odir/coeffs_i1-o$order.F90 || return $?
  parse_I1p $order $logf > $odir/coeffs_i1p-o$order.F90 || return $?

  parse_I2 $order $logf C2  C2a > $odir/coeffs_i2a-o$order.F90 || return $?
  parse_I2 $order $logf C2y C2b > $odir/coeffs_i2b-o$order.F90 || return $?

  parse_I3 $order $logf > $odir/coeffs_i3-o$order.F90 || return $?
  parse_I4a $order $logf > $odir/coeffs_i4a-o$order.F90 || return $?
  parse_I4b $order $logf > $odir/coeffs_i4b-o$order.F90 || return $?
}

main "$@"
exit $?
