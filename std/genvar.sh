#!/usr/bin/zsh -f
# Time-stamp: <2023/11/20 13:05:36 fuyuki genvar.sh>

this="$0"

main ()
{
  local ovw=
  local aref= avar= filter=
  local karg=
  local sed=ssed
  if [[ $# -eq 0 ]]; then
    print -u2 - "Usage: $this [-o] SOURCE PROCEDURE_i ..."
    return 0
  fi

  while [[ $# -gt 0 ]]
  do
    case $1 in
    (-o) ovw=$1;;
    (-A) karg=$2; shift || return $?;;
    (-A*) karg=${1: 2};;
    (-r) aref=$2; shift || return $?;;
    (-r*) aref=${1: 2};;
    (-v) avar=$2; shift || return $?;;
    (-v*) avar=${1: 2};;
    (-d) filter=diff;;
    (-x) filter=xxdiff;;
    (*)  break;;
    esac
    shift
  done

  local file=$1; shift
  local sub=

  local tmp=$(mktemp)
  local src=$(mktemp)
  local mod=$(mktemp)

  local bpat= epat=
  local xpat=() x=
  local var=() ref= base= v=
  local rtype=
  local macro=

  local -A KIND=(i KI32 l KI64 f KFLT d KDBL q KQPL)
  [[ -z $karg ]] && karg=KARG

  var=(${(s::)avar})
  [[ -z $var ]] && var=(i l f d a)

  for sub in "$@"
  do
    ref=${sub##*_}
    if [[ $#ref -eq 1 ]]; then
      base=${sub%_*}_
    else
       [[ -z $aref ]] && print -u2 - "cannot set reference suffix for $sub" && return 1
       ref=$aref
       base=${sub%$ref}
    fi
    print -u2 - "transform: $base+[$ref] > $sub"

    bpat="^[^!]*\\(subroutine\\|function\\) *$sub"
    epat="^[^!]*end .*\\(subroutine\\|function\\) *$sub"

    $sed -ne "/$bpat/,/$epat/p" < $file > $src

    case $ref in
    (i|l) rtype=integer;;
    (f|d) rtype=real;;
    (*)   print -u2 - "unknown reference type $refe;"; return 1;;
    esac

    rm -f $mod; touch $mod
    for v in $var
    do
      if [[ $v == $ref ]]; then
        cat $src >> $mod
        continue
      fi
      rep=${base}$v
      macro=
      case $v in
      (q) macro=OPT_REAL_QUADRUPLE_DIGITS;
      esac

      if [[ -n $macro ]]; then
        print - "#if $macro > 0" >> $mod
      fi
      case $v in
      (i) $sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
               -e "s/\\($karg *=>\\? *\\)[^ ]*\\>/\\1KI32/" \
               -e "s/^\\( *\\)$rtype\\((KIND=$karg),\\)/\\1integer\\2/I" \
               < $src >> $mod;;
      (l) $sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
               -e "s/\\($karg *=>\\? *\\)[^ ]*\\>/\\1KI64/" \
               -e "s/^\\( *\\)$rtype\\((KIND=$karg),\\)/\\1integer\\2/I" \
               < $src >> $mod;;
      (f) $sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
               -e "s/\\($karg *=>\\? *\\)[^ ]*\\>/\\1KFLT/" \
               -e "s/^\\( *\\)$rtype\\((KIND=$karg),\\)/\\1real\\2/I" \
               < $src >> $mod;;
      (d) $sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
               -e "s/\\($karg *=>\\? *\\)[^ ]*\\>/\\1KDBL/" \
               -e "s/^\\( *\\)$rtype\\((KIND=$karg),\\)/\\1real\\2/I" \
               < $src >> $mod;;
      (q) $sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
               -e "s/\\($karg *=>\\? *\\)[^ ]*\\>/\\1KQPL/" \
               -e "s/^\\( *\\)$rtype\\((KIND=$karg),\\)/\\1real\\2/I" \
               < $src >> $mod;;
      (a) $sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
               -e "s/^\\( *\\)$rtype\\((KIND=$karg),\\)/\\1character(len=*),  /I" \
               < $src >> $mod;;
      esac
      if [[ -n $macro ]]; then
        print - "#endif /* $macro > 0 */" >> $mod
      fi
    done
    xpat=()
    for v in $var
    do
      rep=${base}$v
      xpat+=(-e "/^[^!]*\\(subroutine\\|function\\) *$rep\\>/","/^[^!]*end .*\\(subroutine\\|function\\) *$rep\\>/"d)
    done
    # output
    {
      $sed -e "/$bpat/,\$d" "${(@)xpat}" $file
      [[ ${var[(I)$ref]} -eq 0 ]] && cat $src
      cat $mod
      $sed -e "1,/$epat/d" "${(@)xpat}" $file
    } > $tmp
    if [[ -n $ovw ]]; then
      mv $file $file.org
      mv $tmp $file
    elif [[ -z $filter ]]; then
      cat $tmp
    else
      case $filter in
      (diff)   diff $file $tmp;;
      (xxdiff) xxdiff $file $tmp;;
      (*) print -u2 - "Unknown filter $filter"; return 1;;
      esac
    fi
  done

  rm -f $src $mod $tmp
  return 0
}

main "$@"; err=$?
exit $err
