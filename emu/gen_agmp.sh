#!/usr/bin/zsh -f
# Time-stamp: <2024/01/18 16:54:13 fuyuki gen_agmp.sh>

thisd=$0:h

MAXC=120
PFX=6

main ()
{
  local ORDER=(7)
  local odir=$thisd/agmp

  while [[ $# -gt 0 ]]
  do
    case $1 in
    (-O) ORDER=(${(s:,:)2}); shift;;
    (-o) odir=$2; shift;;
    (-*) print -u2 - "Unknown argument $1"; return 1;;
    (*)  break;;
    esac
    shift
  done

  mkdir -p $odir || return $?

  local gmax=$thisd/intersects_agmp.max

  SET=("$@")
  [[ -z $SET ]] && SET=(e)

  local order=
  for order in $ORDER
  do
    local logf=log.agmp-$order
    maxima --batch-string "ORDER:$order; batchload(\"$gmax\");" | sed -ne '/^C/p' > $logf

    [[ $SET[(I)c] -gt 0 ]] && parse_Cc $order $logf > $odir/agmp-c$order.F90
    [[ $SET[(I)d] -gt 0 ]] && parse_Cd $order $logf > $odir/agmp-d$order.F90
    [[ $SET[(I)e] -gt 0 ]] && parse_Ce $order $logf > $odir/agmp-e$order.F90
  done
}



parse_Cc ()
{
  local order=$1 logf=$2
  local ptag=Cc
  # read
  local cols=() t= c=
  local -A D=() C=() N=() L=()
  local o= j= k= s=
  local cp=
  while read -A cols
  do
    c=$cols[1]; shift cols
    # print -u2 -l - "${(@)cols}"
    t=(${(s/:/)c})
    [[ $t[1] != $ptag ]] && continue
    case $t[2] in
    (n) o=$cols[1]
        shift cols
        N[$o]="$cols"
        ;;
    (d) o=$cols[1]
        shift cols
        [[ $cols[1] != $cols[2] ]] && print -u2 - "Cc[$o]: not greatest divisor: $cols"
        D[$o]="$cols[1]"
        ;;
    (c) o=$cols[1]
        shift cols
        C[$o]="$cols"
        ;;
    (l) o=$cols[1] k=$cols[2]; shift 2 cols
        cp=$cols[1]; shift cols
        s=$cols[1]
        # print -u2 - "($o)($k)($cp)($s)"
        [[ $s == 0 ]] && continue
        s=${s//s^2/s2}
        s=$(print - "$s" | sed -e 's/\(\<[0-9]\+\>\)/\1.0_KTGT/g')
        L[$o,$k]="$s"
    ;;
    (*) print -u2 - "Unknown table($t) $c $cols";;
    esac
  done < $logf

  local nt=
  if [[ $((order % 2)) -eq 0 ]]; then
    nt=$(( (order/4+2) * ((order+2)/4) * 2))
  else
    nt=$(( (order/2+4) * (order/+1) / 2))
  fi
  fput -c "ORDER: $order ($nt terms)"

  local kk=() K=() j00=
  j=0
  for o in ${(onk)N}
  do
    # fput -c "($o) $D[$o] $C[$o]"
    K=(${(Onk)L[(I)$o,*]})
    fput
    fput -c "[c tan(dlat/2)]**$o"
    fput -c "   $N[$o]"

    fput "${ptag}($j) = $D[$o].0_KTGT   !! denom"
    let j++
    # fput -c "${ptag}[$j] = c ** $C[$o]     !! factor tan(dlat/2)**$o"
    # let j++
    for k in $K
    do
      kk=(${(s:,:)k})
      # fput -c "($k)"
      fput "${ptag}($j) = $L[$k]    !! (dlon/2)**$kk[2]"
      [[ $kk[2] -eq 1 && $o -eq 0 ]] && j00=$j
      let j++
    done
    # j=$((j + $#K + 1))
  done
  let j--
  fput
  fput -c "subtract pmem area"
  fput "if (rel) ${ptag}($j00) = 0.0_KTGT"
  # even order:
  #      {4,6,8,10,12}  n=6,12,16,24,30...
  #                       3*2 (2+4)*2 (3+5)*2 (2+4+6)*2 (3+5+7)*2
  #                       (3*1)*2 (3*2)*2 (4*2)*2 (4*3)*2 (5*3)*2
  #                        6*2/2   6*4/2   8*4/2   8*6/2   10*6/2
  #                       (o/4+2)*((o+2)/4)*2
  # odd order:
  #      {5,7,9,11}     n=9,14,20,27,...
  #                       2+3+4, 2+3+4+5, 2+..+6, 2+..+7
  #                       6*3/2  7*4/2    8*5/2   9*6/2
  #                       (o/2+4)*(o/2+1)/2
  #   ((o+1)/4 + (o/4) + 4) * ((n % 2) * (n / 2 + 1) % 2)
  #   4 5 6 7 8 9 10 11 12
  #   2   4   4   6     6
  #     3   4   5    6
  #   2 2 4 4 4 4 6  6  6  (n+2)/4 * 2
  #   0 1 0 1 0 1 0  1  0  n % 2
  #   1 1 0 0 1 1 0  0  1  (n / 2 + 1) % 2
  #   0 1 0 0 0 1 0  0  0
  #   0 1 0 0 0 1 0  0  0
  #   3 3 4 4 5 5 6  6  7  (n+2)/2

  # for o in ${(ok)L}
  # do
  #   print - "! ($o) $L[$o]"
  # done
}

parse_Cd ()
{
  local order=$1 logf=$2
  local ptag=Cd
  # read
  local cols=() t= c=
  local -A D=() C=() N=() L=() T=()
  local o= j= k= s=
  local cp=
  while read -A cols
  do
    c=$cols[1]; shift cols
    # print -u2 -l - "${(@)cols}"
    t=(${(s/:/)c})
    [[ $t[1] != $ptag ]] && continue
    case $t[2] in
    (n) o=$cols[1]
        shift cols
        N[$o]="$cols"
        ;;
    (d) o=$cols[1]
        shift cols
        [[ $cols[1] != $cols[2] ]] && print -u2 - "Cc[$o]: not greatest divisor: $cols"
        D[$o]="$cols[1]"
        ;;
    (c) o=$cols[1]
        shift cols
        C[$o]="$cols"
        ;;
    (t) o=$cols[1]; shift cols
        y="$cols[1]" d="$cols[2]" x="$cols[3]"
        y="${y//x/\\\\dLonH}"
        y="${y//s/\\\\SinL}"
        y="${y//[*]/}"
        if [[ $x == 1 ]]; then
          x=''
        elif [[ $x == c ]]; then            
          x="${x//c/\\\\CosL\\\\tdLatH}"
        else
          x="${x//c/(\\\\CosL\\\\tdLatH)}"
        fi
        if [[ $d -eq 1 ]]; then
          yd="$y"
        else
          if [[ -n $y ]]; then
            yd="\\\\frac{$y}{$d}"
          else
            yd="\\\\frac{1}{$d}"
          fi
      fi
        T[$o]="$T[$o] + $yd"
        ;;
    (l) o=$cols[1] k=$cols[2]; shift 2 cols
        cp=$cols[1]; shift cols
        s=$cols[1]
        # print -u2 - "($o)($k)($cp)($s)"
        [[ $s == 0 ]] && continue
        s=${s//s^2/s2}
        s=$(print - "$s" | sed -e 's/\(\<[0-9]\+\>\)/\1.0_KTGT/g')
        L[$o,$k]="$s"
    ;;
    (*) print -u2 - "Unknown table($t) $c $cols";;
    esac
  done < $logf

  local no=$(( (order + 1) / 2))
  local nt=$(( no * (no + 3) ))
  # if [[ $((order % 2)) -eq 0 ]]; then
  #   nt=$(( (order/4+2) * ((order+2)/4) * 2))
  # else
  #   nt=$(( (order/2+4) * (order/+1) / 2))
  # fi
  fput -c "ORDER: $order ($nt terms)"

  for o in ${(onk)T}
  do
    tt=${T[$o]# *[+]}
    print -u2 - "D_{$o} & = $tt "'\\\\'
  done

  local kk=() K=() j00=
  j=0
  for o in ${(onk)N}
  do
    # fput -c "($o) $D[$o] $C[$o]"
    K=(${(Onk)L[(I)$o,*]})
    fput
    fput -c "[c tan(dlat/2)]**$o"
    fput -c "   $N[$o]"

    fput "${ptag}($j) = $D[$o].0_KTGT   !! denom"
    let j++
    # fput -c "${ptag}[$j] = c ** $C[$o]     !! factor tan(dlat/2)**$o"
    # let j++
    for k in $K
    do
      kk=(${(s:,:)k})
      # fput -c "($k)"
      fput "${ptag}($j) = $L[$k]    !! (dlon/2)**$kk[2]"
      [[ $kk[2] -eq 1 && $o -eq 0 ]] && j00=$j
      let j++
    done
    # j=$((j + $#K + 1))
  done
  let j--
  fput
  fput -c "subtract pmem area"
  fput "if (rel) ${ptag}($j00) = 0.0_KTGT"
}

parse_Ce ()
{
  local order=$1 logf=$2
  local ptag=Ce
  local tblv=C
  # read
  local cols=() t= c=
  local -A D=() C=() N=() L=() T=()
  local o= j= k= s=
  local cp=
  while read -A cols
  do
    c=$cols[1]; shift cols
    # print -u2 -l - "${(@)cols}"
    t=(${(s/:/)c})
    [[ $t[1] != $ptag ]] && continue
    case $t[2] in
    (n) o=$cols[1]
        shift cols
        N[$o]="$cols"
        ;;
    (d) o=$cols[1]; shift cols
        D[$o]="$cols[1]"
        ;;
    (l) o=$cols[1]; shift cols
        k=$cols[1]; shift cols
        s=$cols[1]
        [[ $s == 0 ]] && continue
        s=$(print - "$s" | sed -e 's/\(tdhsq\)^2/\1*\1/g')
        s=$(print - "$s" | sed -e 's/\(\<[0-9]\+\>\)/\1.0_KTGT/g')
        L[$o,$k]="$s"
        ;;
    (t) o=$cols[1]; shift cols
        y="$cols[1]" d="$cols[2]" x="$cols[3]"
        y="${y//y/\\\\eta}"
        y="${y//\*/}"
        x="${x//x/\\\\xi}"
        if [[ $d -eq 1 ]]; then
          if [[ $#y -eq 1 ]]; then
            yd="$y"
          else
            yd="($y)"
          fi
        else
          yd="\\\\frac{$y}{$d}"
        fi
        T[$o]="$T[$o] + $yd $x"
        ;;
    (*) print -u2 - "Unknown table($t) $c $cols";;
    esac
  done < $logf
  local nt=$(( order * (order + 3) ))
  fput -c "ORDER: $order ($nt terms)"

  for o in ${(onk)T}
  do
    tt=${T[$o]# *[+]}
    print -u2 -l "C_{$o} = $tt"
  done
  # for k in "${(@ok)L}"
  # do
  #   print -u2 - "$k: ${L[$k]}"
  # done
  # print -u2 - "terms: $#L $nt"
  local sc=
  local j=0 n=
  for o in ${(onk)N}
  do
    if [[ $((o % 2)) -eq 0 ]]; then
      sc=sin
    else
      sc=cos
    fi
    n=$((2 * (o / 2) + 1))
    fput
    if [[ $sc == cos ]]; then
      fput -c "[$o] ${sc}[$n lat] tan(dlat/2)"
    else
      fput -c "[$o] ${sc}[$n lat]"
    fi
    fput "${ptag}($j) = $D[$o].0_KTGT   !! denom"
    let j++
    K=(${(onk)L[(I)$o,*]})
    for k in $K
    do
      k=${k#*,}
      # fput -c "  [$o,$k] (dlon/2)**$k"
      fput "${ptag}($j) = $L[$o,$k]   !! (dlon/2)**$k"
      let j++
    done
  done
  fput
  fput -c "subtract pmem area"
  fput "if (rel) ${ptag}(1) = 0.0_KTGT"
  fput

  j=0
  local jj=0
  local jd= je= line=() cj=
  for o in ${(onk)N}
  do
    if [[ $((jj % 2)) -eq 0 ]]; then
      if [[ $o -eq 0 ]]; then
        fput "dlhf = dlh"
      else
        fput "dlhf = dlhf * dlhsq"
      fi
    fi
    if [[ $((o % 2)) -eq 0 ]]; then
      sc=sin
    else
      sc=cos
    fi
    n=$((order - (o / 2)))
    # print -u2 - "$o $n"
    line=""
    for k in {$n..1}
    do
      cj=$(( j + k ))
      # print -u2 - "  $o $n $k"
      if [[ -z $line ]]; then
        line="${ptag}($cj)"
      else
        line="($line*dlhsq+${ptag}($cj))"
      fi
    done
    fput "${tblv}($jj) = $line * dlhf / ${ptag}($j)"
    j=$(( j + n + 1))
    let jj++
  done
  return 0

  local kk=() K=() j00=
  j=0
  for o in ${(onk)N}
  do
    # fput -c "($o) $D[$o] $C[$o]"
    K=(${(Onk)L[(I)$o,*]})
    fput
    fput -c "[c tan(dlat/2)]**$o"
    fput -c "   $N[$o]"

    fput "${ptag}($j) = $D[$o].0_KTGT   !! denom"
    let j++
    # fput -c "${ptag}[$j] = c ** $C[$o]     !! factor tan(dlat/2)**$o"
    # let j++
    for k in $K
    do
      kk=(${(s:,:)k})
      # fput -c "($k)"
      fput "${ptag}($j) = $L[$k]    !! (dlon/2)**$kk[2]"
      [[ $kk[2] -eq 1 && $o -eq 0 ]] && j00=$j
      let j++
    done
    # j=$((j + $#K + 1))
  done
  let j--
  fput
  fput -c "subtract pmem area"
  fput "if (rel) ${ptag}($j00) = 0.0_KTGT"
}


is_integer ()
{
  [[ "$1" =~ ^[-+]?[0-9]+$ ]]
}

fput ()
{
   local comment=
   [[ $1 == -c ]] && comment='!! ' && shift
   local line="$*"
   [[ -z $line && -z $comment ]] && print - && return 0
   local tab="$(printf "%${PFX}s" ' ')"
   local pr=
   local maxc=$((MAXC - PFX))

   if [[ $#line -le $maxc ]]; then
     print - "$tab${comment}$line"
   else
     local x=
     local pfx="$tab"
     while [[ $#line -gt $maxc ]]
     do
       pr=${line: :$maxc}
       x=${pr[(I)[*/]]}
       let x--
       if [[ -z $comment ]]; then
         print - "$pfx${pr: :$x} &"
         pfx="$tab & "
       else
         print - "$pfx${comment}${pr: :$x}"
         pfx="$tab${comment}  "
       fi
       line=${line: $x}
     done
     print - "$pfx${line}"
   fi
}


main "$@"
exit $?
