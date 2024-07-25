#!/usr/bin/zsh -f
# Time-stamp: <2024/07/26 09:57:25 fuyuki gen_demo.sh>

odir=$0:h

chak=$CHAK
[[ -z $chak ]] && chak=chak
ngted=ngted

main ()
{
  local out=$1; shift
  local X=$1 Y=$2 Z=$3 T=$4; shift 4
  local dset="$1"

  local dt=(${(s:,:)T})
  T=$dt[1]
  shift dt
  [[ -z $dt ]] && dt=1

  filt=(DUP ABS 1e-6 MAX EXCH SIGN)
  oprx=($X DIV PI 2 MUL MUL COS SQR $filt)
  opry=($Y DIV PI 2 MUL MUL SIN 3 POW $filt)
  oprz=($Z DIV PI 2 MUL MUL SIN 1 ADD $filt)

  for v ttl unit f dim in va 'demo.va' 'shaku' 0   xyz \
                          vb 'demo.vb' 'sun'   3   xyz \
                          vc 'demo.vc' 'kan'   -1  xy
  do                  
    t=0
    tt=1
    dim=(${(s::)dim})

    while [[ $t -lt $T ]]
    do
      size=() opr=()
      xp=(0) yp=(0) zp=(0)
      [[ $dim[(I)x] -gt 0 ]] && size+=(lon/$X) xp=(X $t       ADD $f ADD $X DIV)
      [[ $dim[(I)y] -gt 0 ]] && size+=(lat/$Y) yp=(Y $t 2 MUL ADD        $Y DIV)
      [[ $dim[(I)z] -gt 0 ]] && size+=(lev/$Z) zp=(Z $t NEG   ADD        $Z DIV)

      # [[ $dim[(I)x] -gt 0 ]] && size+=(lon/$X) opr+=(X $t ADD $f ADD $oprx)
      # [[ $dim[(I)y] -gt 0 ]] && size+=(lat/$Y) opr+=(Y $t 2 MUL SUB $opry)
      # [[ $dim[(I)z] -gt 0 ]] && size+=(lev/$Z) opr+=(Z $t NEG ADD   $oprz)
      opr+=($xp $zp ADD PI 2 MUL MUL SIN)
      opr+=($yp $zp ADD PI 2 MUL MUL COS)
      opr+=(ADD ADD)
      # for d in $dim
      # do
      #     opr+=(ADD)
      # done
      # print -u2 - "$opr"

      $chak -H0 -a 1 SIZE=${(j:,:)size} $opr \
            = $out FMT=ur8 \
            ITEM="$v" TITLE="$ttl" UNIT="$unit" DSET="$dset"

      let r++
      tm="$(printf '%02d' $tt)0000"
      $ngted -t $r \
             -e "time:c$((tt+100))" \
             -e "date:c19730130 $tm"  $out
      tt=$((tt + dt))
      let t++
    done
  done

}

out=$odir/demo.gt4
rm -f $out

r=0
main $out 16 8 6 4   'zbt-demo_A'
main $out 12 6 4 2   'zbt-demo_A'
main $out 16 8 6 4   'zbt-demo_B'
main $out 12 6 4 2,2 'zbt-demo_B'
