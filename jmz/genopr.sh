#!/usr/bin/zsh -f
# Time-stamp: <2023/05/19 08:57:49 fuyuki genopr.sh>

this=$0:t
jmzd=$0:h

main ()
{
  local opt=
  while [[ $# -gt 0 ]]
  do
    case $1 in
    (-[dxwc]) opt=$1;;
    (-*) print -u2 - "unknown argument $1"; return 1;;
    (*)  break;
    esac
    shift
  done

  local call=("$@")
  [[ -z $call ]] && call=(decl reg call sub elem)

  local -A GRP=() SUBG=()
  local -A NSTACK=() ALIAS=() SYM=()
  local -A DSTACK=() # stack properties for description
  local -A OPT=() PARAM=() INFIX=()
  local -A FUNC=()
  local -A IVAR=() AVAR=()
  local -A GRANGE=()
  local -A DESCR=()

  register_all || return $?

  local GODR=(system output anchor stack queue unary ubool bool binary filter lazy float other)
  GODR=($GODR ${(k)GRP})
  GODR=(${(u)GODR})
  local grp=
  for grp in $GODR
  do
    GRANGE[$grp]="grp_${grp}_bgn grp_${grp}_end"
  done

  [[ -z ${(M)call:#d*} ]] || run $opt decl $GODR || return $?
  [[ -z ${(M)call:#r*} ]] || run $opt register $GODR || return $?
  [[ -z ${(M)call:#c*} ]] || run $opt call $GODR || return $?
  [[ -z ${(M)call:#s*} ]] || run $opt sub $GODR || return $?
  [[ -z ${(M)call:#e*} ]] || run $opt elem $GODR || return $?
  [[ -z ${(M)call:#t*} ]] || run $opt table $GODR || return $?
  [[ -z ${(M)call:#l*} ]] || run $opt list $GODR || return $?
  return 0
}

run ()
{
  local opt=
  case $1 in
  (-d) opt=(diff diff); shift;;
  (-w) opt=(write); shift;;
  (-x) opt=(diff xxdiff); shift;;
  (-c) opt=(clip xclip -i); shift;;
  esac
  local sub=$1; shift
  local of=
  case $sub in
  (d*) of=$jmzd/chak_decl.F90;;
  (r*) of=$jmzd/chak_reg.F90;;
  esac
  local cmd=(output_$sub $of "$@")

  case $opt[1] in
  (diff) [[ -z $of ]] && print -u2 - "Need old file" && return 1;
         [[ ! -e $of ]] && print -u2 - "no old file $of" && return 1
         ($cmd || return $?) | $opt[2] - $of
         ;;
  (write) [[ -z $of ]] && print -u2 - "Need old file" && return 1;
          $cmd > $of || return $?
          ;;
  (clip)  shift opt
          $cmd | $opt || return $?
          ;;
  (*)     $cmd || return $?
  esac
  return 0
}


# cf. http://orc.csres.utexas.edu/documentation/html/refmanual/ref.syntax.precedence.html
#     https://en.cppreference.com/w/c/language/operator_precedence
register_all ()
{
  # output
  register -g output -s '='  -n 1,0 OUTPUT_POP
  register -g output -s ':=' -n 1,1 OUTPUT_KEEP

  # system
  register -g system -s - -n 1,1 INPUT
  register -g system -s - -n 1,0 OUTPUT
  register -g system -s -        ANCHOR
  register -g system -s - -n 1,1 TRANSF

  # anchor management
  register -g anchor MARK 'fragile anchor (removed by first touch)'
  register -g anchor STOP 'robust anchor (removed by GO)'
  register -g anchor GO   'remove last anchor'

  # stack manipulation
  register -g stack -n 1,2         DUP      'duplicate top stack'
  register -g stack -n 1,2         COPY     'copy top stack on new buffer'
  register -g stack -n 1,1         CLONE    'copy top stack on new buffer and discard the original'
  register -g stack -n 1,0 -o NAME POP      'discard top stack and optionally tag'
  register -g stack -n 2,2         EXCH     'B A; exchange two top stacks'
  register -g stack -n 0,0         NOP      'no operation; do nothing'
  register -g stack +n m,'2(m-1)'  DIST     'distribute top stack for every stack from last anchor'
  register -g stack +n m,m         INSERT   'move top stack after last anchor'
  register -g stack +n m,'2m'      REPEAT   'repeat from last non-adjacent anchor'
  register -g stack +n m,0         FLUSH    'flush-out from last anchor'
  register -g stack +n m,0         DFLUSH   'flush-out from last anchor (defined only)'
  register -g stack +n m,0 -o COLS CFLUSH   'flush-out from last anchor (columnized)'

  # queue manipulation
  register -g queue ITER   'iterate last queue operator for each set from last anchor'
  register -g queue CUM    'apply last queue non-unary operator from last anchor'
  register -g queue MAP    'reserved; DIST ITER'

  # logical operation
  register -n 2,1 -i logical,'&&'  AND   'logical and; B if both A and B are defined, else MISS'
  register -n 2,1 -i call          MASK  'A if both A and B are defined, else MISS'

  # logical unary
  register -g ubool -n 1,1            -i neg,'!'  NOT   'logical not; 1 if undefined, else MISS'
  register -g ubool -n 1,1            -i call     BOOL  'boolean; 1 if defined, else MISS'
  register -g ubool -n 1,1 -f -,FALSE -i call     BIN   'binary; 1 if defined, else 0'

  # logical operation, inclusive
  register -g lazy -n 2,1            -i logical,'||'  OR      'logical or; A if defined, else B if defined, else MISS'
  register -a OR                                      LOR
  register -g lazy -n 2,1            -i call          ROR     'logical or (reverse); B if defined, else A if defined, else MISS'
  register -g lazy -n 2,1            -i call          XOR     'logical exclusive-or; A or B if B or A undefined, else MISS'
  register -g lazy -n 2,1 -f AND     -i call          LAND    'lazy AND'
  register -g lazy -n 2,1 -f MASK    -i call          LMASK   'lazy MASK'
  register -g lazy -n 2,1 -f LAY,-,T -i call          LLAY    'background layer; B if A outside, else A'
  register -a LLAY                                    LAY
  register -g lazy -n 2,1 -f LAY,-,F -i call          RLAY    'background layer; A if B outside, else B'

  # primitive binary
  register -n 2,1 -i add,'+'  ADD     'A+B'
  register -n 2,1 -i add,'-'  SUB     'A-B'
  register -n 2,1 -i mul,'*'  MUL     'A*B'
  register -n 2,1 -i mul,'/'  DIV     'A/B'
  register -n 2,1 -i mul,'//' IDIV    'A//B'
  register -n 2,1 -i mul,'%'  MOD     'mod(A,B)'
  register -n 2,1 -i exp,'**' POW     'pow(A,B)'

  register -n 2,1 -i call MODULO      'modulo(A,B)'

  # primitive binary inclusive
  register -g lazy -n 2,1 -i add,'+'  -f ADD,ZERO LADD    'lazy ADD'
  register -g lazy -n 2,1 -i add,'-'  -f SUB,ZERO LSUB    'lazy SUB'
  register -g lazy -n 2,1 -i mul,'*'  -f MUL,ONE  LMUL    'lazy MUL'
  register -g lazy -n 2,1 -i mul,'/'  -f DIV,ONE  LDIV    'lazy DIV'

  # primitive unary
  register          -n 1,1 -i neg,'-'  NEG     '-A'
  register          -n 1,1 -i call     INV     '1/A'
  register          -n 1,1 -i call     ABS     'abs(A)'
  register          -n 1,1 -i call     SQR     'A*A'
  register -g float -n 1,1 -i call     SQRT    'square root'
  register          -n 1,1 -i call     SIGN    'copy A sign on 1'
  register          -n 1,1 -i call     ZSIGN   '-1,0,+1 if negative,zero,positive'

  # integer opration
  register -n 1,1 -i call          FLOOR   'largest integer <= A'
  register -n 1,1 -i call          CEIL    'smallest integer >=A'
  register -n 1,1 -i call          ROUND   'nearest integer of A'
  register -n 1,1 -i call          TRUNC   'truncate toward 0'
  register -n 1,1 -i call -f TRUNC INT     'truncate toward 0 and convert'

  # math operation
  register -g float -n 1,1 -i call EXP      'exp(A)'
  register -g float -n 1,1 -i call LOG      'log(A)'
  register -g float -n 1,1 -i call LOG10    'log10(A)'
  register -g float -n 1,1 -i call SIN      'sin(A)'
  register -g float -n 1,1 -i call COS      'cos(A)'
  register -g float -n 1,1 -i call TAN      'tan(A)'
  register -g float -n 1,1 -i call ASIN     'arcsin(A)'
  register -g float -n 1,1 -i call ACOS     'arccos(A)'
  register -g float -n 1,1 -i call ATAN     'arctan(A)'
  register -g float -n 2,1 -i call ATAN2    'arctan(A/B)'
  register -g float -n 1,1 -i call SINH     'sinh(A)'
  register -g float -n 1,1 -i call COSH     'cosh(A)'
  register -g float -n 1,1 -i call TANH     'tanh(A)'

  register -g float -n 1,1 -i call R2D      'Convert radian to degree'
  register -g float -n 1,1 -i call D2R      'Convert degree to radian'

  register -g float -n 2,1 -i call HYPOT    'sqrt(A * A + B * B)'

  # bitwise operation
  register -n 2,1 -i and,'&'    BITAND   'bitwise AND'
  register -n 2,1 -i or,'|'     BITOR    'bitwise OR'
  register -n 2,1 -i xor,'^'    BITXOR   'bitwise XOR'
  register -n 1,1 -i neg,'~'    BITNOT   'bitwise NOT'
  register -n 2,1 -i shift,'<<' LSHIFT   'bitwise left shift'
  register -n 2,1 -i shift,'>>' RSHIFT   'bitwise right shift'

  # floating-point operation
  register -g float -n 1,1 -i call EXPONENT  'exponent(A)'
  register -g float -n 1,1 -i call FRACTION  'fraction(A)'
  register -g float -n 2,1 -i call SCALE     'scale(A,B)'
  register -g float -n 2,1 -i call NEAREST   'nearest(A,B)'
  register -g float -n 1,1 -i call SPACING   'spacing(A)'
  register -g float -n 1,1 -i call RRSP      'rrspacing(A)'

  # other operation
  register         -n 2,1 -i call             MIN    'min(A,B)'
  register         -n 2,1 -i call             MAX    'max(A,B)'
  register -g lazy -n 2,1 -i call -f -,ULIMIT LMIN   'lazy MIN'
  register -g lazy -n 2,1 -i call -f -,LLIMIT LMAX   'lazy MAX'

  # conditional operation (binary)
  register -g bool -n 2,1 -f -,FALSE -i call EQB       '1 if A==B, else 0'
  register -g bool -n 2,1 -f -,FALSE -i call NEB       '1 if A!=B, else 0'
  register -g bool -n 2,1 -f -,FALSE -i call LTB       '1 if A<B, else 0'
  register -g bool -n 2,1 -f -,FALSE -i call GTB       '1 if A>B, else 0'
  register -g bool -n 2,1 -f -,FALSE -i call LEB       '1 if A<=B, else 0'
  register -g bool -n 2,1 -f -,FALSE -i call GEB       '1 if A>=B, else 0'

  # conditional operation (binary or MISS)
  register -g bool -n 2,1 -i call EQ      '1, 0, MISS for A==B, not, either MISS'
  register -g bool -n 2,1 -i call NE      '1, 0, MISS for A!=B, not, either MISS'
  register -g bool -n 2,1 -i call LT      '1, 0, MISS for A<B, not, either MISS'
  register -g bool -n 2,1 -i call GT      '1, 0, MISS for A>B, not, either MISS'
  register -g bool -n 2,1 -i call LE      '1, 0, MISS for A<=B, not, either MISS'
  register -g bool -n 2,1 -i call GE      '1, 0, MISS for A>=B, not, either MISS'

  register -a EQ EQU
  register -a NE NEU
  register -a LT LTU
  register -a GT GTU
  register -a LE LEU
  register -a GE GEU

  # conditional operation (filter)
  register -g filter -n 2,1 -i call EQF      'A if A==B, else MISS'
  register -g filter -n 2,1 -i call NEF      'A if not A==B, else MISS'
  register -g filter -n 2,1 -i call LTF      'A if A<B, else MISS'
  register -g filter -n 2,1 -i call GTF      'A if A>B, else MISS'
  register -g filter -n 2,1 -i call LEF      'A if A<=B, else MISS'
  register -g filter -n 2,1 -i call GEF      'A if A>=B, else MISS'

  # transform
  register -n 2,1 -i call EXTR            'extraction'
  register -n 2,1 -i call -p COOR CDIFF   'central difference'
  register -n 2,1 -i call -p COOR FDIFF   'forward difference'
  register -n 2,1 -i call -p COOR BDIFF   'backward difference'

  # reduction operation
  register -g reduction -o RANK -i call NORM    'normalize (0:1) through stacks or rank(s)'
  register -g reduction -o RANK -i call SUM     'sum through stacks or rank(s)'
  register -g reduction -o RANK -i call AVR     'arithmetic mean through stacks or rank(s)'
  register -g reduction -o RANK -i call COUNT   'count defined elements through stacks or rank(s)'

  register -g reduction -n 1,1 -i call -p RANK -s 'MIN=' UMIN    'minimum'
  register -g reduction -n 1,1 -i call -p RANK -s 'MAX=' UMAX    'maximum'

  # transform operation
  #### coor=0,1,2,name,alias for coodinate, -1 or s for stack
  # register COUNT=COOR
  # register AVR=COOR
  # register MEAN=COOR
  # register HIGH=COOR
  # register LOW=COOR

  # property manipulation
  register -g buffer -p NAME              TAG
  register -g buffer -p NAME              DESC           'description'
  register -g buffer -o NAME              FUNC           'function declaration'
  register -g buffer -p NAME/REPL/RANGE,.. -n 1,1 PERM   'array shape permutatation'
  register -a PERM SHAPE
  register -g buffer -p NAME/REPL/SIZE,..  -n 1,1 SIZE    'array size(shape) permutatation'
  register -g buffer -p NAME/SHIFT         -n 1,1 CSHIFT  'circular shift along coordinate'
  register -g buffer -p NAME/SHIFT         -n 1,1 EOSHIFT 'end-off shift along coordinate'

  register -g buffer,index -o NAME/REPL/RANGE    C0  'put top stack coordinate[0] index'
  register -g buffer,index -o NAME/REPL/RANGE    C1  'put top stack coordinate[1] index'
  register -g buffer,index -o NAME/REPL/RANGE    C2  'put top stack coordinate[2] index'
  register -g buffer,index -o NAME/REPL/RANGE    C3  'put top stack coordinate[3] index'
  register -g buffer,index -o NAME/REPL/RANGE    X   'put top stack coordinate[0] index'
  register -g buffer,index -o NAME/REPL/RANGE    Y   'put top stack coordinate[1] index'
  register -g buffer,index -o NAME/REPL/RANGE    Z   'put top stack coordinate[2] index'

  register -g index        -o NAME,...           FLAT  'flat index'
  register -a FLAT INDEX

  # register -g buffer -p VALUE                 MISS    "replace missing value"

  register -g header        -p FORMAT      FMT     "set output data format"
  register -g header        -p STRING      ITEM    "item replacement or filter"
  register -g header        -p STRING      UNIT
  register -g header        -p STRING      TITLE
  register -g header        -p STRING      EDIT
  register -g header        -p STRING      DSET
  register -g header        -p LIST   -s T TSEL    "time filter"
  register -g header        -p LIST   -s R RSEL    "record filter"
  register -g header,buffer -p VALUE       MISS    "replace missing value"
  register -g header,buffer -o UNIT        DUR     "duration"

  return 0
}

# register [-g GROUP[,SUBGROUP]][-n POP,PUSH][-a ALIAS][-s SYMBOL]
#          [-o OPTION][-p PARAM][-i INFIX][-f FUNC]
#          OPERATOR [DESCRIPTION]

register ()
{
  local grp= subg= nstack= alias= sym=
  local opt= param= infix= func=
  local opr= descr= dstack=
  while [[ $# -gt 0 ]]
  do
    case $1 in
    (-g) subg=(${(s:,:)2}); grp=$subg[1]; shift subg; shift;;
    (-n) nstack=(${(s:,:)2}); shift;;
    (+n) dstack=(${(s:,:)2}); shift;;
    (-a) alias=$2; shift;;
    (-s) sym=$2; shift;;
    (-o) opt=$2; shift;;
    (-p) param=$2; shift;;
    (-i) infix=(${(s:,:)2}); shift;;
    (-f) func=$2; shift;;
    (--) shift; break;;
    (-*) print -u2 - "unknown option $1"; exit 1;;
    (*)  break;;
    esac
    shift
  done
  local key=$1; shift
  local descr="$*"
  # symbol (string)
  [[ -z $sym ]] && sym="$key"
  [[ $sym == - ]] && sym=" $key"  # system-only symbol

  ALIAS[$key]="$alias"
  DESCR[$key]="$descr"
  # group
  if [[ -z $grp ]]; then
    if [[ -n $alias ]]; then
      grp=${(k)GRP[(r)* $alias *]}
      [[ -z $grp ]] && grp=${(k)GRP[(r)* $alias]}
      [[ -z $grp ]] && print -u2 - "cannot find alias source for $key ($alias)" && return 1
    elif [[ $nstack[1] -eq 2 && $nstack[2] -eq 1 ]]; then
      grp='binary'
    elif [[ $nstack[1] -eq 1 && $nstack[2] -eq 1 ]]; then
      grp='unary'
    else
      grp='other'
    fi
  fi
  GRP[$grp]+=" $key"

  [[ -n $subg ]] && GRP[$subg]+=" $key"
  SUBG[$key]="$subg"
  OPT[$key]="$opt"
  PARAM[$key]="$param"
  FUNC[$key]="$func"
  INFIX[$key]="$infix"
  SYM[$key]="$sym"
  NSTACK[$key]="$nstack"
  DSTACK[$key]="$dstack"

  IVAR[$key]="opr_$key"
  AVAR[$key]="str_$key"

  return 0
}

output_decl ()
{
  local of=$1; shift
  local grp= key=
  local iv= av= gv=()
  local subg=
  # symbol
  output_f90_header "$of" "operator symbol declaration"

  fout "!! operation symbols"
  for grp in $@
  do
    fout "!! group: $grp"
    for key in ${=GRP[$grp]}
    do
      subg=$SUBG[$key]
      [[ ${subg[(wI)$grp]} -gt 0 ]] && continue
      av=$AVAR[$key]
      fout "character(len=*),parameter :: $av = '$SYM[$key]'"
      let jnum++
    done
  done
  # id
  fout "!! operation id"
  local jnum=0
  local ref=
  for grp in $@
  do
    gv=($=GRANGE[$grp])
    fout "!! group: $grp"
    fout "integer,parameter :: $gv[1] = $jnum"
    for key in ${=GRP[$grp]}
    do
      iv=$IVAR[$key]
      subg=$SUBG[$key]
      [[ ${subg[(wI)$grp]} -gt 0 ]] && continue
      if [[ -n $ALIAS[$key] ]]; then
        ref=$IVAR[$ALIAS[$key]]
        fout "integer,parameter :: $iv = $ref"
      else
        fout "integer,parameter :: $iv = $jnum"
        let jnum++
      fi
    done
    fout "integer,parameter :: $gv[2] = $jnum"
  done
  return 0
}

output_register ()
{
  local of="$1"; shift
  local grp= key=
  local iv= av=
  local nstack=()
  local infix=() rarg=()
  local sub=
  output_f90_header "$of" "operator registration"
  # symbol
  for grp in "$@"
  do
    for key in ${=GRP[$grp]}
    do
      subg=$SUBG[$key]
      [[ ${subg[(wI)$grp]} -gt 0 ]] && continue

      av=$AVAR[$key]
      iv=$IVAR[$key]
      nstack=(${=NSTACK[$key]})
      infix=(${=INFIX[$key]})
      rarg=(ierr "$iv" "$av")
      [[ -n $nstack ]] && rarg+=($nstack[1] $nstack[2])
      [[ -n $infix[1] ]] && rarg+=("ilev=ilev_$infix[1]")
      [[ -n $infix[2] ]] && rarg+=("istr='$infix[2]'")
      fout -t 4 "if (ierr.eq.0) call reg_opr_prop(${(j:, :)rarg})"
      # if [[ -z $nstack ]]; then
      #   fout -t 4 "if (ierr.eq.0) call reg_opr_prop(ierr, $iv, $av)"
      # else
      #   fout -t 4 "if (ierr.eq.0) call reg_opr_prop(ierr, $iv, $av, $nstack[1], $nstack[2])"
      # fi
    done
  done
  return 0
}

output_call ()
{
  local grp= key=
  local iv= av=
  local nstack=() push= pop=
  local func=()
  local sub= ssfx=
  local apply= afunc=
  local post=() ptmp=()
  local args=()
  local wk=
  for grp in "$@"
  do
    case $grp in
    (unary) apply=apply_opr_UNARY;;
    (binary) apply=apply_opr_BINARY;;
    (lazy) apply=apply_opr_BINARY_lazy;;
    (ubool) apply=apply_opr_UNARY;;
    (bool)  apply=apply_opr_BINARY;;
    (filter)  apply=apply_opr_BINARY;;
    (float) apply=;;
    (*)    continue;;
    esac
    for key in ${=GRP[$grp]}
    do
      [[ -n $ALIAS[$key] ]] && continue
      args=('ierr' 'handle' 'lefts(1:push)' 'righth(1:pop)')
      iv=$IVAR[$key]
      if [[ -n $ALIAS[$key] ]]; then
        fout "!! $key == $ALIAS[$key]"
        continue
      fi
      nstack=(${=NSTACK[$key]})
      pop=$nstack[1] push=$nstack[2]
      [[ $pop -gt 1 ]] && args+=(cmode)
      func=(${(s:,:)FUNC[$key]})
      post=(${func: 1})
      if [[ $grp == bool || $grp == ubool ]]; then
        # ignore post (used in sub template)
        post=('.TRUE.')
      elif [[ $grp == lazy ]]; then
        ptmp=("${(@)post}")
        if [[ -n $ptmp[2] ]]; then
          if [[ x${ptmp[1]:--} == x- ]]; then
            case $ptmp[2] in
            (T) post=(rev=.TRUE.);;
            (F) post=(rev=.FALSE.);;
            (*) post=(rev=$ptmp[2]);;
            esac
          fi
        fi
      fi
      afunc=$apply
      if [[ -z $afunc ]]; then
        if [[ $pop -eq 1 && $push -eq 1 ]]; then
          afunc=apply_opr_UNARY
        elif [[ $pop -eq 2 && $push -eq 1 ]]; then
          afunc=apply_opr_BINARY
        else
          print -u2 - "Cannot determine apply function for $key"
          return 1
        fi
      fi
      get_sub_name sub $key $pop
      args+=($sub $post)
      fout "else if (handle.eq.$iv) then"
      fout "  call ${afunc}(${(j:, :)args})"
    done
  done
}

output_sub ()
{
  local grp= key=
  local nstack=() push= pop=
  local sub= stype=
  local candi=(unary binary lazy ubool bool filter float)
  local elem=
  local DONE=()
  local extval=
  for grp in "$@"
  do
    [[ $candi[(I)$grp] -eq 0 ]] && continue
fout -t 0 "!!!_  - $grp operations"
    for key in ${=GRP[$grp]}
    do
      [[ -n $ALIAS[$key] ]] && continue
      nstack=(${=NSTACK[$key]})
      pop=$nstack[1] push=$nstack[2]
      get_sub_name sub $key $pop
      [[ ${DONE[(I)$sub]} -gt 0 ]] && continue
      DONE+=($sub)
      get_elem_name elem $key $pop
      stype=$grp

      extval=
      if [[ $grp == bool || $grp == ubool ]]; then
        func=(${(s:,:)FUNC[$key]})
        post=(${func: 1})
        [[ -n $post ]] && extval=$post
      fi
      [[ $stype == ubool ]] && stype=unary
      [[ $stype == bool ]] && stype=binary
      [[ $stype == filter ]] && stype=binary
      if [[ $stype == float ]]; then
        if [[ $pop -eq 1 && $push -eq 1 ]]; then
          stype=unary
        elif [[ $pop -eq 2 && $push -eq 1 ]]; then
          stype=binary
        else
          print -u2 - "Cannot determine subroutine for $key"
          return 1
        fi
      fi
      case $stype in
      (unary)
          cat <<UNARY
!!!_   . $sub
  subroutine $sub &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = $elem(X(jx), F)
       else
          Z(jz) = ${extval:-F}
       endif
    enddo
  end subroutine $sub
UNARY
          ;;
      (binary)
          cat <<BINARY
!!!_   . $sub
  subroutine $sub &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = $elem(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = ${extval:-FZ}
       endif
    enddo
  end subroutine $sub
BINARY
          ;;
      (lazy)
          cat <<LAZY
!!!_   . $sub
  subroutine $sub &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = $elem(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine $sub
LAZY
          ;;
      (*) print -u2 - "unknown subroutine type $stype"; return 1;;
      esac
    done
  done
}
output_elem ()
{
  local grp= key=
  local nstack=() push= pop=
  local sub= stype=
  local candi=(unary binary lazy ubool bool filter float)
  local elem=
  local DONE=()
  for grp in "$@"
  do
    [[ $candi[(I)$grp] -eq 0 ]] && continue
    for key in ${=GRP[$grp]}
    do
      [[ -n $ALIAS[$key] ]] && continue
      nstack=(${=NSTACK[$key]})
      pop=$nstack[1] push=$nstack[2]
      get_elem_name elem $key $pop
      [[ ${DONE[(I)$elem]} -gt 0 ]] && continue
      DONE+=($elem)
      stype=$grp
      [[ $stype == ubool ]] && stype=unary
      [[ $stype == bool ]] && stype=binary
      [[ $stype == filter ]] && stype=binary
      if [[ $stype == float ]]; then
        if [[ $pop -eq 1 && $push -eq 1 ]]; then
          stype=unary
        elif [[ $pop -eq 2 && $push -eq 1 ]]; then
          stype=binary
        else
          print -u2 - "Cannot determine elemental for $key"
          return 1
        fi
      fi
      case $stype in
      (unary)
          cat <<UNARY
!!!_    * $elem ()
  ELEMENTAL &
  real(kind=KBUF) function $elem (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = X
    endif
  end function $elem
UNARY
          ;;
      (binary|lazy)
          cat <<BINARY
!!!_    * $elem()
  ELEMENTAL &
  real(kind=KBUF) function $elem (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = X + Y
    endif
  end function $elem
BINARY
          ;;
      (*) print -u2 - "unknown subroutine type $stype"; return 1;;
      esac
    done
  done
}

get_sub_name ()
{
  local __var=$1; shift
  local key=$1 pop=$2
  local func=(${(s:,:)FUNC[$key]})
  local __sub=$func[1]
  local ssfx=
  [[ ${__sub:--} == - ]] && __sub=$key
  if [[ $pop -eq 1 ]]; then
    ssfx=UNARY
  elif [[ $pop -eq 2 ]]; then
    if [[ $grp == lazy ]]; then
      ssfx=BINARY_lazy
    else
      ssfx=BINARY
    fi
  else
    print -u2 - "$key/$grp subroutine is not prepared"
    return 1
  fi
  __sub="apply_${ssfx}_${(U)__sub}"
  : ${(P)__var::=$__sub}
  return 0
}

get_elem_name ()
{
  local __var=$1; shift
  local key=$1
  local func=(${(s:,:)FUNC[$key]})
  local __elem=$func[1]
  [[ ${__elem:--} == - ]] && __elem=$key
  __elem="elem_${(U)__elem}"
  : ${(P)__var::=$__elem}
  return 0
}

fout ()
{
  local tab=2
  while [[ $# -gt 0 ]]
  do
    case $1 in
    (-t) tab=$2; shift;;
    (--) shift; break;;
    (*) break;;
    esac
    shift
  done
  if [[ $tab -eq 0 ]]; then
    tab=''
  else
    tab=$(printf "%${tab}s" ' ')
  fi
  print "$tab$@"
  return 0
}

output_table ()
{
  local grp= key=
  local nstack=() push= pop=
  local sym= alias= opt=
  local candi=(unary binary lazy ubool bool filter stack index float)
  local aapp=
  for grp in "$@"
  do
    [[ $candi[(I)$grp] -eq 0 ]] && continue
    for key in ${=GRP[$grp]}
    do
      [[ -n $ALIAS[$key] ]] && continue
      nstack=(${=DSTACK[$key]})
      [[ -z $nstack ]] && nstack=(${=NSTACK[$key]})
      pop="$nstack[1]" push="$nstack[2]"
      desc="$DESCR[$key]"

      sym=$SYM[$key]
      opt=$OPT[$key]

      if [[ $grp == index ]]; then
         pop=0 push=1
         [[ $grp == $SUBG[$key] ]] && opt=''
      fi

      alias=(${(k)ALIAS[(R)$key]})
      aapp=
      if [[ -n $alias && -n $opt ]]; then
        syms=($sym)
        aapp=T
      else
        syms=($sym $alias)
      fi
      [[ -n $opt ]] && syms=(${^syms}"[=$opt]")
      print - "| $syms | $pop | $push | $desc | "
      if [[ -n $aapp ]]; then
        [[ -n $opt ]] && syms=(${^alias}"[=$opt]")
        print - "| $syms | $pop | $push | (alias of $sym) $desc | "
      fi
    done

  done | sort | column -s '|' -o '|' -t
  print -

  candi=(anchor queue)
  for grp in "$@"
  do
    [[ $candi[(I)$grp] -eq 0 ]] && continue
    for key in ${=GRP[$grp]}
    do
      [[ -n $ALIAS[$key] ]] && continue
      desc="$DESCR[$key]"

      sym=$SYM[$key]
      alias=(${(k)ALIAS[(R)$key]})
      syms=($sym $alias)
      opt=$OPT[$key]
      [[ -n $opt ]] && syms=(${^syms}"[=$opt]")
      print - "| $syms | $desc | "
    done | sort | column -s '|' -o '|' -t
    print -
  done

  candi=(buffer header)
  for grp in "$@"
  do
    [[ $candi[(I)$grp] -eq 0 ]] && continue
    for key in ${=GRP[$grp]}
    do
      [[ -n $ALIAS[$key] ]] && continue
      desc="$DESCR[$key]"
      sym=$SYM[$key]
      alias=(${(k)ALIAS[(R)$key]})
      syms=($sym $alias)
      opt=$OPT[$key]
      param=$PARAM[$key]
      if [[ -n $opt ]]; then
         syms=(${^syms}"[=ARG]")
         arg="$opt"
      elif [[ -n $param ]]; then
         syms=(${^syms}"=ARG")
         arg="$param"
      fi
      print - "| $syms | $arg | $desc | "
    done | sort | column -s '|' -o '|' -t
    print -
  done
}

output_f90_header ()
{
  local base=$1 desc="$2"
  local date=$(date -Iseconds)
  base=${base#./}
  cat <<HEADER
!!!_! $base - TOUZA/Jmz CH(swiss) army knife $desc
! Maintainer: SAITO Fuyuki
! Created by $this at $date
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
HEADER
  return 0
}

output_list ()
{
  local grp= key=
  local sym= list=()
  local candi=(unary binary lazy ubool bool filter stack index float)

  for grp in "$@"
  do
    [[ $candi[(I)$grp] -eq 0 ]] && continue
    list=()
    for key in ${=GRP[$grp]}
    do
      sym=$SYM[$key]
      list+=($sym)
    done
    if [[ -n $list ]]; then
      print - "# $grp"
      print - "OPRS_$grp='$list'"
    fi
  done
  print -

}

main "$@"; err=$?
exit $err
