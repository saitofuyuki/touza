#!/usr/bin/zsh -f
# Time-stamp: <2022/09/30 12:51:44 fuyuki genopr.sh>

typeset -A STR=() KEY=() OPTS=()

typeset -A GRP=()
typeset -A STACK=()

apfx=str_
ipfx=opr_
gpfx=grp_
epfx=elem_

main ()
{
  local call=("$@")

  [[ -z $call ]] && call=(int str reg app)

  register_all || return $?

  local GODR=(system output anchor stack queue unary bool binary lazy other)
  GODR=($GODR ${(k)GRP})
  GODR=(${(u)GODR})

  local grp= key=

  if [[ -n ${(M)call:#s*} ]]; then
    for grp in $GODR
    do
      fout "!! $grp"
      for key in ${=GRP[$grp]}
      do
        output_str $key
      done
    done
  fi

  if [[ -n ${(M)call:#i*} ]]; then
    local jnum=0
    for grp in $GODR
    do
      fout "integer,parameter :: $gpfx${grp}_bgn = $jnum"
      for key in ${=GRP[$grp]}
      do
        output_int $key $jnum
        let jnum++
      done
      fout "integer,parameter :: $gpfx${grp}_end = $jnum"
      print -
    done
  fi

  if [[ -n ${(M)call:#r*} ]]; then
    for grp in $GODR
    do
      for key in ${=GRP[$grp]}
      do
        output_reg $key
      done
    done
  fi

  if [[ -n ${(M)call:#a*} ]]; then
    for grp in $GODR
    do
      for key in ${=GRP[$grp]}
      do
        output_apply $grp $key
      done
    done
  fi
}

# register KEY=PARAM/STRING     [GROUP,]POP,PUSH[,OPT...]
# register KEY[=PARAM]/STRING
# register KEY[=PARAM]/-
register ()
{
  local c=()
  local key=$1; shift
  local opts= str=
  c=(${(s:/:)key})
  str=$c[2] opts=$c[1]
  key=${opts%%[][=]*}
  opts=${opts%$key}
  if [[ -z $str ]]; then
    str="$key"
  elif [[ $str == - ]]; then
    str=" $key"
  fi

  local stack=$1; shift
  stack=(${(s:,:)stack})
  local prop=$stack[1]
  # print - $prop ${prop[(I)[0-9]]}
  if [[ ${prop[(I)[0-9]]} -eq 0 ]]; then
    shift stack
  else
    if [[ $stack[1] -eq 2 && $stack[2] -eq 1 ]]; then
      prop='binary'
    elif [[ $stack[1] -eq 1 && $stack[2] -eq 1 ]]; then
      prop='unary'
    else
      prop='other'
    fi
  fi
  GRP[$prop]+=" $key"
  OPTS[$key]="$opts"
  STR[$key]="$str"
  STACK[$key]="$stack"
  # print - "## register $prop/$key [$str]($stack)$opts"
}

register_all ()
{
  # output
  register OUTPUT_POP/'='    output,1,0
  register OUTPUT_KEEP/':='  output,1,1

  # system
  register INPUT/-    system,1,1
  register OUTPUT/-   system,1,0
  register ANCHOR/-   system

  # anchor management
  register MARK   anchor    'fragile anchor (removed by first touch)'
  register STOP   anchor    'robust anchor (removed by GO)'
  register GO     anchor    'remove topmost anchor'

  # stack manipulation
  register DUP          stack,1,2  'duplicate top stack'
  register 'POP[=name]' stack,1,0  'discard top stack and optinally tag'
  register EXCH         stack,2,2  'B A; exchange two top stacks'
  register NOP          stack,0,0  'no operation; do nothing'
  register DIST         stack      'distribute top stack for every stack from anchor'
  register INSERT       stack      'move top stack after anchor'
  register REPEAT       stack      'repeat from anchor (including anchor)'
  register FLUSH        stack      'flush-out from anchor'

  # queue manipulation
  register ITER queue  'iterate last queue operator for each set from anchor'
  register CUM  queue  'apply last queue non-unary operator from anchor'
  register MAP  queue  'reserved; DIST ITER'

  # logical operation
  register AND   2,1   'logical and; B if both A and B are defined, else UNDEF'
  register MASK  2,1   'A if both A and B are defined, else UNDEF'

  # logical unary
  register NOT   bool,1,1   'logical not; 1 if undefined, else UNDEF'
  register BOOL  bool,1,1   'boolean; 1 if defined, else UNDEF'
  register BIN   bool,1,1   'binary; 1 if defined, else 0'

  # logical operation, inclusive
  register OR     lazy,2,1  'logical or; A if defined, else B if defined, else UNDEF'
  register XOR    lazy,2,1  'logical exclusive-or; A or B if B or A undefined, else UNDEF'
  register LMASK  lazy,2,1,-,MASK  'lazy MASK'

  # primitive binary
  register ADD  2,1    'A+B'  '+'
  register SUB  2,1    'A-B'  '-'
  register MUL  2,1    'A*B'  '*'
  register DIV  2,1    'A/B'  '/'
  register IDIV 2,1    'A//B' '//'
  register MOD  2,1    'mod(A,B)'  '%'
  register POW  2,1    'pow(A,B)'  '**'

  # primitive binary inclusive
  register LADD lazy,2,1,ZERO,ADD    'lazy ADD'  '+'
  register LSUB lazy,2,1,ZERO,SUB    'lazy SUB'  '-'
  register LMUL lazy,2,1,ONE,MUL     'lazy MUL'  '*'
  register LDIV lazy,2,1,ONE,DIV     'lazy DIV'  '/'

  # primitive unary
  register NEG   1,1   '-A'     '-@'
  register INV   1,1   '1/A'    '1/@'
  register ABS   1,1   'abs(A)'
  register SQR   1,1   'A*A'
  register SQRT  1,1   'square root'
  register SIGN  1,1   'copy A sign on 1'
  register ZSIGN 1,1   '-1,0,+1 if negative,zero,positive'

  # integer opration
  register FLOOR  1,1  'largest integer <= A'
  register CEIL   1,1  'smallest integer >=A'
  register ROUND  1,1  'nearest integer of A'
  register TRUNC  1,1  'truncate toward 0'
  register INT    1,1,TRUNC  'truncate toward 0 and convert'

  # math operation
  register EXP   1,1   'exp(A)'
  register LOG   1,1   'log(A)'
  register LOG10 1,1   'log10(A)'
  register SIN   1,1   'sin(A)'
  register COS   1,1   'cos(A)'
  register TAN   1,1   'tan(A)'
  register TANH  1,1   'tanh(A)'
  register ASIN  1,1   'arcsin(A)'
  register ACOS  1,1   'arccos(A)'
  register ATAN2 2,1   'arctan(A/B)'

  # floating-point operation
  register EXPONENT 1,1 'exponent(A)'
  register FRACTION 1,1 'fraction(A)'
  register SCALE    2,1 'scale(A,B)'

  # other operation
  register MIN   2,1   'min(A,B)'
  register MAX   2,1   'max(A,B)'
  register LMIN  lazy,2,1,ULIMIT   'lazy MIN'
  register LMAX  lazy,2,1,LLIMIT   'lazy MAX'

  # conditional operation (binary)
  register EQ   bool,2,1     '1 if A==B, else 0'
  register NE   bool,2,1     '1 if A!=B, else 0'
  register LT   bool,2,1     '1 if A<B, else 0'
  register GT   bool,2,1     '1 if A>B, else 0'
  register LE   bool,2,1     '1 if A<=B, else 0'
  register GE   bool,2,1     '1 if A>=B, else 0'

  # conditional operation (binary or undef)
  register EQU  bool,2,1     '1, 0, UNDEF for A==B, not, either UNDEF'
  register NEU  bool,2,1     '1, 0, UNDEF for A!=B, not, either UNDEF'
  register LTU  bool,2,1     '1, 0, UNDEF for A<B, not, either UNDEF'
  register GTU  bool,2,1     '1, 0, UNDEF for A>B, not, either UNDEF'
  register LEU  bool,2,1     '1, 0, UNDEF for A<=B, not, either UNDEF'
  register GEU  bool,2,1     '1, 0, UNDEF for A>=B, not, either UNDEF'

  # conditional operation (filter)
  register EQF   2,1    'A if A==B, else UNDEF'
  register NEF   2,1    'A if not A==B, else UNDEF'
  register LTF   2,1    'A if A<B, else UNDEF'
  register GTF   2,1    'A if A>B, else UNDEF'
  register LEF   2,1    'A if A<=B, else UNDEF'
  register GEF   2,1    'A if A>=B, else UNDEF'

  # reduction operation
  register AVR   reduction   'arithmetic mean from the anchor to the top stack'
  register COUNT reduction   'count defined elements from the anchor to the top stack'

  # transform operation
  # register UNDEF transf,1,1   'UNDEF value on top stack (scalar)'
  #### coor=0,1,2,name,alias for coodinate, -1 or s for stack
  # register COUNT=COOR
  # register AVR=COOR
  # register MEAN=COOR
  # register HIGH=COOR
  # register LOW=COOR

  # property manipulation
  register 'TAG=name'         buffer
  register 'COOR=c0,c1,c2'    buffer
  register 'C0=low:high'      buffer
  register 'C1=low:high'      buffer
  register 'C2=low:high'      buffer
  register 'C3=low:high'      buffer
  register 'X=low:high'       buffer
  register 'Y=low:high'       buffer
  register 'Z=low:high'       buffer
  register 'LON=low:high'     buffer
  register 'LAT=low:high'     buffer
  register 'LEV=low:high'     buffer

  register 'DFMT=format'    header

  register 'ITEM=string'    header
  register 'UNIT=string'    header
  register 'TITLE=string'   header
  register 'EDIT=string'    header
  register 'TSEL=list'/T    header

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
  tab=$(printf "%${tab}s" ' ')
  print "$tab$@"
  return 0
}


output_str ()
{
  local op=$1
  str=$STR[$op]
  fout "character(len=*),parameter :: $apfx$op = '$str'"
}
output_int ()
{
  local op=$1 val=$2
  fout "integer,parameter :: $ipfx$op = $2"
}

output_reg ()
{
  local op=$1
  local iopr=$ipfx$op
  local aopr=$apfx$op
  local stack=(${=STACK[$op]})
  if [[ -z $stack ]]; then
    fout -t 4 "if (ierr.eq.0) call reg_opr_prop(ierr, $iopr, $aopr)"
  else
    fout -t 4 "if (ierr.eq.0) call reg_opr_prop(ierr, $iopr, $aopr, $stack[1], $stack[2])"
  fi
}

output_apply ()
{
  local grp=$1; shift
  local op=$1
  local iopr=$ipfx$op
  local h='handle'
  local pre="ierr, handle, lefth(1:push), righth(1:pop)"
  local post=
  local afunc=
  local flags=(${=STACK[$op]})
  local esym=$op
  local pop=$flags[1] push=$flags[2]

  # print - "$op $flags"
  case $grp in
  (unary)   afunc=apply_elem_UNARY
            [[ -n ${flags[3]} ]] && esym=$flags[3]
            ;;
  (binary)  afunc=apply_elem_BINARY;;
  (lazy)    afunc=apply_elem_BINARY_lazy
            [[ ${flags[3]:--} == - ]] || post=", $flags[3]"
            [[ -n ${flags[4]} ]] && esym=$flags[4]
            ;;
  (bool)    if [[ $push -eq 1 && $pop -eq 1 ]]; then
              afunc=apply_elem_UNARY
            elif [[ $push -eq 1 && $pop -eq 2 ]]; then
              afunc=apply_elem_BINARY
            else
              print -u2 - "Operator $op ($push $pop) is not prepared."
              return 1
            fi
            post=', .TRUE.'
            ;;
  esac

  if [[ -n $afunc ]]; then
    fout -t 7  "else if ($h.eq.$iopr) then"
    fout -t 10 "call $afunc($pre, ${epfx}$esym$post)"
  fi
}


main "$@"; err=$?
exit $err
