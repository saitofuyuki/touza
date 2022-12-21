#!/usr/bin/zsh -f
# Time-stamp: <2022/12/20 21:29:56 fuyuki genvar.sh>

main ()
{
  local ovw=

  while [[ $# -gt 0 ]]
  do
    case $1 in      
    (-o) ovw=$1;;
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

  for sub in "$@"
  do
    bpat="^[^!]*\\(subroutine\\|function\\) *$sub"
    epat="^[^!]*end .*\\(subroutine\\|function\\) *$sub"

    sed -ne "/$bpat/,/$epat/p" < $file > $src

    # l
    karg=KARG
    rep=${sub%i}l
    sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
        -e "s/\\($karg *= *\\)KI32/\\1KI64/" \
        < $src > $mod
    # f
    rep=${sub%i}f
    sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
        -e "s/\\($karg *= *\\)KI32/\\1KFLT/" \
        -e "s/^\\( *\\)integer\\((KIND=$karg),\\)/\\1real\\2   /" \
        < $src >> $mod
    # d
    rep=${sub%i}d
    sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
        -e "s/\\($karg *= *\\)KI32/\\1KDBL/" \
        -e "s/^\\( *\\)integer\\((KIND=$karg),\\)/\\1real\\2   /" \
        < $src >> $mod
    # a
    rep=${sub%i}a
    sed -e "1s/$sub/$rep/" -e "\$s/$sub/$rep/" \
        -e "s/^\\( *\\)integer\\((KIND=$karg),\\)/\\1character(len=*),  /" \
        < $src >> $mod

    # output
    end=${sub%i}a
    epat="^[^!]*end .*\\(subroutine\\|function\\) *$end"
    {
      sed -e "/$bpat/,\$d" $file
      cat $src
      cat $mod
      sed -e "1,/$epat/d" $file
    } > $tmp
    if [[ -n $ovw ]]; then
      mv $file $file.org
      mv $tmp $file
    else
      cat $tmp
    fi
  done

  rm -f $src $mod $tmp
  return 0
}

main "$@"; err=$?
exit $err
