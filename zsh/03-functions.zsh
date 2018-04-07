# 03-functions.zsh

trash() {
  local piece

  if (( $# == 1 )) && [[ $1 == 'dump' ]] then
    rm -rf $TRASH_DIR/*
    return 0
  elif (( $# == 0 )) then
    print - 'trash: usage: `trash file [ ...file ]'\'
    print - '              OR `trash dump'\'
    return 0
  elif [[ ! -d $TRASH_DIR || ! -O $TRASH_DIR ]] then
    print - 'trash: go define $TRASH_DIR' &>2
    return 1
  fi

  for piece in $@; do
    if [[ ! -O $piece ]] then
      print - "trash: invalid piece of trash: $piece" &>2
      return 1
    fi
  done

  while (( $# )); do
    mv -f $1 $TRASH_DIR
    shift
  done
}

