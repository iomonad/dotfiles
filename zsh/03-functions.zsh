# 03-functions.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

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


sandbox-shell () {
    sudo nsenter -t $(pgrep urxvtd) \
	 -m -u -i -n \
	 -p /bin/bash
}

# FFMPEG Conversions

function convert_to_webm_shrinked() {
    ffmpeg -i $1 -s 568x320 \
	   -vcodec libvpx \
	   -b:v 1100k \
	   -acodec libvorbis \
	   -b:a 192k \
	   $2.webm
}


function audio_and_img_to_yt() {
    ffmpeg -loop 1 -i $1 \
	   -i $2 -c:v libx264 \
	   -tune stillimage \
	   -c:a aac -b:a 192k \
	   -pix_fmt yuv420p \
	   -shortest output.mkv
}

# High level lisp loading

function lisp() {
    sbcl --noinform \
	 --script "$@"
}

# Synchronize folders

function fs_sync () {

    local sourcef=$1
    local dest=$2

    rsync --info=progress2 \
	  --ignore-existing \
	  -auvz \
	  ${sourcef} \
	  ${dest}
}

function normalize_utf8() {
    local buffer=$1

    iconv -f utf-8 -t ascii//TRANSLIT <<< ${buffer}
}


# Normalize Analog Scan Folder

function normalize_analog_scan() {
    local count=1

    for i in *.jpg; do
	mv ${i} analog-${(l:3::0:)count};
	echo "[*] ${(l:10::0:)value}"
	((count++))
    done
}

function ansi-cat () {
    iconv -f CP437 $@
}

function recursive-replace () {
    local search=$1
    local replace=$2

    grep -rl $search . | xargs sed -i s/${search}/${replace}/g
}

function kube-force-delete-ns () {
	(
	NAMESPACE=$1
	kubectl proxy &
	kubectl get namespace $NAMESPACE -o json |jq '.spec = {"finalizers":[]}' >temp.json
	curl -k -H "Content-Type: application/json" -X PUT --data-binary @temp.json 127.0.0.1:8001/api/v1/namespaces/$NAMESPACE/finalize
	)
}
