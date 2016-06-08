#!/usr/bin/zsh

local -a MPLAYER VIM FEH
# For Vim {{{
VIM=(
  asm
  awk
  bash
  c
  cfg
  coffee
  conf
  cpp
  cs
  css
  csv
  diff
  enc
  eps
  etx
  ex
  example
  git
  gitignore
  go
  h
  hs
  htm
  html
  info
  ini
  java
  jhtm
  js
  jsm
  jsm
  json
  jsp
  lisp
  log
  lua
  map
  markdown
  md
  mf
  mfasl
  mi
  mkd
  mtx
  nfo
  pacnew
  patch
  pc
  pfa
  php
  pid
  PKGBUILD
  pl
  PL
  pm
  pod
  py
  rb
  rdf
  ru
  sed
  sfv
  sh
  signature
  sty
  sug
  t
  tcl
  tdy
  tex
  textile
  tfm
  tfnt
  theme
  txt
  urlview
  vim
  viminfo
  xml
  yml
  zsh
)
# }}}
# For Mplayer {{{
MPLAYER=(
  avi
  cue
  dat
  fcm
  flac
  flv
  m3u
  m4
  m4a
  m4v
  mkv
  mov
  mp3
  mp4
  mpeg
  MOV
  mpg
  ogg
  ogm
  ogv
  rmvb
  sample
  spl
  ts
  wmv
)
#}}}
# For Feh {{{
FEH=(
  bmp
  cdr
  gif
  ico
  jpeg
  jpg
  JPG
  png
  svg
  xpm
)
#}}}

local MPLAYER_CONFIG=$HOME/.mplayer/config

alias -s $^VIM='vim -X --'
alias -s $^MPLAYER='mplayer -msgmodule 1 -msgcolor -include $MPLAYER_CONFIG --'
alias -s $^FEH='feh -Fzrd --'

alias -s se='ssh'


# vim: set ts=2 expandtab sw=2 ft=sh:
