#! /bin/zsh
###########################################################
## locate: ${XDG_CONFIG_HOME}/herbstluftwm/dzen-post.zsh ##
## author: Vincent Z (github.com/milomouse)              ##
## detail: hlwm specific tag(s) information for `dzen2'  ##
###########################################################
## NOTE 1: relies on `herbstluftwm' to be up and running ##
## NOTE 2: script is dynamic and only updates on changes ##
## NOTE 3: able to be reloaded by `herbstclient reload'  ##
### dependencies: #########################################
##    HERBSTLUFTWM                    (herbstluftwm.org) ##
##      `> HERBSTCLIENT                                  ##
## of course, you need ZSH and DZEN2 to exec this script ##
###########################################################

herbstclient getenv DISPLAY &>/dev/null || exit 1
source ${XDG_CONFIG_DIR:-$HOME}/herbstluftwm/dzen-colors.zsh

herbstclient --idle | while read i ; do
  if [[ $i == complete || $i =~ tag_ ]]; then
    tags=( $(herbstclient tag_status) ) || exit 2
    cnum="${(w)#${${${${"$(herbstclient stack)"}/*Normal Layer}/Frame Layer*}//[[:punct:][:blank:]]}}"
    case ${#cnum} {
      2) cnum=("^fg(#616161)${cnum[1,2]}") ;;
      1|0) cnum=("#^fg(#616161)${cnum}") ;;
      *) cnum=(">=") ;;
    }
    for i ( $tags ) {
      case ${i[1]} {
        '#') tags=("${tags/$i/${c_00}${b_07}${i#[[:graph:]]}${b_08} }") ;;
        '+') tags=("${tags/$i/^fg(#707070)${i#[[:graph:]]} }") ;;
        '%') tags=("${tags/$i/${c_04}${i#[[:graph:]]} }") ;;
        '-') tags=("${tags/$i/^fg(#707070)${i#[[:graph:]]} }") ;;
        '.') tags=("${tags/$i/^fg(#707070)${i#[[:graph:]]} }") ;;
        ':')
          _n=(${#${(M)${$(herbstclient layout ${i[2,-1]})}#0[[:alpha:]]}})
          case ${_n} {
            0) _N=' ' ;;
            1) _N='¹' ;;
            2) _N='²' ;;
            3) _N='³' ;;
            4) _N='⁴' ;;
            *) _N='֡' ;; #'keepforsyntax
          }
          tags=("${tags/$i/${c_fg}${i#[[:graph:]]}^fg(#696969)${_N}}")
        ;;
        '!') tags=("${tags/$i/${c_01}${i#[[:graph:]]} }") ;;
        *) tags=("${tags/$i/^fg(#707070)${i#[[:graph:]]} }") ;;
      }
    }
    print "${_XX}${b_08} ${c_XX}${b_00}${c_07} ɦerbstluftwm ${b_08} ${c_XX}^bg(#2a2a2a)${tags:-${c_08}$(repeat 12 { printf " · " })}^bg(#292929)${c_08} ${cnum} ${_XX}"
  elif [[ $i =~ quit || $i =~ reload ]]; then
    kill $!
    exit
  fi
done | dzen2 -p -x 5 -y 0 -w 332 -h 16 -ta l -bg ${_bg} -fg ${_fg} \
       -fn '-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1' \
       &>/dev/null || exit 5
