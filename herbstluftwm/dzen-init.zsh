#! /bin/zsh
###########################################################
## locate: ${XDG_CONFIG_HOME}/herbstluftwm/dzen-init.zsh ##
## author: Vincent Z (github.com/milomouse)              ##
## detail: wm-independent information for `dzen2'        ##
###########################################################
## NOTE 1: script should be ran from the "xinitrc" file  ##
## NOTE 2: needs to be started -before- `herbstluftwm'   ##
## NOTE 3: will NOT be reloaded by `herbstclient reload' ##
### dependencies: #########################################
##    i_mifo() == MIFO       (github.com/milomouse/mifo) ##
##    i_pvol() == PULSEVOL   (^^^^^^/milomouse/pulsevol) ##
##    i_mail() == [none]                                 ##
##    i_load() == [none]                                 ##
##    i_sbat() == [none]                                 ##
##    i_date() == DATE                       [coreutils] ##
###########################################################
## of course, you need ZSH and DZEN2 to exec this script ##
###########################################################

source ${XDG_CONFIG_DIR:-$HOME}/herbstluftwm/dzen-colors.zsh

## FUNCTIONS:
function i_mifo {
  m_=$(mifo -a)           ; [[ ${#m_} -eq 0 ]] && m_="/"
  m_B=${${m_:t:r}//_/ }   ; [[ ${#m_B} -eq 0 ]] && m_B='<unknown>'
  m_A=${${m_:h:h:t}//_/ } ; [[ ${#m_A} -eq 0 ]] && m_A='<unknown>'
  m_D=${${m_:h:t}//_/ }   ; [[ ${#m_D} -eq 0 ]] && m_D='<unknown>'

  T=60 ; t=${T} ; F="${m_B}${m_A}${m_D}"
  ac=${m_A[1,${T}]} ; dc=${m_D[1,${T}]} ; bc=${m_B[1,${T}]}

  # dynamically subtract total output until value <= ${T}
  until [[ ${#F} -le ${T} ]] {
    t=$(( ${t} - 1 ))
    case $x { ; m_D) x=m_B ;; ; m_B) x=m_A ;; ; *) x=m_D ;; ; }
    if [[ ${#${(P)x}} != ${#${${(P)x}[1,$((${t}-1))]% }} ]] {
      typeset ${x}="${${(P)x}[1,$((${t}-2))]}.."
    } else { typeset ${x}=${(P)x}
  } ; F="${m_B}${m_A}${m_D}" ; }

  print - "${c_XX}${b_08} ^bg(#333333)${c_07} ӎplayer2 ${c_08}${b_00}▒${_XX}\
$(mifo -a ${b_08} '^bg(#303030)'${c_12} ${m_A:-%D:2:} ${_XX}\
'^bg(#292929)^fg(#616161)' ${m_D:-%D} ${_XX}\
'^bg(#242424)'${c_13} ${m_B:-%B} ${_XX}\
'^bg(#292929)'${c_08} %e ${_XX}\
'^bg(#333333)'${c_04} %c ${c_XX}/ ${c_12}%C ${_XX})"
}

function i_pvol {
  VOLUME="${${$(pulsevol -a volume)/\//${c_07}ʆ^fg(#666666)}:gs/%/^fg(#484848)&}"
  MUTE="${${$(pulsevol -a mute)/yes/${c_01}}/no/${c_13}}•"
  print - "${c_XX}${b_08} ${c_07}^bg(#333333) ⩗olume ${_XX}\
^bg(#292929)^fg(#666666) ${VOLUME} ${_XX}\
^bg(#333333) ${MUTE} ${_XX}"
}

function i_mail {
  setopt NOnomatch
  ALL=(/howl/mail/*/*/new/*(D.om))
  if [[ $ALL == '/howl/mail/*/*/new/*(D.om)' ]] { unset ALL
  } else { unset N
    print ${(F)ALL} | while { read i } {
      if [[ ${#i} != ${#${i:l}/inbox} ]] { N=$((${N:-0} + 1)) }
  };}
  print - "${c_XX}${b_08} ^bg(#333333)${c_07} ӎailbox ${c_08}${b_00}▒${_XX}${b_08} \
^bg(#292929)^fg(#494949) unread ${_XX}\
^bg(#242424)${c_13} ${N:-0} ${_XX}\
^bg(#292929)^fg(#494949) $((${#ALL} - ${N:-0})) ${_XX}"
}

function i_load {
  LOAD=${${${${${${${${(s. .)$(</proc/loadavg)}[1]/0./${c_08}0.}/1./${c_07}1.}/2./${c_05}2.}/3./${c_04}3.}/4./${c_03}4.}/5./${c_11}5.}//./${c_13}.^fg(#616161)}
  print - "${c_XX}${b_08} ${c_07}^bg(#333333) ɭoadavg ${_XX}\
^bg(#292929)${c_01} ${LOAD} ${_XX}"
}

function i_sbat {
  D=/sys/class/power_supply/BAT0
  if [[ ! -d $D ]] { STATE="^fg(#444444)∼" ; CHARGE=' - '
  } else { STATE=${$(<${D}/status):l} ; CHARGE=$(<${D}/capacity) }
  STATE=${${${${STATE/full/${c_12}✓}/discharging/${c_01}✗}/charging/${c_03}⚡}/unknown/^fg(#444444)∼}
  BAT="${b_08} ^bg(#292929) ${STATE} ${b_08}${c_07} ${CHARGE}^fg(#666666)%"
  print - "${c_XX}${b_08} ${c_07}^bg(#333333) Ϧattery ${c_08}${b_00}▒${_XX}${BAT} ${_XX}"
}

function i_date {
  date "+${c_XX}${b_08} ${c_07}${_XX}\
^bg(#292929) ^fg(#616161)%b %d ${_XX}\
^bg(#333333) ^fg(#585858)%a ${_XX}\
${b_08} ${c_07}%H:%M ${_XX}"
}

## RUNTIME:
_font='-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1'
_wide='1260'
function left { print - "$(i_mifo) $(i_pvol)" }
function right { print - "$(i_mail) $(i_load) $(i_sbat) $(i_date)${_XX} "}

while true ; do
  _rwide=$(( $(print - ${#${"$(right | sed 's.\^[^(]*([^)]*)..g')"}}) * 6 - 5))
  print - "$(left)^pa($((${_wide} - ${_rwide})))$(right)"
  sleep 1s
done | dzen2 -p -x 337 -y 0 -h 16 -w ${_wide} -ta l -bg ${_bg} -fg ${_fg} \
       -fn '-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1' \
       &>/dev/null || exit 5
