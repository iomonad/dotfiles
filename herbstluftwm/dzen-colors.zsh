#! /bin/zsh
#############################################################
## locate: ${XDG_CONFIG_HOME}/herbstluftwm/dzen-colors.zsh ##
## author: Vincent Z (github.com/milomouse)                ##
## detail: sourced X color settings converted for `dzen2'  ##
#############################################################
## NOTE 1: variable must be set to a valid X settings file ##
## NOTE 2: this script will fail if no *colors are defined ##
#############################################################

XF="${XDG_CONFIG_HOME:-$HOME}/xorg/Xresources"

if [[ -s ${XF} ]]; then
  c_XX='^fg()'
  b_XX='^bg()'
  _XX=${c_XX}${b_XX}
  <${XF} | grep "^*" | while read c ; do
    case "${${(s. .)c:l}[1]}" {
      #'*background:') _bg=${${(s. .)c}[-1]} ; c_bg=^fg(${_bg}) ; b_bg=^bg(${_bg}) ;;
      '*background:') _bg='#191919' ; c_bg=^fg(${_bg}) ; b_bg=^bg(${_bg}) ;;
      '*foreground:') _fg=${${(s. .)c}[-1]} ; c_fg=^fg(${_fg}) ; b_fg=^bg(${_fg}) ;;
      '*color0:') _00=${${(s. .)c}[-1]} ; c_00=^fg(${_00}) ; b_00=^bg(${_00}) ;;  ## black
      '*color8:') _08=${${(s. .)c}[-1]} ; c_08=^fg(${_08}) ; b_08=^bg(${_08}) ;;  ## black:bold
      '*color1:') _01=${${(s. .)c}[-1]} ; c_01=^fg(${_01}) ; b_01=^bg(${_01}) ;;  ## red
      '*color9:') _09=${${(s. .)c}[-1]} ; c_09=^fg(${_09}) ; b_09=^bg(${_09}) ;;  ## red:bold
      '*color2:') _02=${${(s. .)c}[-1]} ; c_02=^fg(${_02}) ; b_02=^bg(${_02}) ;;  ## green
      '*color10:') _10=${${(s. .)c}[-1]} ; c_10=^fg(${_10}) ; b_10=^bg(${_10}) ;; ## green:bold
      '*color3:') _03=${${(s. .)c}[-1]} ; c_03=^fg(${_03}) ; b_03=^bg(${_03}) ;; ## yellow
      '*color11:') _11=${${(s. .)c}[-1]} ; c_11=^fg(${_11}) ; b_11=^bg(${_11}) ;; ## yellow:bold
      '*color4:') _04=${${(s. .)c}[-1]} ; c_04=^fg(${_04}) ; b_04=^bg(${_04}) ;; ## blue
      '*color12:') _12=${${(s. .)c}[-1]} ; c_12=^fg(${_12}) ; b_12=^bg(${_12}) ;; ## blue:bold
      '*color5:') _05=${${(s. .)c}[-1]} ; c_05=^fg(${_05}) ; b_05=^bg(${_05}) ;; ## magenta
      '*color13:') _13=${${(s. .)c}[-1]} ; c_13=^fg(${_13}) ; b_13=^bg(${_13}) ;; ## magenta:bold
      '*color6:') _06=${${(s. .)c}[-1]} ; c_06=^fg(${_06}) ; b_06=^bg(${_06}) ;; ## cyan
      '*color14:') _14=${${(s. .)c}[-1]} ; c_14=^fg(${_14}) ; b_14=^bg(${_14}) ;; ## cyan:bold
      '*color7:') _07=${${(s. .)c}[-1]} ; c_07=^fg(${_07}) ; b_07=^bg(${_07}) ;; ## white
      '*color15:') _15=${${(s. .)c}[-1]} ; c_15=^fg(${_15}) ; b_15=^bg(${_15}) ;; ## white:bold
    }
  done
else
  exit 3
fi
