# 01-options.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# Load Completions
fpath=(~/.zsh/completion $fpath)

# Load ZSH Modules
zmodload zsh/stat
zmodload zsh/complist

# Autoload Features
autoload zmv
autoload -U colors && colors
autoload -U vcs_info && vcs_info
autoload -U compinit && compinit -i
autoload -U zmv

# Set features
setopt extended_glob
setopt prompt_subst
setopt noclobber
setopt correct
setopt no_case_glob
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt autocd auto_pushd pushdignoredups

# Remove Features
unsetopt beep

# ZSH related configuration
export HISTSIZE=10000
export SAVEHIST=1000
export HISTFILE=$HOME/.zsh/history

# spectrum
# A script to make using 256 colors in zsh less painful.

typeset -AHg FX FG BG

FX=(
  reset     "%{[00m%}"
  bold      "%{[01m%}" no-bold      "%{[22m%}"
  italic    "%{[03m%}" no-italic    "%{[23m%}"
  underline "%{[04m%}" no-underline "%{[24m%}"
  blink     "%{[05m%}" no-blink     "%{[25m%}"
  reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
  FG[$color]="%{[38;5;${color}m%}"
  BG[$color]="%{[48;5;${color}m%}"
done

# Show all 256 colors with color number
function spectrum_ls() {
  setopt localoptions nopromptsubst
  local ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}
  for code in {000..255}; do
    print -P -- "$code: ${FG[$code]}${ZSH_SPECTRUM_TEXT}%{$reset_color%}"
  done
}

# Show all 256 colors where the background is set to specific color
function spectrum_bls() {
  setopt localoptions nopromptsubst
  local ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}
  for code in {000..255}; do
    print -P -- "$code: ${BG[$code]}${ZSH_SPECTRUM_TEXT}%{$reset_color%}"
  done
}
