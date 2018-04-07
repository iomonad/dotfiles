# 01-options.zsh

zmodload zsh/stat
zmodload zsh/complist
autoload zmv
autoload -U colors && colors
autoload -U vcs_info && vcs_info
autoload -U compinit && compinit -i
autoload -U zmv
setopt extended_glob
setopt prompt_subst
setopt autocd
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

export HISTSIZE=1000
export SAVEHIST=1000
export HISTFILE=$HOME/.zsh/history
export DISPLAY=:0
