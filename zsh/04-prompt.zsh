# 04-prompt.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# Requirements
autoload colors; colors;

setopt multios

if [[ ${WINDOW} != x ]]
then
    SCREEN_NO="%B$WINDOW%b "
else
    SCREEN_NO=""
fi

# VCS

autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats '%F{yellow}(%b) '

# Prompt
PROMPT=' %B$(kube_ps1)%F %B${vcs_info_msg_0_}%F{red}» %f'
RPROMPT='%B%F{dark_grey}%~ %B%F{white}%#'
setopt prompt_subst

# Correction Prompt
autoload -U colors && colors
export SPROMPT="$fg[cyan]Correct $fg[red]%R$reset_color $fg[magenta]to $fg[green]%r?$reset_color ($fg[white]YES :: NO :: ABORT :: EDIT$fg[white])"
