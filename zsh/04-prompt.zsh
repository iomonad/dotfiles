# 04-prompt.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# Requirments
autoload colors; colors;

setopt multios

if [[ ${WINDOW} != x ]]
then
    SCREEN_NO="%B$WINDOW%b "
else
    SCREEN_NO=""
fi

# Prompt
PROMPT=' %B%F{red}» %f'
RPROMPT='%B%F{black}%~ %B%F{white}%#'
setopt prompt_subst

# Correction Prompt
autoload -U colors && colors
export SPROMPT="$fg[cyan]Correct $fg[red]%R$reset_color $fg[magenta]to $fg[green]%r?$reset_color ($fg[white]YES :: NO :: ABORT :: EDIT$fg[white])"
