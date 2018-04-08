# 04-prompt.zsh

PROMPT='%B%F{red}>> %f'
RPROMPT='%B%F{black}%~ %B%F{white}%#'
# Color command correction promt
autoload -U colors && colors
export SPROMPT="$fg[cyan]Correct $fg[red]%R$reset_color $fg[magenta]to $fg[green]%r?$reset_color ($fg[white]YES :: NO :: ABORT :: EDIT$fg[white])"
