# The shell options {{{

# Loading plugins {{{
autoload -U compinit promptinit zsh-mime-setup colors
compinit
promptinit
zsh-mime-setup
colors

# ZLE
# Using:
# url-quote-magic -> Make url write easy
# predict-on -> history prediction
insert_sudo () { zle beginning-of-line; zle -U "sudo " }

autoload -Uz url-quote-magic predict-on
zle -N self-insert url-quote-magic
zle -N predict-on

zle -N insert-sudo insert_sudo
bindkey "^X" insert-sudo # Map alt x to sudo
#}}}

# Options {{{
# why type cd dir, just type dir
setopt AUTO_CD
# No unnecessary slashes
setopt AUTO_REMOVE_SLASH
# pipe to multiple outputs
setopt MULTIOS
# spell check commands
setopt CORRECT
# expand glos when possible
setopt GLOB_COMPLETE
setopt NO_CASE_GLOB
# extended glob
setopt EXTENDED_GLOB
setopt NUMERIC_GLOB_SORT
# no beeps
setopt NO_BEEP
# careful with rm
setopt RM_STAR_WAIT
# color ls
eval `dircolors -b`
# append history, don't overwrite
setopt APPEND_HISTORY
# no duplicate entries
setopt HIST_IGNORE_DUPS
# save hist space
# Correct Typing errors
setopt correctall
# Remove command duplicata in history
setopt hist_ignore_all_dups
# Dont' save spaced command in history
setopt hist_ignore_space
setopt inc_append_history
setopt autopushd pushdminus pushdsilent pushdtohome
setopt cdablevars
setopt interactivecomments
setopt noclobber
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt SH_WORD_SPLIT
setopt nohup
# Use correctly symbolic links
setopt chase_links
setopt hist_verify
# Use regex in commands.
setopt extendedglob
# }}}
# Beep go to hell
unsetopt beep
unsetopt hist_beep
unsetopt list_beep

# Configurations:
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*:descriptions' format '%U%F{cyan}%d%f%u'
#}}}

# Some mods {{{
zmodload zsh/stat
zmodload zsh/complist
# }}}
# Oh, and don't forget this baby:
set always_to_end
