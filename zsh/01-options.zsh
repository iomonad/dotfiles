# The shell options {{{
# ZSH Modules
autoload -U compinit promptinit zsh-mime-setup colors
compinit
promptinit
zsh-mime-setup
colors

# Options
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

# Configurations:
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
#}}}
