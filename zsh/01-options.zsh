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

bindkey -e
bindkey -M main "^[" vi-cmd-mode
bindkey -M vicmd "/" history-incremental-search-backward
bindkey -M vicmd "q" push-line
bindkey -M vicmd "v" set-mark-command
bindkey -M vicmd "y" copy-region-as-kill

#
# Set vi mode status bar
#

#
# Reads until the given character has been entered.
#
readuntil () {
    typeset a
    while [ "$a" != "$1" ]
    do
        read -E -k 1 a
    done
}

#
# If the $SHOWMODE variable is set, displays the vi mode, specified by
# the $VIMODE variable, under the current command line.
#
# Arguments:
#
#   1 (optional): Beyond normal calculations, the number of additional
#   lines to move down before printing the mode.  Defaults to zero.
#
showmode() {
    typeset movedown
    typeset row

    # Get number of lines down to print mode
    movedown=$(($(echo "$RBUFFER" | wc -l) + ${1:-0}))

    # Get current row position
    echo -n "\e[6n"
    row="${${$(readuntil R)#*\[}%;*}"

    # Are we at the bottom of the terminal?
    if [ $((row+movedown)) -gt "$LINES" ]
    then
        # Scroll terminal up one line
        echo -n "\e[1S"

        # Move cursor up one line
        echo -n "\e[1A"
    fi

    # Save cursor position
    echo -n "\e[s"

    # Move cursor to start of line $movedown lines down
    echo -n "\e[$movedown;E"

    # Change font attributes
    echo -n "\e[1m"

    # Has a mode been set?
    if [ -n "$VIMODE" ]
    then
        # Print mode line
        echo -n "-- $VIMODE -- "
    else
        # Clear mode line
        echo -n "\e[0K"
    fi

    # Restore font
    echo -n "\e[0m"

    # Restore cursor position
    echo -n "\e[u"
}

clearmode() {
    VIMODE= showmode
}

#
# Temporary function to extend built-in widgets to display mode.
#
#   1: The name of the widget.
#
#   2: The mode string.
#
#   3 (optional): Beyond normal calculations, the number of additional
#   lines to move down before printing the mode.  Defaults to zero.
#
makemodal () {
    # Create new function
    eval "$1() { zle .'$1'; ${2:+VIMODE='$2'}; showmode $3 }"

    # Create new widget
    zle -N "$1"
}

# Extend widgets
makemodal vi-add-eol           INSERT
makemodal vi-add-next          INSERT
makemodal vi-change            INSERT
makemodal vi-change-eol        INSERT
makemodal vi-change-whole-line INSERT
makemodal vi-insert            INSERT
makemodal vi-insert-bol        INSERT
makemodal vi-open-line-above   INSERT
makemodal vi-substitute        INSERT
makemodal vi-open-line-below   INSERT 1
makemodal vi-replace           REPLACE
makemodal vi-cmd-mode          NORMAL

unfunction makemodal
