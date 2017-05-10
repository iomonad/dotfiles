#!/usr/bin/zsh

# Globals {{{
bindkey -v
bindkey "^?" backward-delete-char
bindkey "^H" backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^r" history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^W" backward-delete-word
bindkey "^b" backward-word
bindkey "^f" forward-word
bindkey "^d" delete-word
bindkey "^k" kill-line
bindkey " " magic-space
bindkey "^I" complete-word
#}}}

# ../ {{{
bindkey -M isearch " " self-insert
#}}}
# NULL * {{{
bindkey -s '^x0' ' &> /dev/null '
bindkey -s '^x1' ' > /dev/null '
bindkey -s '^x2' ' 2> /dev/null '
bindkey -s '^x3' ' 2>&1 '
#}}}

# Soft {{{
# warning -> ^m is enter !
bindkey -s '^k' 'zsh_stats\n'
bindkey -s '^t' 'htop\n'
bindkey -s '^n' 'ncmpcpp\n'
bindkey -s '^v' 'vim\n'
bindkey -s '^u' 'sudo emerge --sync && sudo emerge -auDN @world\n'
bindkey -s '^b' 'mutt\n'
bindkey -s '^w' 'firefox'
#bindkey -s '^p' 'rm -rf ~/.{local,cache,gvfs,dbus,m2,w3m,thumbnails,lyrics,java,ipython,dbshell,wget-hsts,netrc,swp}\n'
bindkey -s '^p' 'git push origin master'
#}}}

# Bind custom defined completers {{{
#bindkey "^N"      most-accessed-file
bindkey "^X^A"    all-matches
bindkey "^_^A"    all-matches
bindkey "^X^P"    pids
bindkey "^_^P"    pids
#}}}

# Other {{{
bindkey "\e[1~" beginning-of-line # Home
bindkey "\e[4~" end-of-line # End
bindkey "\e[5~" beginning-of-history # PageUp
bindkey "\e[6~" end-of-history # PageDown
bindkey "\e[2~" quoted-insert # Ins
bindkey "\e[3~" delete-char # Del
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "\e[Z" reverse-menu-complete # Shift+Tab
# for rxvt
bindkey "\e[7~" beginning-of-line # Home
bindkey "\e[8~" end-of-line # End
# for non RH/Debian xterm, can't hurt for RH/Debian xterm
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
# for freebsd console
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line
# for guake
bindkey "\eOF" end-of-line
bindkey "\eOH" beginning-of-line
bindkey "^[[1;5D" backward-word
bindkey "^[[1;5C" forward-word
bindkey "\e[3~" delete-char # Del
# History
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
# Misc
# don't nice background tasks
setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP
# allow functions to have local options
setopt LOCAL_OPTIONS
# allow functions to have local traps
setopt LOCAL_TRAPS
# share history between sessions ???
setopt SHARE_HISTORY
# add timestamps to history
setopt EXTENDED_HISTORY
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
# adds history
setopt APPEND_HISTORY
# adds history incrementally and share it across sessions
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
# don't record dupes in history
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt HIST_EXPIRE_DUPS_FIRST
# dont ask for confirmation in rm globs*
setopt RM_STAR_SILENT
# shellcheck disable=SC2004
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init() {
    echoti smkx
  }
  function zle-line-finish() {
    echoti rmkx
  }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

# Use emacs key bindings
bindkey -e

# [Ctrl-r] - Search backward incrementally for a specified string.
# The string may begin with ^ to anchor the search to the beginning of the line.
bindkey '^r' history-incremental-search-backward
# [PageUp] - Up a line of history
if [[ ! -z "$terminfo[kpp]" ]]; then
  bindkey "$terminfo[kpp]" up-line-or-history
fi
# [PageDown] - Down a line of history
if [[ ! -z "$terminfo[knp]" ]]; then
  bindkey "$terminfo[knp]" down-line-or-history
fi
if [[ ! -z "$terminfo[khome]" ]]; then
    # [Home] - Go to beginning of line
    bindkey "$terminfo[khome]" beginning-of-line
    # OPTION+left
    bindkey '[D' beginning-of-line
fi
if [[ ! -z "$terminfo[kend]" ]]; then
  # [End] - Go to end of line
  bindkey "$terminfo[kend]"  end-of-line
  # OPTION+right
  bindkey '[C' end-of-line
fi

# [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5C' forward-word
# [Ctrl-LeftArrow] - move backward one word
bindkey '^[[1;5D' backward-word
# [Shift-Tab] - move through the completion menu backwards
if [[ ! -z "$terminfo[kcbt]" ]]; then
  bindkey "$terminfo[kcbt]" reverse-menu-complete
fi
# [Backspace] - delete backward
bindkey '^?' backward-delete-char
# [Delete] - delete forward
if [[ ! -z "$terminfo[kdch1]" ]]; then
  bindkey "$terminfo[kdch1]" delete-char
else
  bindkey "^[[3~" delete-char
  bindkey "^[3;5~" delete-char
  bindkey "\e[3~" delete-char
fi
#}}}
