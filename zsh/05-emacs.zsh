# 05-emacs.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

function emacs() {
    emacsclient -c -nw -a= $* 2> /dev/null
}

alias e=emacs