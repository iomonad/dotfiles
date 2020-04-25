# 05-emacs.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

function emacs() {
    emacsclient -c -nw -a= $*
}

alias e=emacs
