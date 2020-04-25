# 01-env.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# Basic Environs
export SHELL='/bin/zsh'
export EDITOR='emacs -nw -c -nw -a= $*'
export ALTERNATE_EDITOR="nano"
export VIEW='w3m'
export PAGER='less'

# Locales
# export LC_ALL=en_US.UTF-8
# export LANG=en_US.UTF-8
# export LANGUAGE=en_US.UTF-8
export QT_XKB_CONFIG_ROOT=/usr/share/X11/xkb

# Paths
export PATH=$PATH:$HOME/.local/bin
