# 01-env.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# Basic Environs
export SHELL='/bin/zsh'
export EDITOR='mg'
export ALTERNATE_EDITOR="emacs"
export VIEW='w3m'
export PAGER='less'
export DIFFTOOL="difft"

if [ -n "$DISPLAY" ]; then
    export BROWSER=google-chrome-stable
else
    export BROWSER=w3m
fi

# Locales
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# Paths
export PATH=$PATH:$HOME/.local/bin:$HOME/bin:$HOME/sdks/android/platform-tools

# Misc
export QT_XKB_CONFIG_ROOT=/usr/share/X11/xkb
