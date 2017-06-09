#!/usr/bin/zsh

# Some Classic {{{
export HOSTNAME="darknet"
export USER_NAME="iomonad"
export USER_MAIL="iomonad@riseup.net"
export USER_CITY="Delft, NL"
export USER_FAVORITE_EDITOR="vim"
export USER_FAVORITE_IDE="emacs"
export USER_FAVORITE_BROWSER_GUI="qutebrowser"
export USER_FAVORITE_BROWSER_CLI="w3m"
export USER_FAVORITE_PAGER="less"
export USER_FAVORITE_MEDIA_PLAYER="mplayer"
export USER_FAVORITE_MUSIC_PLAYER="ncmpcpp"
export USER_FAVORITE_MAIL_READER="mutt"
export USER_FAVORITE_LANGUAGE="haskell"
export USER_FAVORITE_SCRIPT_LANGUAGE="perl"
export USER_FAVORITE_SHELL="$SYSTEM_BOOTSTAGE_BIN/zsh"
export USER_FAVORITE_OFFICE_APP="libreoffice"
export USER_FAVORITE_KEYMAP="dvorak"
export USER_FAVORITE_PDF_READER="zathura"
export USER_FAVORITE_IMAGE_VIEWER="sxiv"
export USER_FAVORITE_IRC_CLIENT="weechat"
export USER_FAVORITE_MUSIC_STYLE="ACID"
export USER_FAVORITE_OS="gentoo"
export USER_FAVORITE_WM="xmonad"
export USER_FAVORITE_BUILD_TOOL="make"
export USER_FAVORITE_JAVA_BUILD_TOOL="gradle"
export USER_FAVORITE_HASKELL_BUILD_TOOL="stack"
export USER_PHONE_OS="cyanogenmod"
export USER_FAVORITE_SEARCH_ENGINE="https://duckduckgo.com"
export USER_FAVORITE_PLAYLIST="https://www.youtube.com/playlist?list=PLB_qb4K3oqyEA7VHJtxs7ehAL8cHVwCnb"
export USER_FAVORITE_QUOTE="The more you can increase fear of drugs and crime, welfare mothers, immigrants and aliens, the more you control all the people."
export ZSH_THEME="g0tr00t"
export ZSH_ACTIVE_COMPLETIONS="$( echo ${(kv)_comps[@]} )"
export EDITOR=vim
export PAGER=less
export BROWSER=/opt/firefox/firefox
export PORT="1337"
#}}}

# Directories {{{
export XDG_CONFIG_HOME="$HOME/.config"
#export XDG_CACHE_HOME="$HOME/etc/cache"
#export XDG_DATA_HOME="$HOME/var"
#export DEVEL_HOME="$HOME/dev"
export BIN_HOME="$HOME/bin"
#export DEVEL_HOME="$HOME/dev"
# Export The urxvt Socket path
export RXVT_SOCKET='/tmp/urxvt-socket'
#}}}}

# Perl Hack Path {{{
export PERL_HACK_LIB="/tmp"
export PERL_MM_USE_DEFAULT=1
#export PERL5OPT='-Mstrict'
#}}}
# Harness Summary {{{
export HARNESS_SUMMARY_COL_FAIL="bold red"
export HARNESS_SUMMARY_COL_SUC="bold cyan"
#}}}
# Gists ;_;
#export GIST_DIR=$HOME/dev/_gists/
# GO Path{{{
export GOROOT=$HOME/bin/go
#}}}
# Vi still alive ???{{{
export VI_QUICKFIX_SOURCEFILE='stdin'
#}}}
# My HardWare {{{
export LP_DEST="Canon_MP150"
export PRINTER="Canon_MP150"
export PHONE="Huawei PLK-l01"
export SSD="toshiba"
export GFX_CARD="Nvidia GTK 750"
export ETHERNET_CONTROLER="Qualcomm Atheros Killer E220x"
export IOT_CARD="Beagle_Bone_Black"
#}}}
# Zsh History Hack {{{
export HISTFILE=$XDG_DATA_HOME/zsh/history
export HISTSIZE=5000
export SAVEHIST=1000000
export HISTIGNORE="&:ls:[bf]g:exit:reset:clear:cd*"
#}}}
# Some Pango Fix {{{
export MOZ_DISABLE_PANGO=1
#export CLIVE_CONFIG="/home/scp1/etc/cliverc"

# The Path {{{
export GEM_PATH='/home/seyt/.gem/ruby/2.2.0/bin/'
export MANPATH=$MANPATH:/usr/local/man:/opt/local/share/man
#}}}

# Android sdk
export MONGODB_PATH="mongodb://monabot:secretpass@localhost:27017/irc-bot-db"
# Fonts For System, Useless {{{
export FONT='-windows-montecarlo-medium-r-normal--0-0-72-72-c-0-microsoft-cp1252'
export FONT_B='-windows-montecarlo-bold-r-normal--0-0-72-72-c-0-microsoft-cp1252'
export FONT_I='-nil-profont-medium-r-normal--10-100-72-72-c-50-iso8859-1'
#}}}
# Dmenu Config {{{
export CLIPBORED_DMENU_FONT=${FONT}
export CLIPBORED_DMENU_NORMAL_FG='#484848'
export CLIPBORED_DMENU_NORMAL_BG='#1c1c1c'
export CLIPBORED_DMENU_SELECT_FG='#1c78ef'
export CLIPBORED_DMENU_SELECT_BG='#292929'
export CLIPBORED_DMENU_LISTMODE='vertical'
export CLIPBORED_DMENU_LINES=30
export DMENU_FONT=${FONT}
export DMENU_NORMAL_FG='#484848'
export DMENU_NORMAL_BG='#1c1c1c'
export DMENU_SELECT_FG='#1c78ef'
export DMENU_SELECT_BG='#292929'
export DMENU_LINES='-l 30'
#}}}
# Dzen2 Config {{{
export DZEN_FONT=${FONT}
export DZEN_FG='#ffffff'
export DZEN_BG='#1c1c1c'
export DZEN_WIDTH=1680
export DZEN_X_POS=0
export DZEN_Y_POS=1040
export DZEN_ALIGNMENT='c'
export X_OSD_COLOR='#a8ff00'
#}}}
# Yeah Hack My MPD {{{
#export MPD_HOST='localhost'
#export MPD_HOST='192.168.1.100'
#export MPD_PORT=6600
#export MPD_PASS=`smokingkills`
#export MPD_USER='mpd'
export LISTMAX=300
#}}}
# Some shit Paths {{{
#export CDPATH='.:~:/mnt'
#export PERL_DL_NONLAZY=1
#export PERL_UNICODE=1
#export PERL_RL=Zoid
#}}}

# Ruby Path
export GEM_PATH='/home/seyt/.gem/ruby/2.2.0/bin/'

export CONFIG_PATH="$HOME/etc"
export ZSH_PATH="$CONFIG_PATH/zsh"
export VIM_PATH="$CONFIG_PATH/vim"
export EMACS_PATH="$HOME/.emacs.d"
export XMONAD_PATH="$CONFIG_PATH/xmonad"
export MOZILLA_HAXX_PATH="$CONFIG_PATH/mozilla/"

# Android PATH {{{
export ANDROID_HOME="$HOME/android/sdk/"
export ANDROID_SDK_VER="25.0.1"
export ANDROID_BUILD_TOOLS="$ANDROID_HOME/build-tools/$ANDROID_SDK_VER/"
export ANDROID_PLATFORM_TOOLS="$ANDROID_HOME/platform-tools/"
export ANDROID_TOOLs="$ANDROID_HOME/tools"
export CYANOGEN_ROOT="$HOME/android/cyanogen"
export ANDROID_PATHS="$ANDROID_BUILD_TOOLS:$ANDROID_TOOLs:$ANDROID_PLATFORM_TOOLS"
export PICC="$HOME/dev/rpi-toolchain"
# }}}

# Java path {{{
export JAVA_BIN_PATH="/opt/jdk/bin/"
# }}}

# Haskell related {{{
export CABAL_BIN_PATH="$HOME/.cabal/bin"
export STACK_BIN_PATH="$HOME/.stack/bin" # Nope
# }}}

# User binary path {{{
export USER_BIN_PATH="$HOME/bin"
export USER_UTILS_PATH="$HOME/bin/utils/"
export SYSTEM_BOOTSTAGE_BIN="/bin"
export SYSTEM_SU_BIN="/sbin"
export SYSTEM_WIDE_BIN="/usr/bin"
export SYSTEM_SU_WIDE_BIN="/usr/sbin"
export SYSTEM_HOST_BIN="/usr/local/bin"
export SYSTEM_PATHS="$USER_BIN_PATH:$SYSTEM_BOOTSTAGE_BIN:$SYSTEM_SU_BIN:$SYSTEM_WIDE_BIN:$SYSTEM_SU_WIDE_BIN:$SYSTEM_HOST_BIN:$USER_UTILS_PATH"
# }}}
# Perl Paths {{{
export PERL_SITE_BIN="$SYSTEM_WIDE_BIN/site_perl"
export PERL_CORE_PATH="$SYSTEM_WIDE_BIN/core_perl"
export PERL_VENDOR_PATH="$SYSTEM_WIDE_BIN/vendor"
export PERL5_SITE_BIN="/usr/lib/site_perl/bin"
export PERL5_CORE_PATH="/usr/lib/perl5/core_perl/bin"
export PERL5_VENDOR_PATH="/usr/lib/perl5/vendor_perl/bin"
export PERL_PATHS="$PERL_SITE_BIN:$PERL_CORE_PATH:$PERL_VENDOR_PATH:$PERL5_SITE_BIN:$PERL5_CORE_PATH:$PERL5_VENDOR_PATH"
# }}}

export GOPATH="$HOME/dev/gopath"
unset GOROOT # fix path
# The Glorious Path {{{
export PATH="$SYSTEM_PATHS:$GEM_PATH:$CABAL_BIN_PATH:$STACK_BIN_PATH:$JAVA_BIN_PATH:$PERL_PATHS:$ANDROID_PATHS:$PICC"
#}}}

# vim: set ts=2 expandtab sw=2:
IPFS_PATH=/home/iomonad/public/ipfs
export KBUILD_VERBOSE=1
