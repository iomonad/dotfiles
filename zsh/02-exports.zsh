#!/usr/bin/zsh

# Some Classic {{{
export HOSTNAME="darknet"
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
export PATH=$ANDROID_HOME/tools/:$ANDROID_HOMEbuild-tools/25.0.1/:$ANDROID_HOME/platform-tools/:$GEM_PATH:~/.cabal/bin:/opt/jdk/bin:~/bin:/bin:/usr/bin:/usr/local/bin:/usr/bin/site_perl:/usr/bin/core_perl:/usr/bin/vendor_perl:/usr/lib/perl5/site_perl/bin:/usr/lib/perl5/vendor_perl/bin:/usr/lib/perl5/core_perl/bin:/sbin:/usr/sbin:/usr/bin/perlbin/site:/usr/bin/perlbin/vendor:/usr/bin/perlbin/core:/home/scp1/perl5/perlbrew/perls/perl-5.8.3/:$HOME/go/bin
export MANPATH=$MANPATH:/usr/local/man:/opt/local/share/man
#}}}

# Android sdk
export ANDROID_TOOLS="~/android/sdk/build-tools/24.0.2/:~/android/sdk/platform-tools/:~/android/sdk/tools/"
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
#export MPD_USER='scp1'
export LISTMAX=300
#}}}
# Some Fucked Paths {{{
#export CDPATH='.:~:/mnt'
#export PERL_DL_NONLAZY=1
#export PERLBREW_ROOT="/mnt/Leftover/Perl5"
#export PERL_UNICODE=1
## Use Term::ReadLine::Zoid for vi-bindings
## Not needed anymore - http://japh.se/all/perl/2011/01/21/Readline/
## also see https://github.com/trapd00r/configs/blob/master/.inputrc
#export PERL_RL=Zoid
#}}}
export ANDROID_HOME="/home/seyt/Android/Sdk/build-tools"
# vim: set ts=2 expandtab sw=2:

