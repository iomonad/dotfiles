# 01-env.zsh

# basic unix req
export SHELL='/bin/zsh'
export EDITOR='emacs'
export VIEW='w3m'
export PAGER='less'

# locales
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# paths
export PATH=$PATH:$HOME/documents/bins

PATH="$HOME/devels/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="$HOME/devels/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/devels/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/devels/perl5/\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/devels/perl5"; export PERL_MM_OPT;
