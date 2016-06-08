# Some Functions 

# Mplayer Hack {{{
mplayer() {
  /usr/bin/mplayer \
    -msgmodule 1 -msgcolor -include $XDG_CONFIG_HOME/mplayer/config "$@"
}
mplayer_headphones() {
  /usr/bin/mplayer \
    -msgmodule 1 -msgcolor -include $XDG_CONFIG_HOME/mplayer/config \
    -channels 6 -af resample=48000,hrtf "$@"
}
#}}}
# ZSH compile {{{
zc() {
  for z in $HOME/etc/zsh/*.zsh $HOME/.zshrc; do
    zcompile $z
    echo "Compiled $z"
  done
}
#}}}
# Edit Configs {{{
vimconfig() {
  vim $HOME/.vim{rc,/*.vim}
}

zshconfig() {
  vim $XDG_CONFIG_HOME/zsh/*.{zsh,theme}
}
#}}}
# The Evil Become {{{
daemon() {
  echo '
sub daemonize {
  my $daemon_log = shift // q{/dev/null};
  use POSIX q{setsid};
  my $PID = fork();
  exit(0) if($PID); #parent
  exit(1) if(!defined($PID)); # out of resources

  setsid();
  $PID = fork();
  exit(1) if(!defined($PID));

  if($PID) { # parent
    waitpid($PID, 0);
    unlink($pidfile_daemon); # remove the lock when child have died
    exit(0);
  }
  elsif($PID == 0) { # child
    open(my $fh, q{>}, $pidfile_daemon)
      or die(qq{Cant open $pidfile_daemon: $!});
    print $fh $$;
    close($fh);
    open(STDOUT, q{>}, $daemon_log);
    open(STDERR, q{>}, q{/dev/null});
    open(STDIN,  q{<}, q{/dev/null});
  }
}
sub killkid {
  open(my $fh, q{<}, $pidfile_pimpd) or return 1; # for now
  my $pimpd_player = <$fh>;
  close($fh);
  return 0;
}'
}
#}}}
# System info {{{
du1() {
  du -h --max-depth=1 "$@" | sort -k 1,1hr -k 2,2f;
}
#}}}
# Hacked CD {{{
cd() {
  builtin cd $@; ls
}
regcheck() {
  emulate -L zsh
  zmodload -i zsh/pcre
  pcre_compile $1 && \
  pcre_match $2 && echo 'matches' || echo 'no match'
}
#}}}
# ZSH MVP {{{
zsh_stats() {
  history|awk '{print $2}'|sort|uniq -c|sort -rn|head
}
#}}}
# Perl Hacks {{{
absurl() {
  perl -MWWW::Mechanize -e "$m = WWW::Mechanize->new;$u=shift;$m->get($u)||die;print $_->url_abs, "\n" for  $m->links;"
}
#}}}
# The life {{{
tf() {
    val=$?
    if [ "$val" = "0" ]
    then
        echo ":-)"
    else
        echo ":-("
    fi
}
#}}}
# UNIX Hacks {{{
goto() { [ -d "$1" ] && cd "$1" || cd "$(dirname "$1")"; }
cpf() { cp "$@" && goto "$_"; }
mvf() { mv "$@" && goto "$_"; }
#}}}

# Backup ZSH config {{{
zshbackup(){
  cd $HOME/.config/zsh/
  tar -c *.zsh -f backupzsh.tar.gz
  cd ~/
  echo 'Backup saved into ~/.config/zsh/'
}
#}}}
# vim: ft=sh:
