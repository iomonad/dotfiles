# Some Functions

# Mplayer Hacks {{{
mplayer() {
  /usr/bin/mplayer \
    -msgmodule 1 -msgcolor -include $HOME/.mplayer/config "$@"
}
mplayer_headphones() {
  /usr/bin/mplayer \
    -msgmodule 1 -msgcolor -include $HOME/.mplayer/config \
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
  vim $HOME/etc/zsh/*.zsh
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
  tar -c *.zsh -f backupzsh.tar.gz &> /dev/null
  cd ~/
  echo 'Backup saved into ~/.config/zsh/'
}
#}}}
# nullpointer url shortener {{{
short() {
  curl -F"shorten=$*" https://0x0.st
}
#upload file
x0st() {
    curl -F"file=@${1}" https://0x0.st
}
#}}}

random-wallpaper () {
    while true; do
    #Deleting any old wallpaper downloaded by this app in the past
    rm wallpaperRandomUnsplash.jpg

    # Downloading picture
    wget https://unsplash.it/1920/1080/?random >/dev/null 2>&1

    # Converting picture
    mv ./index.html?random ./wallpaperRandomUnsplash.jpg

    # Setting wallpaper
    feh --bg-scale wallpaperRandomUnsplash.jpg

    sleep 600
done
}

shellcode-extract () {

	if [ $# -lt 1 ]; then
              echo '`shellcode-extract` requires an binary object (.o).'
	fi
	for i in $(objdump -d $1 -M intel |grep "^ " |cut -f2); do echo -n '\x'$i; done;echo

}
wall() {
hsetroot -fill ~/media/images/wallpapers/blacknwhite/wallpaper-$(((RANDOM%150)+1)).jpg  -contrast .85 -brightness -0.02
}

# Todo sec
todo-clean() {
    cat /dev/null > $HOME/.todo
}
todo-add() {
    echo "# $1" >> $HOME/.todo
}


sshagent() {
    exec ssh-agent zsh # Open SHell
    ssh-add
}

# Append to the firstline
# of a file.
insheader() {
    if [ -z "$2"]
     then
        echo "Insert header to a file"
        echo "Usage: insheader <header> <file>"
        exit 0
    fi
    echo -e "$(cat $1)\n$(cat $2)" > $2 2> /dev/null
    echo "Done."
}

# Use a direct toilet banner
insbanner() {
    if [ -z "$2"]
     then
        echo "Insert toilet banner to the head"
        echo "Usage: insbanner <text> <file>"
        exit 0
    fi
    echo -e "$(toilet -f future $1)\n$(cat $2)" > $2 2> /dev/null
    echo "Done. Don't forget to add comments."
}
# Dictionnary functions {{{
dwordnet () { curl dict://dict.org/d:${1}:wn; }
dacron () { curl dict://dict.org/d:${1}:vera; }
djargon () { curl dict://dict.org/d:${1}:jargon; }
dfoldoc () { curl dict://dict.org/d:${1}:foldoc; }
dthesaurus () { curl dict://dict.org/d:${1}:moby-thes; }
# }}}

# Wiki search
wikipediaSearch() {
    echo -n -e "\n============================================\n\tWelcome to WikiPedia Search"; echo ""; i=1 ; for line in $(lynx --dump "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&profile=default&search=$1&fulltext=Search" | grep http://en.wikipedia.org/wiki | cut -c7-); do echo $i $line; lines[$i]=$line ;  i=$(($i+1)); done ; echo -n -e "\n============================================\n\tPlease select the link to open - "; read answer; w3m ${lines[$answer]}
}
sepSearch() {
    echo -n -e "\n============================================\n\tWelcome to WikiPedia Search"; echo ""; i=1 ; for line in $(lynx --dump "http://plato.stanford.edu/search/searcher.py?query=$1" | grep http://plato.stanford.edu | cut -c7-); do echo $i $line; lines[$i]=$line ;  i=$(($i+1)); done ; echo -n -e "\n============================================\n\tPlease select the link to open - "; read answer; w3m ${lines[$answer]}
}
# Error prompt
ewarn() { echo -e "\033[1;33m>>> \033[0m$@"; }"]]"}

# Creates an archive from given directory
mktar() { tar cvf  "${1%%/}.tar"     "${1%%/}/"; }
mktgz() { tar cvzf "${1%%/}.tar.gz"  "${1%%/}/"; }
mktbz() { tar cvjf "${1%%/}.tar.bz2" "${1%%/}/"; }

# Colored man pages {{{
man() {
	env \
		LESS_TERMCAP_mb=$(printf "\e[1;37m") \
		LESS_TERMCAP_md=$(printf "\e[1;37m") \
		LESS_TERMCAP_me=$(printf "\e[0m") \
		LESS_TERMCAP_se=$(printf "\e[0m") \
		LESS_TERMCAP_so=$(printf "\e[1;47;30m") \
		LESS_TERMCAP_ue=$(printf "\e[0m") \
		LESS_TERMCAP_us=$(printf "\e[0;36m") \
			man "$@"
}
#}}}
# vim: ft=sh:
