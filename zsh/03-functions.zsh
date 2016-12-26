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
# emacs functions
EMACS_CLIENT_CMD=(command emacsclient)
EMACS_CLIENT_APP='emacs-client'
EMACS_STANDALONE_CMD=(command emacs)
EMACS_DAEMON_LOG="$HOME/.emacs.d/daemon.log"
function emacsclient () {
    XMODIFIERS='@im=none' $EMACS_CLIENT_CMD "$@"
}
function emacsc () {
    emacsclient -nw "$@"
}
function emacsclient_desktop () {
    gtk-launch "$EMACS_CLIENT_APP" >/dev/null 2>&1
}
function emacs-standalone () {
    XMODIFIERS='@im=none' $EMACS_STANDALONE_CMD "$@"
}

function emacsb () {
    [[ -z "$1" ]] && {
        cat <<EOF
Usage: $0 [compile [-p] [-L dir ...] FILE]...
EOF
        return
    }
    local cmd; cmd=(emacs-standalone --batch)
    local -a libs; libs=()
    local compile; compile=($cmd -l ~/.emacs.d/init/compile.el)
    local action; action=$1; shift
    local package=0
    [ "x$1" = 'x-p' ] && { package=1; shift; }
    while [[ "x$1" = 'x-L' ]]; do
        libs=($libs $1 "$2"); shift; shift
    done
    case "$action" in
    compile)
        if [ $package = 1 ]; then
            $compile -L . $libs -f batch-byte-compile-with-package "$@"
        else
            $cmd -L . $libs -f batch-byte-compile "$@"
        fi
        ;;
    help)
        $0
        ;;
    *)
        $cmd "$@"
        ;;
    esac
}
alias emacs-compile="emacsb compile"

# Emacs server
function emacsd () {
    local cmd; cmd=(emacs-standalone --daemon)
    [[ -z "$1" ]] && 1='help'
    local action; action=$1; shift
    case "$action" in
    status)
        cmd=($EMACS_STANDALONE_CMD)
        [[ "$cmd[1]" == 'command' ]] && cmd=$cmd[2,-1]
        local grep; grep=(pgrep -f -u $USER "^$cmd --daemon")
        if [[ -n `$grep` ]]; then
            echo 'emacs daemon is running'
            return 0
        fi
        echo 'emacs daemon is not running'
        return 1
        ;;
    echo)
        cmd=($EMACS_CLIENT_CMD)
        $0 status >/dev/null &&
            $cmd --eval "(message \"$1\")" >/dev/null 2>&1 && return 0
        return 1
        ;;
    start)
        $0 status >/dev/null && {
            echo 'emacs daemon is already running'
            return 1
        }
        if [[ -n "$EMACS_DAEMON_LOG" ]]; then
            local log; log="$EMACS_DAEMON_LOG"
            DBUS_SESSION_BUS_ADDRESS= SESSION_MANAGER= $cmd 2>&1 | tee "$log"
        else
            DBUS_SESSION_BUS_ADDRESS= SESSION_MANAGER= $cmd
        fi
        ;;
    stop)
        cmd=($EMACS_CLIENT_CMD)
        $0 status >/dev/null &&
            $cmd -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'
        ;;
    restart)
        $0 stop
        local -i c; c=0
        while (( c < 10 )) && $0 status >/dev/null; do
            (( c++ ))
            sleep 0.1
        done
        $0 start
        ;;
    setenv)
        cmd=($EMACS_CLIENT_CMD)
        local bs='\\\\'
        local val="$2"; val="${val//\\/${~bs}}"; val="${val//\"/\\\"}"
        $cmd -e "(let ((val \"$val\")) \
                   (setenv \"$1\" (if (> (length val) 0) val nil)))"
        ;;
    update-env)
        while [[ -n "$1" ]]; do
            $0 setenv "$1" "${(P)1}" >/dev/null; shift
        done
        ;;
    wait)
        local w=0.3
        local trial=200
        local i=0
        for (( i=0; $i < $trial; i++ )); do
            $0 echo 'ping' && return 0
            sleep $w
        done
        return 1 # timedout
        ;;
    *)
        echo "Usage: $0 status|start|stop|restart|setenv|update-env"
        ;;
    esac
}

function _emacs_get_comm () {
    local -a opts
    [[ -n "$EMACS_SERVER_FILE" ]] && opts[1]="$EMACS_SERVER_FILE"
    zparseopts -E -a opts s: -socket-name: f: -server-file:
    (( $#opts > 0 )) && echo ${opts[-1]#=}
}

function emacs () {
    if [[ -z "$EMACS_USE_DAEMON" ]] || [[ `id -ur` = 0 ]]; then
        emacs-standalone "$@"
    else
        emacsd status >/dev/null || emacsd start
        [[ -n "$STY" ]] && {
            # identifier of the target emacs daemon
            local comm; comm=`_emacs_get_comm $@`
            [[ -z "$comm" ]] && comm='default'

            # get daemons already registered
            local reg; reg=`screen_getenv "$STY" SCREEN_EMACSD`;
            reg=(${(s.:.)reg})

            # register emacs daemon to screen
            local num; num=$reg[(i)$comm]; local hook
            reg[$num]="$comm"
            screen_setenv "$STY" SCREEN_EMACSD "${(j.:.)reg}"
            hook="emacsclient $@ -e '(screen-sync-env \"$STY\")'"
            screen_add_attach_hook "$STY" "SCREEN_EMACSD_ENV$num" "$hook"
        }
        DISPLAY="$DISPLAY" emacsc "$@"
    fi
}

function emacs-edit () {
    if [[ -z "$EMACS_USE_DAEMON" ]] || [[ `id -ur` = 0 ]]; then
        if [[ "$1" = '-n' ]]; then
            shift
            emacs-standalone "$@" &
        else
            emacs-standalone "$@"
        fi
    else
        emacsd status >/dev/null || {
            emacsclient_desktop
            emacsd wait
        }
        local frames=$(emacsclient -e '(length (visible-frame-list))')
        (( $frames > 1 )) || emacsclient_desktop
        emacsclient "$@" </dev/null >/dev/null
    fi
}

function snatch() {
    gdb -p $1 -batch -n -x \
        =(echo "p (int)open(\"/proc/$$/fd/1\", 1)
                p (int)dup2(\$1, 1)
                p (int)dup2(\$1, 2)")
}

# Go up
# see http://subtech.g.hatena.ne.jp/secondlife/20080604/1212562182
# and http://d.hatena.ne.jp/hitode909/20100211/1265879271
function _gu () {
    [[ -z "$1" || "${1[1]}" = '/' ]] && {
        [[ -n "$vcs" ]] || return
        # move to repository root
        if [[ "$vcs" = 'git' ]]; then
            local root; root=`git rev-parse --show-cdup`
            [[ -n "$root" ]] && cd "${root:a}"
        else
            $0 ".$vcs"
        fi
        [[ -n "$1" ]] && cd "${1[2,-1]}"
        return
    }
    local parent; parent='.'
    while [[ "${parent:a}" != "/" ]]; do
        parent="../$parent"
        [[ "${parent:a:t}" = "$1" ]] && cd "$parent" && return
        [[ -e "$parent/$1" ]] && cd "$parent" && return
    done
    return 1
}
function gu () {
    [[ "$1" = '-h' || "$1" = '--help' ]] && {
        echo "Usage: $0 [ /<relative> | <directory> | <file> ]"
        cat <<EOT
Go up to the repository root.
Options:
  /<relative>    The destination is <relative> under the repository root.
  <directory>    The destination is an ancestor of the current directory,
                 where the name of the ancestor matches <directory>.
  <file>         The destination is an ancestor of the current directory,
                 where the ancestor contains a file whose name matches <file>.
EOT
        return
    }
    local stack; stack=("${${(f)$(dirs -lp)}[@]}")
    local dir; dir="${$(pwd):a}"
    _gu $@
    dirs $stack
    [[ "$dir" != "${$(pwd):a}" ]] && return
    popd
    [[ -z "$1" ]] && cd ..
}

function watchdir () {
    [[ -z "$1" ]] && {
        echo "Usage: $0 <dir> [-e event1 -e event2 ...]"
        return
    }

    whence inotifywait >/dev/null || {
        echo "$0: inotifywait not found" > /dev/stderr
        return
    }

    local dir="$1"; shift
    ls $dir
    while true; do
        inotifywait -q "$@" "$dir"
    done
}

function git-set-remote () {
    if [[ "$1" == '-h'  || "$1" == '--help' || "$1" == 'help' ]]; then
        echo "Usage: $0 <remote=origin> <branch=CURRENT_BRANCH>"
        return
    fi
    local remote=$1
    local branch=$2
    [[ -n "$remote" ]] || remote=origin
    [[ -n "$branch" ]] || branch=`git rev-parse --abbrev-ref HEAD`
    git config --add "branch.$branch.remote" "$remote"
    git config --add "branch.$branch.merge" "refs/heads/$branch"
}

# file conversion
function pdf2svg {
    [[ "$1" == '-h' || "$1" == '--help' ]] && {
        echo "Usage: $0 [-p <page>] <input> <output>"
        return
    }

    local page; page=1
    [[ "$1" == '-p' ]] && {
        shift; page="$1"; shift
    }
    local catpdf; catpdf=(pdftk "$1" cat $page output -)
    pstoedit -f plot-svg -dt -ssp =($catpdf) "$2"
}
# vim: ft=sh:
