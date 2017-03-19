#vim: tw=0 nowrap ts=2 ft=sh:

# Some Fun And Usefull Aliases

# Some Perl Hacks {{{
alias 80='perl -e "print q[x] x 80, qq[\n]"'
# Translate Tricks {{{
alias swe='translate -from en -to swe'
alias  en='translate -from swe -to en'
#}}}
# Random Hacks {{{
alias   today="echo *(e:age today now:)|perl -pe 's/ /\n/g'"
alias    hour="echo *(e-age $(date +%H:00) now-)|perl -pe 's/ /\n/g'"
alias     gcc='gcc -ansi -pedantic -Wextra -Wempty-body -Wfloat-equal -Wignored-qualifiers -Wmissing-declarations -Wmissing-parameter-type -Wmissing-prototypes -Wold-style-declaration -Woverride-init -Wsign-compare -Wstrict-prototypes -Wtype-limits -Wuninitialized -fstack-protector-all -D_FORTIFY_SOURCE=2'
alias    gccc='gcc -ansi -pedantic -Wall'
alias csyntax='gcc -fsyntax-only'
alias   editc='vim $HOME/.zsh/01-colors.zsh $HOME/dev/File::LsColor/lib/File/LsColor.pm $HOME/devel/LS_COLORS/LS_COLORS'
alias   share='python3 -m http.server'
alias     get='woof -u -U -i 0.0.0.0 -p 4040'
alias     put='woof -u -i 0.0.0.0 -p 4040'
#}}}
# SSH Servers Alias {{{
alias brutus='ssh scp1@brutus.ethup.se'
alias macosx='ssh trapd00r@90.225.22.81'
alias macoss='ssh scp1@industrialrefuge.com'
alias  india='ssh scp1@192.168.1.102 -p 19216'
alias   dvdc='ssh scp1@192.168.1.100 -p 19216'
alias   n900='ssh -p 19216 user@192.168.1.112'
alias router='ssh root@192.168.0.47 -p 2314'
# Here copy files to server
alias   sshl='sshfs -p 19216 scp1@192.168.1.100:/var/log/lighttpd /mnt/lighttpd'
alias ip='curl http://jackit.se/ip.php'
# Cool Trick x84
alias x84='ssh anonymous@1984.ws'
#}}}
# Git Alias {{{
alias     gs='git status --short -b'
alias     gt='git tag|sort --reverse'
alias     gp='git push'
alias    gdd='git diff'
alias     gc='git commit'
alias    glp='gl -p'
alias    gcu='git commit -m "updates"'
alias github='PAGER=cat perl /home/scp1/bin/github'
alias   what="--stat -p --reverse HEAD@{1}.. | perl -pe 's/^(\++.+).*/\e[38;5;34m\e[1m$1\e[m/; s/^(\-+.*)/\e[38;5;196m\e[1m$1\e[m/'"
#alias     gd='PAGER="" git diff $ | skate --language diff -'
#}}}
# UNIX Basics Hacks {{{
alias      cp='cp -v'
alias      mv='mv -v'
alias      rm='rm -v'
alias    grep='grep -Pi --color=auto'
alias   grepp='grep -Pi --color=auto "^|$@"'
alias    rmvi='rm *.sw*'
alias prename='prename -v'
alias bright="xbacklight -set"
alias ls="ls++"
#}}}
# Perls Utils {{{
alias      pc='perlcritic'
alias     pod='grep \=pod -A 9999 "$@"'
alias     pdb='perl -d -e 1'
alias   perlf='ack -f --perl'
#}}}
# System Fast Moving {{{
alias     b='cd $HOME/bin'
alias     c='cd $HOME/.config'
alias     r='cd $HOME/docs/rice'
alias     p='cd $HOME/docs/pentest'
alias     z='cd $HOME/.config/zsh'
alias     d='cd $HOME/docs/rice/dots/'
#}}}
# Hacks Basic Commands {{{
#alias lsusb='lsusb | matchline -random'
#alias lspci='lspci | matchline -lspci'

#alias  ls=' ls++'
alias lso='\ls | pv -qL 10'
alias lsq='\ls --color=always --time-style=full-iso -AlQ'
alias lsl='\ls --color=auto   --group-directories-first -Ah'
alias lss='\ls --color=auto -1 | grep "(^\w*[^_-])"'
alias ls1='\ls -1'
alias lsa='\ls --color=auto --time-style=full-iso'
alias lsd='/bin/ls -FAv | grep /$ | column'
#}}}

# C'mon {{{
alias xlx='xrdb -load ~/.Xresources'
alias dirsize='du -h --max-depth=1 "$@" | sort -k 1,1hr -k 2,2f'
alias creationdate='stat -F '%D' +mtime'
alias tcpdump='sudo tcpdump -i eth0'
alias installfont='sudo fc-cache -f -v'

#}}}
# Common Application Hacks {{{
alias sortbycolumn='sort -n -k3'
alias            R='rehash'
alias           qi='qemu -cdrom iso -boot d hd'
alias           ss='source $HOME/etc/zsh/zshrc'
alias          npd='srinfo -np'
alias         scat='source-highlight -o STDOUT -f esc -i'
# Old School OSS {{{
alias       v+='ossmix vmix0.pcm8 -- +2'
alias       v-='ossmix vmix0.pcm8 -- -2'
alias      v++='ossmix vmix0.pcm9 -- +2'
alias      v--='ossmix vmix0.pcm9 -- -2'
# }}}
# Old School Wminput {{{
alias      wmp='wminput -c mplayer&'
alias      win='wminput -c ir_ptr -w -c neverball&'
alias      wir='wminput -c ir_ptr -w -c ir_ptr&'
alias      wim='wminput -c ir_ptr -w -c buttons-mame&'
alias      wig='wminput -c gamepad&'
alias      wit='wminput -c buttons-term&'
#}}}
#}}}
# VimSpeed Edit {{{
alias    vimsh='vim *.sh'
alias    vimpm='vim *.pm'
alias    vimpl='vim *.pl'
alias     vimc='vim *.{c,h}'
#}}}
# Lost & useless {{{
#alias     cpan='cpanm'
alias    flash=' clive --stream-exec="mplayer -really-quiet %i" --stream=10'
alias     wimp='(wminput -c mplayer&); mplayer'
alias       :q='exit'
alias      die='kill -9 $$'
alias    urxvt='urxvt -name URxvt.darknet'
alias     wget='wget --no-check-certificate -U=Mozilla'
alias     ptop='watch -n1 ps aux --sort=+%cpu'
alias     tree='tree -dA'
#}}}
# Other fun Tricks {{{
alias gource='gource -1280x720 --max-files 0 --file-idle-time 0 --bloom-multiplier 0.5 --bloom-intensity 0.9 -e 0.7 --background 121212 -o - | ffmpeg -y -b 3000K -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -vpre slow -threads 0 gource.mp4'
alias testfetch='sync_cpantesters -a WOLDRICH -d $HOME/dev/CPANTS \
                    && cd $HOME/dev/CPANTS'
alias   iostat='iostat -mtx'
alias     cpuu='ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10'
alias memusage='ps -e -orss=,args= | sort -b -k1,1n|pr -TW$COLUMNS'
alias    shiva='ps -eo pcpu,pid,user,args \
                  | sort -k 1 -r \
                  | head -10 && iostat -mtx && mpstat -P 'ALL' && sar'


alias reset='printf "\033c\033(K\033[J\033[0m\033[?25h"'
alias dev_null='rm /dev/null; mknod /dev/null c 1 3'
#}}}
# Xero Alias, Thanks to him :) {{{
alias v="vim"
alias vi="vim"
alias nano="vim"
alias disks='echo "╓───── m o u n t . p o i n t s"; echo "╙────────────────────────────────────── ─ ─ "; lsblk -a; echo ""; echo "╓───── d i s k . u s a g e"; echo "╙────────────────────────────────────── ─ ─ "; df -h;'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias clbin="curl -F 'clbin=<-' https://clbin.com"
alias psef="ps -ef"
#}}}
# {{{ Title stuffs
precmd() {
	setprompt
	case $TERM in
		rxvt-256color | screen-256color )
			print -Pn "\e]0;%n@%m: %~\a" ;;
	esac
}

preexec() {
	case $TERM in
		rxvt-256color | screen-256color )
			print -Pn "\e]0;$1\a" ;;
	esac
} # }}}
# System Hacks {{{
# Fun with sed
alias df='df -h | grep sd |\
  sed -e "s_/dev/sda[1-9]_\x1b[34m&\x1b[0m_" |\
  sed -e "s_/dev/sd[b-z][1-9]_\x1b[33m&\x1b[0m_" |\
  sed -e "s_[,0-9]*[MG]_\x1b[36m&\x1b[0m_" |\
  sed -e "s_[0-9]*%_\x1b[32m&\x1b[0m_" |\
  sed -e "s_9[0-9]%_\x1b[31m&\x1b[0m_" |\
  sed -e "s_/mnt/[-_A-Za-z0-9]*_\x1b[34;1m&\x1b[0m_"'

alias duch='du -ch | grep insgesamt |\
  sed -e "s_[0-9]*,[0-9]*[B|G|K|M|T]_\x1b[32m&\x1b[0m_"'
alias t='tmux'
alias ta='tmux attach -t'
alias cp='cp -r '
alias pingg='ping google.fr -c 5'
alias pingb='ping 192.168.0.254 -c 5'
#}}}
# {{{ Oneliners
goto() { [ -d "$1" ] && cd "$1" || cd "$(dirname "$1")"; }
cpf() { cp "$@" && goto "$_"; }
mvf() { mv "$@" && goto "$_"; }
mkf() { mkdir -p $1; cd $1 }
cdl() { cd $@; ls++ }
d() { ($1 &) }
zsh_stats() { history | awk '{print $2}' | sort | uniq -c | sort -rn | head }
du1() { du -h --max-depth=1 "$@" | sort -k 1,1hr -k 2,2f; }
epoch() { print $(( `echo $1 | cut -b 1-2` * 3600 + `echo $1 | cut -b 4-5` * 60 + `echo $1 | cut -b 7-8` )) }
alias empty-trash="rm -rf ~/.local"
# }}}

# GIT {{{
alias ga="git add -A"
alias gm="git commit -m"
alias gp="git push origin master"
alias gi="git init"
alias gib="git init --bare"
alias gl="git log"
# }}}

# Raspberry {{{
alias framboise="ssh root@192.168.0.47"
# FUN {{{
alias reddit="rtv"
#}}}


alias nano="vim"
alias youtube="mpsyt"
alias external-ip="curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'  '>'"
alias radio="mplayer http://radio.2f30.org:8000/live.mp3"
alias radio-trinitas="mpv http://live.radiotrinitas.ro:8003"
alias radio-uzic="mplayer http://www.uzic.ch/tek.m3u"
alias radio-trance="mplayer http://streaming.radionomy.com:8000/Trance-Libre-webradio.m3u"
alias estimate-update="emerge -upvND --with-bdeps=y @world"
alias pcmanfm="dbus-launch pcmanfm"
alias speed_test='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test10.zip'
alias poweroff="sudo poweroff"
alias shutdown="sudo shutdown now"
alias reboot="sudo reboot"
# Cabal {{{
alias ci="cabal install"
alias cu="cabal update"
alias cb="cabal build"
alias cr="cabal run"
#}}}
# Stack {{{
alias sb="stack build"
alias st="stack test"
alias sr="stack resolv"
alias se="stack exec"
alias sg="stack ghci"
alias sd="stack list-dependencies"
# }}}

alias mp="mplayer"
alias mpf="mplayer -fs"

alias e="emacs -nw"
alias es="emacs --daemon"
alias ec="emacsclient -c"

# Connect to the rtorrent daemon
alias torrent="stty stop undef; stty start undef; screen -r rtd"

# Android source code sync tool
alias rs="repo sync -c -j 4" # Current branch for cynaogenmod

# Emerge Tricks {{{
alias esystem_update='sudo emerge -aev world'
alias efull_system_update='sudo emerge -auDN @world'
alias esync="sudo emerge --sync"
alias esearch="sudo emerge --search"
alias einfo="sudo emerge --info"
alias eclean="sudo emerge --clean"
alias edepclean="sudo emerge --depclean"
alias unmerge="sudo emerge --unmerge"
alias listsets="sudo emerge --list-sets"
alias eprune="sudo emerge --prune"
alias eregen="sudo emerge --regen"
alias eresume="sudo emerge --resume"
alias esearchdesc="sudo emerge --searchdesc"
alias newsall="sudo eselect news read all"
alias newslist="sudo eselect news list"
alias newspurge="sudo eselect news purge"
# Should be executed when new kernel update
alias nvidia-rebuild-drivers="emerge @module-rebuild"
#}}}

# w00t {{{
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'

alias sshu='ssh -o UserKnownHostsFile=/dev/null'
alias sr='ssh -l root'
alias sru='ssh -l root -o UserKnownHostsFile=/dev/null'

# }}}

# Newly added {{{
alias pcmanfm='ck-launch-session dbus-launch pcmanfm'
alias vim='/usr/bin/em' # back to Micro Emacs
alias vi='/usr/bin/em'
alias proxy="proxychains -q -f $HOME/etc/proxychains/proxychains.conf"
alias clock="while :; do date '+%T'; sleep 1; done | bar $BAROPT"
alias cd="builtin cd"
# }}}
