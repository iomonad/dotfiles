#vim: tw=0 nowrap ts=2 ft=sh:

# Some Fun And Usefull Aliases

#alias perlbrew='PERLBREW_ROOT="/mnt/Leftover/Perl5" perlbrew'
# Some Perl Hacks {{{
alias 80='perl -e "print q[x] x 80, qq[\n]"'
alias perlu='perl -Mv5.12 -Mutf8 -Mstrict -Mautodie -Mwarnings -Mwarnings=FATAL,utf8 -CSAD -Mopen=:std,:utf8 -Mcharnames=:full -Mfeature=unicode_strings -MEncode=encode,decode -MUnicode::Normalize=NFD,NFC,NFKD,NFKC'
#}}}
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
alias   share='python -m http.server'
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
alias docupd='scp -P 19216 -r /mnt/Leftover/doc/* scp1@192.168.1.100:http/japh.se/doc'
# Here copy files to server
alias   sshl='sshfs -p 19216 scp1@192.168.1.100:/var/log/lighttpd /mnt/lighttpd'
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
#}}}
# Hacks Basic Commands {{{
alias lsusb='lsusb | matchline -random'
alias lspci='lspci | matchline -lspci'

#alias  ls=' ls++'
alias lso='\ls | pv -qL 10'
alias lsq='\ls --color=always --time-style=full-iso -AlQ'
alias lsl='\ls --color=auto   --group-directories-first -Ah'
alias lss='\ls --color=auto -1 | grep "(^\w*[^_-])"'
alias ls1='\ls -1'
alias lsa='\ls --color=auto --time-style=full-iso'
alias lsd='/bin/ls -FAv | grep /$ | column'
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
# Vim Speed Edit {{{
alias    vimsh='vim *.sh'
alias    vimpm='vim *.pm'
alias    vimpl='vim *.pl'
alias     vimc='vim *.{c,h}'
#}}}
# Lost & useless {{{
#alias     cpan='cpanm'
alias    flash=' clive --stream-exec="mplayer -really-quiet %i" --stream=10'
alias     make='/home/scp1/dev/utils/mymake'
alias     wimp='(wminput -c mplayer&); mplayer'
alias       :q='exit'
alias      die='kill -9 $$'
alias    urxvt='urxvt -name URxvt.shiva'
alias     wget='wget --no-check-certificate -U=Mozilla'
alias     ptop='watch -n1 ps aux --sort=+%cpu'
alias     tree='tree -dA'
alias    dev='echo http://devel.japh.se/ \&& echo http://dev.japh.se/|xclip'
#}}}
# Very Very Deep Tricks {{{
alias gource='gource -1280x720 --max-files 0 --file-idle-time 0 --bloom-multiplier 0.5 --bloom-intensity 0.9 -e 0.7 --background 121212 -o - | ffmpeg -y -b 3000K -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -vpre slow -threads 0 gource.mp4'
alias logstalgia='logstalgia  japh_selected_log -s 5 --output-ppm-stream - |  ffmpeg -y -b 3000K -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -vpre slow -threads 0 logstalgia.mp4'

alias testfetch='sync_cpantesters -a WOLDRICH -d $HOME/dev/CPANTS \
                    && cd $HOME/dev/CPANTS'
alias   iostat='iostat -mtx'
alias     cpuu='ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10'
alias memusage='ps -e -orss=,args= | sort -b -k1,1n|pr -TW$COLUMNS'
alias    shiva='ps -eo pcpu,pid,user,args \
                  | sort -k 1 -r \
                  | head -10 && iostat -mtx && mpstat -P 'ALL' && sar'


alias a='printf "  %s\n  %s\n  %s\n  %s\n" "Magnus Woldrich" "CPAN ID: WOLDRICH" "m@japh.se" "http://japh.se"'
alias trapd00r='printf "\t\033#3trapd00r\n\t\033#4trapd00r\n\tA simple, lightweight Perl hacker\n"'
alias trapd00rc='printf "\t\033#3\e[38;5;25mt\e[38;5;26mr\e[38;5;27ma\e[38;5;31mp\e[38;5;32md\e[38;5;33m0\e[38;5;33m0\e[38;5;37mr\n\t\033#4\e[38;5;133mt\e[38;5;134mr\e[38;5;135ma\e[38;5;139mp\e[38;5;140md\e[38;5;141m00\e[38;5;145mr\n\t\e[38;5;240mA simple, lightweight Perl hacker\n"'
alias reset='printf "\033c\033(K\033[J\033[0m\033[?25h"'
alias dev_null='rm /dev/null; mknod /dev/null c 1 3'
#}}}
# {{{ Title stuffs
precmd() {
	vcs_info
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
# }}}


# {{{ Most used Commands
mostused() {
	sed -n 's/^\([a-z]*\) .*/\1/p' $HISTFILE |
	sort |
	uniq -c |
	sort -n -k1 |
	tail -25 |
	tac
} # }}}

# {{{ FFMPEG stuffs

# Split Video
ffmpeg_splitvid()
{
	local t=$(epoch `ffprobe $1 2>&1 | grep Duration | cut -b 13-20`)
	local first=$(( $t / 3 ))
	local second=$(( $first * 2 ))
	local duration=$(( $first + 30 ))

	ffmpeg -i $1 -ss 0 -t $duration -vcodec copy -sameq -acodec copy -async 100 -threads 0 ${1%.*}.part1.avi
	ffmpeg -i $1 -ss $first -t $duration -vcodec copy -sameq -acodec copy -async 100 -threads 0 ${1%.*}.part2.avi
	ffmpeg -i $1 -ss $second -t $duration -vcodec copy -sameq -acodec copy -async 100 -threads 0 ${1%.*}.part3.avi
}

ffmpeg_bframes()
{
	ffmpeg -i $1 -vcodec copy -sameq -acodec libmp3lame -ab 128k -ar 48000 -ac 2 -threads 0 ${1%.*}.fix.avi
}

# Convert to x264 (stupid railscasts with their stupid .mov that won't play in mplayer)
ffmpeg_x264() {
	ffmpeg -i $1 -acodec aac -strict experimental -ab 96k -vcodec libx264 -vpre slow -crf 22 -threads 0 -f matroska ${1%.*}.mkv
} 

# Rip Audio as MP3
ffmpeg_mp3() {
	ffmpeg -i $1 -acodec libmp3lame -sameq -threads 0 ${1%.*}.mp3
}

# Convert anything to iPhone and move to LAMP for streaming
ffmpeg_iphone()
{
	ffmpeg -i $1 -acodec libfaac -ab 128k -vcodec libx264 -vpre ipod640 -s 480x320 -r 29 -threads 0 ${1%.*}.mp4
	mv ${1%.*}.mp4 ~/www/iphone/
} # }}}

# {{{ Rip Audio CDs to MP3
ripdatshit()
{
	echo "MP3 VBR quality setting: [0-9]"
	read $q
	mkdir $HOME/tmp/rip
	cd $HOME/tmp/rip
	cdparanoia -B
	for i in *.wav; do
		lame -V $q $i mp3/${i%.*.*}.mp3
	done
	echo "Tag mp3 files with Easytag? [y/n]"
	read $yn
	if [[ "$yn" == "y" ]]; then
		easytag $HOME/tmp/rip
	fi
} # }}}

# {{{ Create ISO from device or directory
mkiso()
{
	case $1 in
		/dev/*)
			dd if=$1 of=$2 ;;
		*)
			mkisofs -o $2 $1 ;;
	esac
} # }}}

# {{{ Setup empty github repo
mkgit() {
	mkdir $1
	cd $1
	git init
	touch README.markdown
	git add README.markdown
	git commit -m 'inital setup - automated'
	git remote add origin git@github.com:seytz/$1.git
	git push origin master
} # }}}

# {{{ Archiving - Compress/decompress various archive types with a single command
ark() {
	case $1 in

		e)
			case $2 in
				*.tar.bz2)   tar xvjf $2      ;;
				*.tar.gz)    tar xvzf $2      ;;
				*.bz2)       bunzip2 $2       ;;
				*.rar)       unrar x $2       ;;
				*.gz)        gunzip $2        ;;
				*.tar)       tar xvf $2       ;;
				*.tbz2)      tar xvjf $2      ;;
				*.tgz)       tar xvzf $2      ;;
				*.zip)       unzip $2         ;;
				*.Z)         uncompress $2    ;;
				*.7z)        7z x $2          ;;
				*)           echo "'$2' kann nicht mit >ark< entpackt werden" ;;
			esac ;;

		c)
			case $2 in
				*.tar.*)    arch=$2; shift 2;
					tar cvf ${arch%.*} $@
					case $arch in
						*.gz)   gzip -9r ${arch%.*}   ;;
						*.bz2)  bzip2 -9zv ${arch%.*} ;;
					esac                                ;;
				*.rar)      shift; rar a -m5 -r $@; rar k $1    ;;
				*.zip)      shift; zip -9r $@                   ;;
				*.7z)       shift; 7z a -mx9 $@                 ;;
				*)          echo "Kein gültiger Archivtyp"      ;;
			esac ;;

		*)
			echo "WATT?" ;;

	esac
} # }}}

# {{{ Quick Link saving for fast saves/recalls
addfile() {echo $2 >> /media/data/Filez/$1}
searchfile() {ls /media/data/Filez | grep $1}
getfile() {cat /media/data/Filez/$1 | xclip}
losefile() {rm /media/data/Filez/$1}
# }}}

# {{{ Functions for more comfortable use of DropboxCLI
# requires:
# dropbox-cli
# cush
# xclip
# ZSH named directory "~Dropbox"


# {{{ Function to switch packer/pacman-color, depending on options used
pac() {
	case $1 in

		-S | -Ss | -Ssq | -Si | -G )
			sudo packer $@ ;;

		-Su | -Syu )
			sudo packer $@
			echo "" > $HOME/.pacmanupdates ;;

		* )
			sudo pacman-color $@ ;;

	esac
} # }}}

port() {
	case $1 in
		install )
			shift
			prt-get depinst $@ ;;
		remove | lock | unlock )
			sudo prt-get $@ ;;
		new )
			mkdir $HOME/ports/$2
			chmod 777 $HOME/ports/$2
			cd $HOME/ports/$2
			touch Pkgfile ;;
		fetchup )
			sudo ports -u
			ports -d ;;
		list )
			ports -l ;;
		pre-install )
			sudo sh $(prt-get path $2)/pre-install ;;
		post-install )
			sudo sh $(prt-get path $2)/post-install ;;
		* )
			prt-get $@ ;;
	esac
}

rc.d() {
	sudo /etc/rc.d/$@
}
