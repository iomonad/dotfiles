# 02-alias.zsh

alias ls="ls++"

alias die='kill -9 $$'

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
alias pingb='ping 192.168.0.254 -c 5''""''"""""""""""]"'

alias radio="mplayer http://radio.2f30.org:8000/live.mp3"
alias radio-trinitas="mpv http://live.radiotrinitas.ro:8003"
alias radio-uzic="mplayer http://www.uzic.ch/tek.m3u"
alias radio-trance="mplayer http://streaming.radionomy.com:8000/Trance-Libre-webradio.m3u"

alias sb="stack build"
alias st="stack test"
alias sr="stack resolv"
alias se="stack exec"
alias sg="stack ghci"
alias sd="stack list-dependencies"

alias e="emacs -nw"
alias es="emacs --daemon"
alias ec="emacsclient -c"

alias sshu='ssh -o UserKnownHostsFile=/dev/null'
alias sr='ssh -l root'
alias sru='ssh -l root -o UserKnownHostsFile=/dev/null'

alias g="git"
alias open="xdg-open"

alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'

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

alias eqf='equery f'
alias equ='equery u'
alias eqh='equery h'
alias eqa='equery a'
alias eqb='equery b'
alias eql='equery l'
alias eqd='equery d'
alias eqg='equery g'
alias eqc='equery c'
alias eqk='equery k'
alias eqm='equery m'
alias eqy='equery y'
alias eqs='equery s'
alias eqw='equery w'

alias make="make -s"

alias external_call="nm -Du $1 | grep U | tr -d \"U\" | rev | grep -v \"_\" | xargs printf \"%s\n\" - | rev | sed \"1d\""
