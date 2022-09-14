# 02-alias.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

alias die='kill -9 $$'

alias df='df -h | grep dev \
  sed -e "s_ /dev/root_\x1b[34m&\x1b[0m_" |\
  sed -e "s_[,0-9]*[MG]_\x1b[36m&\x1b[0m_" |\
  sed -e "s_[0-9]*%_\x1b[32m&\x1b[0m_" |\
  sed -e "s_9[0-9]%_\x1b[31m&\x1b[0m_" |\
  sed -e "s_/mnt/[-_A-Za-z0-9]*_\x1b[34;1m&\x1b[0m_"'

alias t='tmux'
alias ta='tmux attach -t'
alias cp='cp -r '
alias pingg='ping google.fr -c 5'
alias ls="ls --color"

alias sshu='ssh -o UserKnownHostsFile=/dev/null'
alias sr='ssh -l root'
alias sru='ssh -l root -o UserKnownHostsFile=/dev/null'

alias gcd='cd $(git rev-parse --show-toplevel)'
alias open="xdg-open"

alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'

# portage

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

alias external_call="nm -Du $1 | grep U | tr -d \"U\" | rev | grep -v \"_\" | xargs printf \"%s\n\" - | rev | sed \"1d\""

#
# Git related aliases
#

alias g='git'
compdef g=git
alias gst='git status'
compdef _git gst=git-status
alias gl='git pull'
compdef _git gl=git-pull
alias gup='git fetch && git rebase'
compdef _git gup=git-fetch
alias gp='git push'
compdef _git gp=git-push
gdv() { git diff -w "$@" | view - }
compdef _git gdv=git-diff
alias gc='git commit -v'
compdef _git gc=git-commit
alias gca='git commit -v -a'
compdef _git gca=git-commit
alias gco='git checkout'
compdef _git gco=git-checkout
alias gcm='git checkout master'
alias gb='git branch'
compdef _git gb=git-branch
alias gba='git branch -a'
compdef _git gba=git-branch
alias gcount='git shortlog -sn'
compdef gcount=git
alias gcp='git cherry-pick'
compdef _git gcp=git-cherry-pick
alias glg='git log --stat --max-count=5'
compdef _git glg=git-log
alias glgg='git log --graph --max-count=5'
compdef _git glgg=git-log
alias gss='git status -s'
compdef _git gss=git-status
alias ga='git add'
compdef _git ga=git-add
alias gm='git merge'
compdef _git gm=git-merge
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'

# Git and svn mix
alias git-svn-dcommit-push='git svn dcommit && git push github master:svntrunk'
compdef git-svn-dcommit-push=git

alias gsr='git svn rebase'
alias gsd='git svn dcommit'

alias ggpull='git pull origin $(current_branch)'
compdef ggpull=git
alias ggpush='git push origin $(current_branch)'
compdef ggpush=git
alias ggpnp='git pull origin $(current_branch) && git push origin $(current_branch)'
compdef ggpnp=git

# tools & misc

alias cat=bat

alias c=xclip
alias v="xclip -o"
