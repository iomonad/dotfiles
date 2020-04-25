# 05-git.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

if git rev-parse --is-inside-work-tree 2> /dev/null; then
    if [[ $DEBUG > 0 ]]; then
	echo "ZSH: Git repository detected"
    fi
    zstyle ':vcs_info:git:*' formats '%b'
fi

#
# Will return the current branch name
# Usage example: git pull origin $(current_branch)
#

function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function current_repository() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo $(git remote -v | cut -d':' -f 2)
}
