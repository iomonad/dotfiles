# 03-binds.zsh

# insert sudo at the beginning of
# command input
dont_use_when_you_are_drunk () {
	zle beginning-of-line; zle -U "sudo "
}
zle -N dont-use-when-you-are-drunk dont_use_when_you_are_drunk
bindkey "^X" dont-use-when-you-are-drunk
