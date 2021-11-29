# 02-syntax.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# Syntax Highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

#
# Install dependencies:
#  $ emerge -a app-shells/zsh-syntax-highlighting
#

case $(cat /etc/os-release | head -n 1 | cut -d \" -f 2) in
    "Arch Linux")
	. /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	;;
    "Gentoo Linux")
        .  /usr/share/zsh/site-functions/zsh-syntax-highlighting.zsh
	;;
esac


# STYLES
# Aliases and functions
ZSH_HIGHLIGHT_STYLES[alias]='fg=blue,bold'
ZSH_HIGHLIGHT_STYLES[function]='fg=blue,bold'

# Commands and builtins
ZSH_HIGHLIGHT_STYLES[command]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[hashed-command]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[builtin]="fg=blue,bold"

# Paths
ZSH_HIGHLIGHT_STYLES[path]='fg=white'

# Globbing
ZSH_HIGHLIGHT_STYLES[globbing]='fg=yellow,bold'

# Options and arguments
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=red'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=red'

ZSH_HIGHLIGHT_STYLES[back-quoted-argument]="fg=green"
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=green"
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=green"
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]="fg=green"
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]="fg=green"

# PATTERNS
# rm -rf
ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')

# Sudo
ZSH_HIGHLIGHT_PATTERNS+=('sudo ' 'fg=white,bold,bg=red')
