# 02-abbr.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

typeset -Ag abbreviations

abbreviations=(
	"Im"    "| more"
	"Ia"    "| awk"
	"Ig"    "| grep"
	"Ieg"   "| egrep"
	"Iag"   "| agrep"
	"Ip"    "| $PAGER"
	"Ih"    "| head"
	"Ik"    "| keep"
	"It"    "| tail"
	"Is"    "| sort"
	"Iw"    "| wc"
	"Ix"    "| xargs"
)

magic-abbrev-expand() {
    local MATCH
    LBUFFER=${LBUFFER%%(#m)[_a-zA-Z0-9]#}
    LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
    zle self-insert
}

no-magic-abbrev-expand() {
  LBUFFER+=' '
}

zle -N magic-abbrev-expand
zle -N no-magic-abbrev-expand

# Bind space to abbrev trigger
bindkey " " magic-abbrev-expand
