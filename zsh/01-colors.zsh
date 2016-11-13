# Some colors {{{
GREP_COLORS="38;5;230:sl=38;5;240:cs=38;5;100:mt=38;5;161;1:fn=38;5;197:ln=38;5;212:bn=38;5;44:se=38;5;166"
#}}}
# Ls colorz
eval $( dircolors -b $HOME/.config/zsh/LS_COLORS )

# ACK {{{
export ACK_COLOR_MATCH="cyan bold"
export ACK_COLOR_FILENAME="cyan bold on_black"
export ACK_COLOR_LINENO="bold green"
# }}}
# Export
export LS_COLORS 
