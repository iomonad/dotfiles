function fish_prompt --description 'Write out the prompt'
	set -l home_escaped (echo -n $HOME | sed 's/\//\\\\\//g')
   set -l pwd (echo -n $PWD | sed "s/^$home_escaped/~/" | sed 's/ /%20/g')
   set -l prompt_symbol ''
   switch $USER
       case root; set prompt_symbol '#'
       case '*';  set prompt_symbol '$'
   end
   printf "\n[ %s@%s:%s%s%s ]\n%s " (set_color purple)$USER (set_color blue)(hostname -s) (set_color $fish_color_cwd) $pwd (set_color normal) $prompt_symbol
end
