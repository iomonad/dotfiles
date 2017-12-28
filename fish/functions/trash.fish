function trash
	for f in $argv
		mv "$f" $HOME/.Trash
	end
end
