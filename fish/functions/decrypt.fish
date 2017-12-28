function decrypt
	openssl enc -d -salt -aes-256-cbc -in $argv[1]
end
