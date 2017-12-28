function encrypt
	openssl enc -aes-256-cbc -salt -in $argv[1] -out $argv[2] 
end

