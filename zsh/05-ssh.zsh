# 05-ssh.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

if [[ ${SSH_CONNECTION} ]]; then
    PROMPT=' %B(ssh@%m) %F{red}» %f'
else
    ;
fi
