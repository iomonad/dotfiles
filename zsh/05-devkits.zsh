# 05-devkits.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# NIX
if [[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

# GO
export GOPATH=$HOME/Devel/go
export GOROOT=/usr/lib/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOPATH
export PATH=$PATH:$GOROOT/bin


# GNUPG

AGENT_SOCK=$(gpgconf --list-dirs | grep agent-socket | cut -d : -f 2)

if [[ ! -S $AGENT_SOCK ]]; then
  gpg-agent --daemon --use-standard-socket &>/dev/null
fi

export GPG_TTY=$(tty)
export GPG_TTY=$TTY

# Set SSH to use gpg-agent if it's enabled
GNUPGCONFIG="${GNUPGHOME:-"$HOME/.gnupg"}/gpg-agent.conf"
if [[ -r $GNUPGCONFIG ]] && command grep -q enable-ssh-support "$GNUPGCONFIG"; then
  export SSH_AUTH_SOCK="$AGENT_SOCK.ssh"
  unset SSH_AGENT_PID
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
