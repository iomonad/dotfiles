# 05-devkits.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

#
# GNUPG
#

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

#
# SDKMAN
#

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

#
# Kubernetes
#

if command -v kubectl &> /dev/null
then
    if [[ $DEBUG > 0 ]]; then
	echo "ZSH: Initialzed kubernetes module"
    fi

    source <(kubectl completion zsh)

    # env
    export KUBECTL_EXTERNAL_DIFF=colordiff
    export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

    # aliases
    alias kctx=kubectx
    alias kns=kubens
fi

# NixPKG

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    if [[ $DEBUG > 0 ]]; then
	echo "ZSH: Initialzed NixPKGs"
    fi
    . $HOME/.nix-profile/etc/profile.d/nix.sh;
fi
