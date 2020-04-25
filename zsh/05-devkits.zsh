# 05-devkits.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

# NIX
if [[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

# GO
export GOPATH=$HOME/go
export GOROOT=/usr/local/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOPATH
export PATH=$PATH:$GOROOT/bin
