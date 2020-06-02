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
