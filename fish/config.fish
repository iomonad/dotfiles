# GOPATH must be set to build and install packages outside the standard Go tree
# It must contain an src directory with your packages underneath.
set -x GOPATH ~/go
set -x GOBIN /usr/local/go/bin
set -x MYBIN $HOME"/bin"
set -x EDITOR /bin/ed

set -x PATH 'usr/local/bin' $PATH $GOBIN $MYBIN $GOPATH

fortune
echo ☮ ♡ ∞
