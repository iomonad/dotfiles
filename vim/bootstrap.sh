#!/bin/bash
# File: bootstrap.sh
# Author: Clement Trosa <me@trosa.io>
# Date: 27/06/2017 08:48:35 PM
# Last Modified: 27/06/2017 08:57:10 PM

export VIMHOME=$HOME/.vim

mv $VIMHOME $HOME/.vim.bak || echo "No vim configuration to backup now"
git clone --depth 1 https://github.com/iomonad/vim $VIMHOME
cd $VIMHOME ; ln -sf $VIMHOME/.vimrc $HOME/.vimrc
vim -E -c PlugInstall -c q
