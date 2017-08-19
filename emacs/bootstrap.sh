# Filename: bootstrap.sh
# Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
# 
# Last-Updated: 04/09/2017 Sunday 23:26:32
# Description: Install configurations

mv $HOME/.emacs.d/ $HOME/.emacs.backup
git clone https://github.com/iomonad/emacs.d ~/.emacs.d/

emacs
