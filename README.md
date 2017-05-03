# ~/.* Dotfiles
-------
> Some dotfiles to use as configurations files for cli programs.

### Installation
```bash
$ git clone https://github.com/iomonad/dotfiles.git $HOME/etc

# Move to the repo
$ cd $HOME/etc

# Make some symlinks. Ex,
$ ln -s $HOME/etc/ssh $HOME/.ssh
```

### Special Note for Emacs
> Emacs is managed in an external repo, to install it:

```bash
# Initialize submodules
$ git submodule init

# Update repo
$ git submodule update

# Make symlinks
$ ln -s $HOME/etc/emacs $HOME/.emacs.d
```

### Keep it up to date
```bash
# To update dotfiles, just execute these commands
$ git pull

# For emacs
$ git fetch ; git merge origin/master
```
### Current setup:
![Desktop Screenshot](https://my.mixtape.moe/dyqjgu.png)
