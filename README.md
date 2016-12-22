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
![Desktop Screenshot](https://u.teknik.io/Mn0QO.png)


```
                                                                '.
             Look like other dotfiles repo right ?             ,;c
                    --v--                                   .;l :ok0Oc
            :'oc.                                            dOd0KKKK0l
          :;xxdc. ....                                      .OxO0KKKKK0,
         'dkOkO0O000000Okdc,.                             .;O00OOOOO00KK'
         lkOKK0OOOkKXXKKK0000Od:'..   ..'';:cclddxxddxOkO000KKKKKKK0K000k.
        .kOOOOkOOOkOXKXK000KKKK000OOO00KKKKKKKKKXKXK00K0O0KKKKKKKKKKKK000,
         cxoxkO0KKkKXXXXKKKKKKKKKKKK00KKKXKK000KXXKXXXK00XKKKKKKXKKKK0K0O'
         .  ,kxk0OXXXXXXXXXXXXXXKKXXKKKKXXXKK00OO0KXXXXXXXXKKKKKXXKKKK0k,
              ... dKXXXXXXXXXXXXXXXXKXXXXXXKK00000KXKXXKKKKKKKK0KKXKKK0c
                   ;OKXXXXXXXXXXXXXXXXXXXKKKK00000KXKKKKKKKKK0000KKK0O.
                     'lk0KKXXXXNXXNXXXXXKKKKK000000KKK0KKK00000O000k;.
                         .;lOKKXXXXXXKKK00K000O00OOOOkkd0OkOOOOOOkkd,
                             'k0KKKKK0000000000O0OO0O0O0OkddxOkxdddol.
                               'dO00K000000O0OOOOOOOO00OOOkkxxxdxkxdl
                                 .:dkOOOOOOO0OOOOOOO00OOO0OOdxkkkkxoc
                                     .'oxkOOOOOOOOOOkOOOkOOkdkkxkkxc
                                         .dkkOO000OOkkOOOOkkxxxdd'.
                                          .kkOOOOOOOkkxxkkxddxo..
                                           .xOOOOOxckxdkxooll;
                                             dO000d 'dOOddxd;
                                              xO00c   xOkkxl.
                                               ldc    .,;;.
                                                '.   ...
                                                .   ..
                                                .....
                                               .',..
                                               .':..
                                             ..,c'.
                                        .....',;'.;'.
                                                                              ```
                                                                              
Some of these dotfiles are found around the web. Some can not work.
I use this repo as folder in $HOME/etc, and use symlinks to manage them.
