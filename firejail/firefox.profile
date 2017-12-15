# Firejail profile for Mozilla Firefox

noblacklist ~/.mozilla
noblacklist ~/.cache/mozilla
include /etc/firejail/disable-common.inc
include /etc/firejail/disable-programs.inc
include /etc/firejail/disable-devel.inc

caps.drop all
seccomp
protocol unix,inet,inet6,netlink
netfilter
tracelog
nonewprivs
noroot

whitelist ${DOWNLOADS}
mkdir ~/.mozilla
whitelist ~/.mozilla
mkdir ~/.cache
mkdir ~/.cache/mozilla
mkdir ~/.cache/mozilla/firefox
whitelist ~/.cache/mozilla/firefox
whitelist ~/.vimperatorrc
whitelist ~/.vimperator
whitelist ~/.bin
read-only ~/.bin
whitelist ~/code/
read-only ~/code/
whitelist ~/.cache/gnome-mplayer/plugin
whitelist ~/.pki

include /etc/firejail/whitelist-common.inc

# x11
# experimental features
