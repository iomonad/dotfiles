#
# Install unit in home
#

install:
	mkdir -p ${HOME}/.config/systemd/user/
	cp org* ${HOME}/.config/systemd/user/
	systemctl enable --user org-sync
	systemctl start --user org-sync

remove:
	rm ~/.config/systemd/user/org*
	systemctl disable --user org-sync
	systemctl stop --user org-sync
