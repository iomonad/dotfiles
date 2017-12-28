# /etc/profile: login shell setup
#
# That this file is used by any Bourne-shell derivative to setup the
# environment for login shells.
#

# Load environment settings from profile.env, which is created by
# env-update from the files in /etc/env.d
if [ -e /etc/profile.env ] ; then
	. /etc/profile.env
fi

# You should override these in your ~/.bashrc (or equivalent) for per-user
# settings.  For system defaults, you can add a new file in /etc/profile.d/.
export EDITOR=${EDITOR:-/bin/nano}
export PAGER=${PAGER:-/usr/bin/less}

# 077 would be more secure, but 022 is generally quite realistic
umask 022

# Set up PATH depending on whether we're root or a normal user.
# There's no real reason to exclude sbin paths from the normal user,
# but it can make tab-completion easier when they aren't in the
# user's PATH to pollute the executable namespace.
if [ "${EUID-}" = "0" ] || [ "${USER-}" = "root" ] ; then
	PATH="${ROOTPATH}"
fi
export PATH
unset ROOTPATH

if [ -n "${BASH_VERSION-}" ] ; then
	# Newer bash ebuilds include /etc/bash/bashrc which will setup PS1
	# including color.  We leave out color here because not all
	# terminals support it.
	if [ -f /etc/bash/bashrc ] ; then
		# Bash login shells run only /etc/profile
		# Bash non-login shells run only /etc/bash/bashrc
		# Since we want to run /etc/bash/bashrc regardless, we source it 
		# from here.  It is unfortunate that there is no way to do 
		# this *after* the user's .bash_profile runs (without putting 
		# it in the user's dot-files), but it shouldn't make any 
		# difference.
		. /etc/bash/bashrc
	else
		PS1='\u@\h \w \$ '
	fi
else
	# Setup a bland default prompt.  Since this prompt should be useable
	# on color and non-color terminals, as well as shells that don't
	# understand sequences such as \h, don't put anything special in it.
	PS1="${USER:-$(whoami 2>/dev/null)}@$(uname -n 2>/dev/null) \$ "
fi

for sh in /etc/profile.d/*.sh ; do
	[ -r "$sh" ] && . "$sh"
done
unset sh
