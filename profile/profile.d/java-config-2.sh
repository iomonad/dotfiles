# Copyright 1999-2013 Gentoo Foundation
# Distributed under the terms of the GNU General Public License, v2 or later

# If we have a current-user-vm (and aren't root)... set it to JAVA_HOME
gentoo_user_vm="${HOME}/.gentoo/java-config-2/current-user-vm"
gentoo_system_vm="/etc/java-config-2/current-system-vm"

# Please make sure that this script is POSIX compliant
# See https://bugs.gentoo.org/show_bug.cgi?id=169925
# for more details"

if [ -z "${UID}" ] ; then
	# id lives in /usr/bin which might not be mounted
	if type id >/dev/null 2>/dev/null ; then
		user_id=$(id -u)
	else
		[ "${USER}" = "root" ] && user_id=0
	fi
fi

# The root user uses the system vm
if [ "${user_id}" != 0 -a -L "${gentoo_user_vm}" ]; then
	export JAVA_HOME=${gentoo_user_vm}
# Otherwise set to the current system vm
elif [ -L "/etc/java-config-2/current-system-vm" ]; then
	export JAVA_HOME=${gentoo_system_vm}
fi

# prepending to come before generation 1
export MANPATH="${JAVA_HOME}/man:${MANPATH}"
export JDK_HOME=${JAVA_HOME}
export JAVAC=${JDK_HOME}/bin/javac
unset gentoo_user_vm gentoo_system_vm user_id
