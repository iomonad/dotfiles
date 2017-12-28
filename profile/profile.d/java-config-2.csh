# Copyright 1999-2013 Gentoo Foundation
# Distributed under the terms of the GNU General Public License, v2 or later

set gentoo_user_vm = "${HOME}/.gentoo/java-config-2/current-user-vm"
set gentoo_system_vm = "/etc/java-config-2/current-system-vm"

## If we have a current-user-vm (and aren't root)... set it to JAVA_HOME
## Otherwise set to the current system vm
if ( ( "$uid" != "0" ) && ( -l $gentoo_user_vm ) ) then
    setenv JAVA_HOME $gentoo_user_vm
else if ( -l $gentoo_system_vm ) then
    setenv JAVA_HOME $gentoo_system_vm
endif
unset gentoo_user_vm gentoo_system_vm

if ( $?JAVA_HOME ) then
	# prepending to come before generation 1
	setenv MANPATH "${JAVA_HOME}/man:${MANPATH}"
	setenv JDK_HOME $JAVA_HOME
	setenv JAVAC ${JDK_HOME}/bin/javac
endif
