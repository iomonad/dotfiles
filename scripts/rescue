#! /bin/bash

domount ()
{
	if [ -d $1 ]; then
		cd $1
		mount -t proc none proc
		mount -t sysfs none sys
		mount -o bind /dev dev
		mount -t devpts none -o gid=5 dev/pts
		mount -o bind /usr/portage usr/portage
		mount -o bind /usr/src usr/src
		mount -o bind /var/tmp var/tmp
		cd -
	else
		echo "directory $1 doesn't exist"
	fi
}

doumount()
{
	if [ -d $1 ]; then
		cd $1
		umount proc sys dev/pts dev usr/portage usr/src var/tmp
		cd -
	else
		echo "directory $1 doesn't exist"
	fi
}

if [ $# != 2 ]; then
	echo $0 "<mount|umount> <directory>"
else
	if [ "x"$1 == "xmount" ]; then
		domount $2
	elif [ "x"$1 == "xumount" ]; then
		doumount $2
	else
		echo $0 "<mount|umount> <directory>"
	fi
fi

