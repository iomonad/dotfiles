#!/bin/bash
#
# A simple script that dynamically
# setup audio depending external hardware
# is setup or not.
#

if [ -z lsusb | grep -ni "Focusrite-Novation Scarlett 18i20" ]; then
    echo "using bridged cadence"
    cadence-session-start --system-start
else
    echo "using pulseaudio"
    pulseaudio --start -D &
fi
