#!/bin/bash
### BEGIN INIT INFO
# Provides:          rtorrent
# Required-Start:    $local_fs $remote_fs $network $syslog
# Required-Stop:     $local_fs $remote_fs $network $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start/stop rtorrent daemon
### END INIT INFO

# ------------------------------------------------------------------------------
# /etc/init.d/rtorrent
#
# This script is an init script to run rtorrent in the background, using a
# screen. The script was designed and tested for Debian systems, but may work on
# other systems. On Debian, enable it by moving the script to
# "/etc/init.d/rtorrent" and issuing the command
# "update-rc.d rtorrent defaults 99"
#    ____                _ _
#   / ___|  ___  ___  __| | |__   _____  __
#   \___ \ / _ \/ _ \/ _` | '_ \ / _ \ \/ /
#    ___) |  __/  __/ (_| | |_) | (_) >  <
#   |____/ \___|\___|\__,_|_.__/ \___/_/\_\
#
# @see http://methvin.net/scripts/rtorrent
# @see http://tldp.org/LDP/abs/html/
# ------------------------------------------------------------------------------

## Username to run rtorrent under, make sure you have a .rtorrent.rc in the
## home directory of this user!
USER="killjoy"

## Absolute path to the rtorrent binary.
RTORRENT="/usr/bin/rtorrent"

## Absolute path to the screen binary.
SCREEN="/usr/bin/screen"

## Name of the screen session, you can then "screen -r rtorrent" to get it back
## to the forground and work with it on your shell.
SCREEN_NAME="rtorrent"

## Absolute path to rtorrent's PID file.
PIDFILE="/var/run/rtorrent.pid"

## Absolute path to rtorrent's XMLRPC socket.
SOCKET="/var/run/rtorrent/rpc.socket"

## Check if the socket exists and if it exists delete it.
delete_socket() {
    if [[ -e $SOCKET ]]; then
        rm -f $SOCKET
    fi
}

case "$1" in
    ## Start rtorrent in the background.
    start)
        echo "Starting rtorrent."
        delete_socket
        start-stop-daemon --start --background --oknodo \
            --pidfile "$PIDFILE" --make-pidfile \
            --chuid $USER \
            --exec $SCREEN -- -DmUS $SCREEN_NAME $RTORRENT
        if [[ $? -ne 0 ]]; then
            echo "Error: rtorrent failed to start."
            exit 1
        fi
        echo "rtorrent started successfully."
        ;;

    ## Stop rtorrent.
    stop)
        echo "Stopping rtorrent."
        start-stop-daemon --stop --oknodo --pidfile "$PIDFILE"
        if [[ $? -ne 0 ]]; then
            echo "Error: failed to stop rtorrent process."
            exit 1
        fi
        delete_socket
        echo "rtorrent stopped successfully."
        ;;

    ## Restart rtorrent.
    restart)
        "$0" stop
        sleep 1
        "$0" start || exit 1
        ;;

    ## Print usage information if the user gives an invalid option.
    *)
        echo "Usage: $0 [start|stop|restart]"
        exit 1
        ;;

esac
