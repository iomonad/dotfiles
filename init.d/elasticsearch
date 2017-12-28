#! /bin/bash
### BEGIN INIT INFO
# Provides:          elasticsearch
# Required-Start:    $all
# Required-Stop:     $all
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Starts elasticsearch
# Description:       Starts elasticsearch using start-stop-daemon
### END INIT INFO

# You may need to change these
USER=elasticsearch          # the user you used to run the elastic search install command
JAVA_HOME=/usr/local/java   # Where java lives

### BEGIN user-configurable settings
NAME=elasticsearch
DESC=elasticsearch
ES_HOME=/home/$USER/elasticsearch
PID_FILE=$ES_HOME/$NAME.pid
LOG_DIR=$ES_HOME/logs
DATA_DIR=$ES_HOME/data
CONFIG_FILE=$ES_HOME/config/elasticsearch.yml
ES_MIN_MEM=256m
ES_MAX_MEM=2g
WORK_DIR=/tmp/$NAME
DAEMON=$ES_HOME/bin/elasticsearch
DAEMON_OPTS="-p $PID_FILE -Des.config=$CONFIG_FILE -Des.path.home=$ES_HOME -Des.path.logs=$LOG_DIR -Des.path.data=$DATA_DIR -Des.path.work=$WORK_DIR"
### END user-configurable settings


# Pull in RHEL/CentOS-specific functionality (daemon, killproc) if available.
if [ -x /etc/init.d/functions ]; then source /etc/init.d/functions; fi

# Exit if the executable is missing.
if [ ! -x $DAEMON ]; then
  echo 'Could not find elasticsearch executable!'
  exit 0
fi

# Exit if any command (outside a conditional) fails.
set -e


case "$1" in
  start)
    echo -n "Starting $DESC: "
    mkdir -p $LOG_DIR $DATA_DIR $WORK_DIR
    chown -R $USER:$USER $LOG_DIR $DATA_DIR $WORK_DIR
    if type -p start-stop-daemon > /dev/null; then
      start-stop-daemon --start --pidfile $PID_FILE --user $USER --chuid $USER --startas $DAEMON -- $DAEMON_OPTS
    else
      runuser -s /bin/bash $USER -c "$DAEMON $DAEMON_OPTS"
      #daemon --pidfile $PID_FILE --user $USER $DAEMON $DAEMON_OPTS
    fi
    if [ $? == 0 ]
    then
        echo "started."
    else
        echo "failed."
    fi
    ;;
  stop)
    if [ ! -e $PID_FILE ]; then
      echo "$DESC not running (no PID file)"
    else
      echo -n "Stopping $DESC: "
      if type -p start-stop-daemon > /dev/null; then
        start-stop-daemon --stop --pidfile $PID_FILE
      else
        kill $(cat $PID_FILE)
        rm $PID_FILE
      fi
      if [ $? == 0 ]
      then
          echo "stopped."
      else
          echo "failed."
      fi
    fi
    ;;
  restart|force-reload)
    ${0} stop
    sleep 0.5
    ${0} start
    ;;
  status)
    if [ ! -f $PID_FILE ]; then
      echo "$DESC not running"
    else
      if ps auxw | grep $(cat $PID_FILE) | grep -v grep > /dev/null; then
        echo "running on pid $(cat $PID_FILE)"
      else
        echo 'not running (but PID file exists)'
      fi
    fi
    ;;
  *)
    N=/etc/init.d/$NAME
    echo "Usage: $N {start|stop|restart|force-reload|status}" >&2
    exit 1
    ;;
esac

exit 0
