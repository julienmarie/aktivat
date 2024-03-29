#!/bin/sh
#
# /Library/StartupItems/redis/redis
#
# A MacOS X script to automatically start up Redis on system startup.
#
# The script assumes that Redis was installed with homebrew, adjust
# paths, etc. as needed.
#
# author: Chris Bailey (@chrisrbailey on Twitter)
#

# Source the common setup functions for startup scripts
test -r /etc/rc.common || exit 1
. /etc/rc.common

REDIS="/usr/local/bin/redis-server"
CONF_FILE="./redis2.conf"

StartService ()
{
  ConsoleMessage "Starting Redis database server"
  $REDIS $CONF_FILE
}

StopService ()
{
  ConsoleMessage "Stopping Redis database server"
  pid=`ps -o pid,command -ax | grep redis-server | awk '!/awk/ && !/grep/ {print $1}'`;
  if [ "${pid}" != "" ]; then
    kill -2 ${pid};
  fi
}

RestartService ()
{
  ConsoleMessage "Restarting Redis database server"
  StopService
  StartService
}

if test -x $REDIS ; then
  RunService "$1"
else
  ConsoleMessage "Could not find $REDIS"
fi