#!/bin/bash
if [ -z "$1" ]; then 
  NODE="master"
else
  NODE=$1
fi
if [ -z "$2" ]; then 
  PORT="8001"
else
  PORT=$2
fi
if [ -z "$3" ]; then 
  COOKIE="cookiemonster"
else
  COOKIE=$3
fi
sudo iex --sname $NODE --cookie $COOKIE --erl " -gproc gproc_dist all  -trackrport $PORT"  -S mix 