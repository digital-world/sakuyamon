#!/bin/sh

### whether Sakuyamon is eating the CPU core
# return 0 means eating, otherwise means clear.
#
# this is an ugly workaround, use at your risk.
###

sakuyamon=`ps -u tamer -o pid,args | grep 'real$' | awk '{print $1}'`;
test -z "${sakuyamon}" && exit 0;

prstat -c 0 1 | grep "${sakuyamon}" | grep "cpu";
test $? -eq 0 && kill -TERM ${sakuyamon};

