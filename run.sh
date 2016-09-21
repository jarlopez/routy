#!/usr/bin/env bash
HOST='j@2001:6b0:1:1041:e888:f9d6:6ce6:bf413'
if [ ! -z "$1" ]; then
    HOST="${$1}@${$2}"
fi

erl -name ${HOST} -setcookie hemlis -connect_all false
