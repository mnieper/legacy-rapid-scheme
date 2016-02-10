#!/bin/sh

OUTPUT=`chibi-scheme -I../lib -Ilib $1`
if [ -n "$OUTPUT" ]; then
    echo "$OUTPUT"
    exit 1
fi
