#!/bin/sh

echo 'export('

egrep '^[\.a-zA-Z]+ <- function' R/*.R |
     awk '{ print $1; }' |
     awk 'BEGIN { FS=":"; OFS=""; } { print "  ", $2; }' |
     grep -v 'pcor.' |
     sort |
     sed '$q;s/$/,/g'

echo ')'
