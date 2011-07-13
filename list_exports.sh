#!/bin/sh

cat << EOF
import(ggplot2, Hmisc, stats)

#
# To generate the list of exported functions, run list_exports.sh.
#
EOF

echo 'export('

egrep '^[\.a-zA-Z0-9]+ <- function' R/*.R |
     awk '{ print $1; }' |
     awk 'BEGIN { FS=":"; OFS=""; } { print "  ", $2; }' |
     grep -v 'pcor.' |
     sort -f |
     sed '$q;s/$/,/g'

echo ')'
