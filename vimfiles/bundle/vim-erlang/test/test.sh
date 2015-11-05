#!/bin/sh

#
# Uncomment the DEBUG macro from erlang_indent.erl
#

if [ $# != 1 ]
then
	echo "Usage: $0 <file>" >&2
	exit 1
fi

INDENTER=$(dirname $0)/../indent/erlang_indent.erl
LAST_LINE=$(wc -l $1 | cut -d ' ' -f 1)

for LINE in $(seq 1 $LAST_LINE)
do
	echo -n Line: $LINE '==> '
	$INDENTER -f $1 $LINE
done
