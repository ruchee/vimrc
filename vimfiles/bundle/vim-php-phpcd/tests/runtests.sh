#!/bin/bash

set -eu

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# vimunit in the plugin root's parent dir (think of ~/.vim/bundle)
VU="$DIR/../../vimunit/vutest.sh"

if [ ! -f "$VU" ]; then
	echo "Could not run tests. Vimunit executeable not found at: '$VU'"
else
    cd ./fixtures
    if [[ $# > 0 ]]; then
        for f in $@; do
            $VU -e "nvim -u $DIR/vimrc" $f
        done
    else
        for f in "$DIR/"*.vim; do
		echo $DIR
            $VU -e "nvim -u $DIR/vimrc" $f
        done
    fi
fi
