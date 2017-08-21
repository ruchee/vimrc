#!/bin/bash

# Intall plugins
if [[ ! -d plugins ]]; then
    mkdir plugins
    git clone https://github.com/baabelfish/vader.vim plugins/vader.vim
fi

function testExecutable() {
    echo "Testing $1"
    $1 -u rc.vim -c 'Vader! tests/**/*.vader'
    err=$?
    if [ "$err" != "0" ]; then
        cat report.log
        exit 1
    else
        echo ""
        echo "Great success!"
    fi
}

testExecutable "nvim"
testExecutable "vim"
