#!/bin/bash
while inotifywait -r -q -e close_write "../autoload" "."
do
    ./run_tests.sh
    sleep 1
done
