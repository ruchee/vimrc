#!/bin/bash

if [ $# -ne 3 ]; then
  echo "require 3 argument"
  exit 1
fi
orig_tigrc=$1
tmp_tigrc=$2
path_file=$3

# make temporary tigrc
cp "$orig_tigrc" "$tmp_tigrc"

# Overwriting temporary tigrc

edit_cmd='edit' # edit on existing tab
echo "bind generic e <sh -c \"echo $edit_cmd +%(lineno) %(file) > $path_file\"" >> "$tmp_tigrc"
echo "bind generic <C-o> <sh -c \"echo $edit_cmd +%(lineno) %(file) > $path_file\"" >> "$tmp_tigrc"

edit_cmd='tabedit' # edit on new tab
echo "bind generic <C-t> <sh -c \"echo $edit_cmd +%(lineno) %(file) > $path_file\"" >> "$tmp_tigrc"

edit_cmd='vsplit' # edit with vsplit
echo "bind generic <C-v> <sh -c \"echo $edit_cmd +%(lineno) %(file) > $path_file\"" >> "$tmp_tigrc"

edit_cmd='split' # edit with split
echo "bind generic <C-s> <sh -c \"echo $edit_cmd +%(lineno) %(file) > $path_file\"" >> "$tmp_tigrc"
