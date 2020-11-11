#!/bin/sh

if [ $# -ne 7 ]; then
  echo "require 7 argument"
  exit 1
fi
orig_tigrc=$1
tmp_tigrc=$2
path_file=$3
keymap_edit=$4
keymap_tabedit=$5
keymap_split=$6
keymap_vsplit=$7

# make temporary tigrc
cp "$orig_tigrc" "$tmp_tigrc"

# Overwriting temporary tigrc

# $1: 'keymap'
# $2: 'edit_cmd'
add_custom_command() {
  echo "bind generic $1 <sh -c \"echo $2 +%(lineno) %(file) > $path_file\"" >> "$tmp_tigrc"
  case $2 in
    tabedit) command="tab TigOpenFileWithCommit";;  
    split) command="TigOpenFileWithCommit!";; 
    vsplit) command="vertical TigOpenFileWithCommit!";; 
    *) command="TigOpenFileWithCommit";; 
  esac

  echo "bind tree $1 <sh -c \"echo $command %(commit) %(file) %(lineno) > $path_file\"" >> "$tmp_tigrc"
  for keymap in blame refs main diff
  do
    echo "bind $keymap $1 <sh -c \"echo $command %(commit) % %(lineno) > $path_file\"" >> "$tmp_tigrc"
  done
}

add_custom_command "e"               "edit"
add_custom_command "$keymap_edit"    "edit"
add_custom_command "$keymap_tabedit" "tabedit"
add_custom_command "$keymap_split"   "split"
add_custom_command "$keymap_vsplit"  "vsplit"
