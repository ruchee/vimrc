#!/bin/sh

if [ $# -ne 12 ]; then
  echo "require 12 argument"
  exit 1
fi
orig_tigrc=$1
tmp_tigrc=$2
path_file=$3
keymap_edit_e=$4
keymap_edit=$5
keymap_tabedit=$6
keymap_split=$7
keymap_vsplit=$8
keymap_commit_edit=$9
keymap_commit_tabedit=${10}
keymap_commit_split=${11}
keymap_commit_vsplit=${12}

# make temporary tigrc
cp "$orig_tigrc" "$tmp_tigrc"

# Overwriting temporary tigrc


add_custom_cmd() {
  view=$1
  keymap=$2
  cmd=$3
  echo "bind $view $keymap <sh -c \"echo $cmd +%(lineno) %(file) > $path_file\"" >> "$tmp_tigrc"
}

add_current_with_commit_cmd() {
  view=$1
  keymap=$2
  cmd=$3
  echo "bind $view $keymap <sh -c \"echo $cmd %(commit) % %(lineno) > $path_file\"" >> "$tmp_tigrc"
}

add_selected_with_commit_cmd() {
  view=$1
  keymap=$2
  cmd=$3
  echo "bind $view $keymap <sh -c \"echo $cmd %(commit) %(file) %(lineno) > $path_file\"" >> "$tmp_tigrc"
}

if [ $keymap_edit_e != "" ]; then
  add_custom_cmd "generic" "$keymap_edit_e"  "edit"
fi

add_custom_cmd "generic" "$keymap_edit"    "edit"
add_custom_cmd "generic" "$keymap_tabedit" "tabedit"
add_custom_cmd "generic" "$keymap_split"   "split"
add_custom_cmd "generic" "$keymap_vsplit"  "vsplit"

add_current_with_commit_cmd "refs" "$keymap_commit_edit"    "TigOpenFileWithCommit"
add_current_with_commit_cmd "refs" "$keymap_commit_tabedit" "tab TigOpenFileWithCommit"
add_current_with_commit_cmd "refs" "$keymap_commit_split"   "TigOpenFileWithCommit!"
add_current_with_commit_cmd "refs" "$keymap_commit_vsplit"  "vertical TigOpenFileWithCommit!"

add_current_with_commit_cmd "main" "$keymap_commit_edit"    "TigOpenFileWithCommit"
add_current_with_commit_cmd "main" "$keymap_commit_tabedit" "tab TigOpenFileWithCommit"
add_current_with_commit_cmd "main" "$keymap_commit_split"   "TigOpenFileWithCommit!"
add_current_with_commit_cmd "main" "$keymap_commit_vsplit"  "vertical TigOpenFileWithCommit!"

add_selected_with_commit_cmd "blame" "$keymap_commit_edit"    "TigOpenFileWithCommit"
add_selected_with_commit_cmd "blame" "$keymap_commit_tabedit" "tab TigOpenFileWithCommit"
add_selected_with_commit_cmd "blame" "$keymap_commit_split"   "TigOpenFileWithCommit!"
add_selected_with_commit_cmd "blame" "$keymap_commit_vsplit"  "vertical TigOpenFileWithCommit!"

add_selected_with_commit_cmd "diff" "$keymap_commit_edit"    "TigOpenFileWithCommit"
add_selected_with_commit_cmd "diff" "$keymap_commit_tabedit" "tab TigOpenFileWithCommit"
add_selected_with_commit_cmd "diff" "$keymap_commit_split"   "TigOpenFileWithCommit!"
add_selected_with_commit_cmd "diff" "$keymap_commit_vsplit"  "vertical TigOpenFileWithCommit!"

add_selected_with_commit_cmd "tree" "$keymap_commit_edit"    "TigOpenFileWithCommit"
add_selected_with_commit_cmd "tree" "$keymap_commit_tabedit" "tab TigOpenFileWithCommit"
add_selected_with_commit_cmd "tree" "$keymap_commit_split"   "TigOpenFileWithCommit!"
add_selected_with_commit_cmd "tree" "$keymap_commit_vsplit"  "vertical TigOpenFileWithCommit!"
