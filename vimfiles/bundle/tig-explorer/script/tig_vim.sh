#!/bin/bash

seek_tig_pid()
{
  check_tig "$1"
  if [ $? = 0 ]; then
    echo "$1"
    return
  fi

  ppid=$(parent_id "$1")
  if [ "$ppid" = "" ] || [ "$ppid" = 0 ] ; then
    exit 1
  else
    echo ""
  fi
}

parent_id(){
  ppid=$(ps --pid "$1" -o ppid -h)
  # shellcheck disable=SC2086
  echo $ppid
}

check_tig(){
  comm=$(comm "$1")
  if [ ! "$comm" = "tig" ]; then
    return 1
  fi
  return 0
}

comm(){
  comm=$(ps -p "$1" -o comm -h)
  # shellcheck disable=SC2086
  echo $comm
}

tig=$(seek_tig_pid "$PPID")

if [ ! "$tig" = "" ]; then
  # if filepath selected with `e` key in tig
  # leave selected filepath and kill parent tig process
  echo "$1 $2" > /tmp/vim_tig_current_file
  kill "$tig"
else # open vim as a child process without killing tig
  # shellcheck disable=SC2086
  vim $1 $2
fi
