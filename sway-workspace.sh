#!/bin/bash

function usage {
  echo "Usage: $0 [-n]  [-h] [-w WS] [DIR]"
  echo "  DIR    Use DIR as session directory. Defaults to directory used for"
  echo "         last nvim session if not specified."
  echo "  -n     create new window for nvim defaults to using current terminal"
  echo "  -h     show this help."
  echo "  -w WS  which workspace to open the session in, defaults to current."
  exit $1
}

DIR=""
NEW_WIN=false
WS=""

while getopts "hnw:" opt; do
  case $opt in
    n)
      NEW_WIN=true
      ;;
    h)
      usage 0
      ;;
    w)
      WS=$OPTARG
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      usage 1
      ;;
  esac
done

shift $((OPTIND - 1))

if [ -z "$1" ]; then
  LAST_SESSION=$(/bin/ls -1rt /home/nuchs/.local/share/nvim/sessions | tail -1)
  DIR=${LAST_SESSION//__/\/}
elif [ "$1" == "." ]; then
  DIR="$PWD"
else
  DIR=$1
fi

echo "Starting workspace in $DIR in workspace '$WS' (new window? $NEW_WIN)"

if [ -n "$WS" ]; then
  swaymsg move to workspace $WS
  swaymsg workspace $WS
fi

if [ "$NEW_WIN" == true ]; then
  swaymsg exec "foot -D $DIR" 
fi

swaymsg split horizontal
swaymsg exec "foot -D $DIR"
sleep 0.1s
swaymsg move left
swaymsg resize shrink width 30
swaymsg splitt
swaymsg exec "foot -D $DIR"
swaymsg layout stacking
sleep 0.1s
swaymsg splitt
swaymsg exec "foot -D $DIR"
sleep 0.1s
swaymsg focus right

if [ "$NEW_WIN" == true ]; then
  wtype cd $DIR -k return
  wtype nvim -k return
else
  cd $DIR
  nvim
fi
