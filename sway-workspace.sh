#!/bin/bash

if [ -n "$1" ]; then
  DIR="$1"

  cd $DIR
else
  LAST_SESSION=$(/bin/ls -1rt /home/nuchs/.local/share/nvim/sessions | tail -1)
  DIR=${LAST_SESSION//__/\/}

  swaymsg workspace 1
  swaymsg exec "foot -D $DIR nvim" 
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

if [ -n "$1" ]; then
  nvim
fi
