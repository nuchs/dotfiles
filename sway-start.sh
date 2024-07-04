#!/bin/bash

LAST_SESSION=$(/bin/ls -1rt /home/nuchs/.local/share/nvim/sessions | tail -1)
LAST_DIR=${LAST_SESSION//__/\/}

swaymsg workspace 1
swaymsg exec "foot -D $LAST_DIR nvim" 
swaymsg split horizontal
swaymsg exec foot
sleep 0.1
swaymsg move left
swaymsg resize shrink width 30
swaymsg splitt
swaymsg exec foot
swaymsg layout stacking
sleep 0.1
swaymsg splitt
swaymsg exec foot
