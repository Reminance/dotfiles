#!/bin/bash

# /usr/bin/amixer set Master toggle
pactl set-sink-mute @DEFAULT_SINK@ toggle
# bash ~/.config/dwm/scripts/dwm-status-refresh.sh
