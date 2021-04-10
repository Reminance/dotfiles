#!/bin/bash

pgrep -x picom > /dev/null && { killall picom; exit 0; }
/bin/bash ~/.config/i3/scripts/picom.sh
