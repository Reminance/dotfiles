#!/bin/bash

# meant to be run with `bash -c "/path/to/wlaunch"` when running from e.g. a Windows shortcut

# explicitly needed when launching with bash -c from Windows
source ~/.bashrc
# export DISPLAY=172.22.64.1:0
export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
i3
