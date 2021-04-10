#!/usr/bin/bash

[[ -z "$1" ]] && echo "USAGE: sudo ./mount.sh mount/umount" && exit 2
[[ ! "$1" =~ mount|umount ]] && echo "USAGE: sudo ./mount.sh mount/umount" && exit 2
[[ -d /run/media/xc/G ]] || echo "init mount dir"; mkdir -p /run/media/xc/G
if [[ "$1" == "mount" ]]; then
    mount /dev/sdc1 /run/media/xc/G
elif [[ "$1" == "umount" ]]; then
    umount /run/media/xc/G
else
    exit 2
fi

