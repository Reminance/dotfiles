#!/bin/bash
rsync -axv --delete --include-from=/include.list --exclude-from=/exclude.list / /run/media/xc/G/arch-backup/$1/
#
