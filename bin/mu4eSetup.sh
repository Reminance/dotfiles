#!/usr/bin/bash

yay -S mbsync-git mu
mkdir -p ~/email/xc-qq
gpg2 --output .mbsyncpass.gpg --symmetric .mbsyncpass
gpg2 --output .authinfo.gpg --symmetric .authinfo
mbsync -a
mu init --maildir=~/email/
