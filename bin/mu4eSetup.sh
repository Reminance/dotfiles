#!/usr/bin/bash

yay -S mbsync-git mu
mkdir -p ~/email/qq
mkdir -p ~/email/work
gpg2 --output .mbsyncpass.gpg --symmetric .mbsyncpass
gpg2 --output .authinfo.gpg --symmetric .authinfo
mbsync -a
mu init --maildir=~/email/ --my-address=872666026@qq.com --my-address=khpurat2@furva.pbz
mu index
