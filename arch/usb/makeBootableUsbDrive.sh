#!/usr/bin/bash

# Find the USB route
sudo fdisk -l

# Format the USB drive
sudo pacman -S dosfstools
sudo umount /dev/sdb
sudo mkfs.vfat /dev/sdb

# Write the .iso image
sudo dd bs=4M if=/root/media/archlinux-2017.11.01-x86_64.iso of=/dev/sdb
sync

