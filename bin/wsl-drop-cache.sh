#!/usr/bin/bash
sudo pacman -S cronie
sudo crontab -e -u root
# */15 * * * * sync; echo 3 > /proc/sys/vm/drop_caches; touch /root/drop_caches_last_run

# command below should be in bashrc or zshrc
[ -z "$(ps -ef | grep cron | grep -v grep)" ] && sudo crond start &> /dev/null
