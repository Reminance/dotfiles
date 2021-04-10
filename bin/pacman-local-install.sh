#!/usr/bin/bash

# 对于Archlinux及类系统，对已经安装的软件进行备份是一个很好的习惯，能够在我们系统出问题或者重装一次性还原系统所需软件。
# 首先生成软件包列表：

date_format_packages=~/.config/arch/packages-backup/packages-`date '+%Y%m%d%H%M%S'`.txt
date_format_packages_aur=~/.config/arch/packages-backup/packages-aur-`date '+%Y%m%d%H%M%S'`.txt
latest_packages=~/.config/arch/packages-backup/packages-latest.txt
latest_packages_aur=~/.config/arch/packages-backup/packages-aur-latest.txt
sudo pacman -Qqen > $date_format_packages
sudo pacman -Qqem > $date_format_packages_aur
cp $date_format_packages $latest_packages
cp $date_format_packages_aur $latest_packages_aur

# 重新安装：
# cat ~/.config/arch/packages-backup/packages-aur-latest.txt | xargs yaourt -S --needed --noconfirm
# sudo pacman --needed -S - < ~/.config/arch/packages-backup/packages-latest.txt
# cat /home/xc/.config/arch/packages-backup/packages-aur-latest.txt | xargs yay -S

# 清理多余软件包
# yaourt -R `pacman -Qdqt`

