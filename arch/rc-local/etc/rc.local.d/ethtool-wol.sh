#!/usr/bin/bash
# 由于启动时无感知网卡是否启动 可能获取微空的网卡设备  遂使用systemd开启wol
#device=`ifconfig|grep enp|awk -F ":" 'NR==1{print $1}'`
#sudo ethtool -s $device wol g
#echo "DEVICE($device) ACTIVE WakeOnLAN DATE:" $(date +"%Y-%m-%d %H:%M:%S") >> /etc/rc.local.d/logs/eth-wol-active.log
