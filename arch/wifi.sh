#!/usr/bin/bash

ip link #查看网络接口
rfkill unblock wifi #取消禁用wifi设备
ip link set wlan0 up #开启wlan0

#输入iwctl进入交互式提示符
station wlan0 scan
station wlan0 get-networks
station wlan0 connect <network name>
station wlan0 show
exit #回到命令行
