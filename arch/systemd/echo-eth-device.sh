#!/usr/bin/bash
echo -e "`ifconfig|grep enp|awk -F ":" 'NR==1{print $1}'`"
