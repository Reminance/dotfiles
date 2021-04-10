#!/usr/bin/bash

if [ ! -s ~/fortivpn-config.sh ]; then echo -e "vpn config file not exists\n";exit 1; fi
sudo openfortivpn -c /home/xc/fortivpn-config.sh
