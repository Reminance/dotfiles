#!/usr/bin/bash
cd ~/.config/arch/openvpn
sleep 1
./generate-cert.sh xuc.ovpn
sleep 1
nohup ./run-openvpn.sh xuc.ovpn xc2020 &
