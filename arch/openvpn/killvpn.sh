#!/usr/bin/bash
ps -ef | grep -v 'grep\|pts' | grep openvpn | awk '{print $2}' | xargs -i sudo kill -9 {}
