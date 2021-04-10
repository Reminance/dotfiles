#!/usr/bin/expect
spawn sudo openvpn --client --config "[lrange $argv 0 0]" --ca "[lrange $argv 0 0]-ca.crt"
sleep 1
expect "*密码*"
send "1\r"
sleep 1
expect "*Password*"
send "[lrange $argv 1 1]\r"
interact
