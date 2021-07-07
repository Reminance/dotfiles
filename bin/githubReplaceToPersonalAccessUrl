#!/usr/bin/bash
git remote -v || exit 1
echo
if [ ! -s ~/github.token ]; then echo -e "token not exists\n";exit 1; fi
token=$(cat ~/github.token)
echo -e "token:$token\n"
origin_url=$(git remote -v | awk 'NR == 1'  | awk {'print $2'})
echo -e "origin_url:$origin_url\n"
replace=$(echo $origin_url | sed "s/https:\/\/[a-z0-9]*@\?github.com/https:\/\/$token@github.com/g")
if [ $? != 0 ]; then echo "replace fail!"; exit 0; fi
echo -e "replace to:\n$replace\n"
git remote set-url origin $replace
echo -e "replace succ!"
git remote -v
