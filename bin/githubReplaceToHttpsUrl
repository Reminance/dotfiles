#!/usr/bin/bash
git remote -v || exit 1
echo
origin_url=$(git remote -v | awk 'NR == 1'  | awk {'print $2'})
echo -e "origin_url:$origin_url\n"
replace=$(echo $origin_url | sed "s/https:\/\/[a-z0-9]\+@github.com/https:\/\/github.com/g")
echo -e "replace to:$replace\n"
git remote set-url origin $replace
echo -e "replace succ!"
git remote -v
