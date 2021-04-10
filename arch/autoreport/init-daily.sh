#!/usr/bin/bash

if [ ! -d "$1/data/base/daily" ];then
  echo -e "\033[36m==== 初始化日报项目与字典 ====\033[0m"
  mkdir -p $1/data/base/daily
fi
curl -s "$2/projects/list" | /home/xc/dev-tools/jq -r . > $1/data/base/daily/project.json
if [ $? -ne 0 ]; then exit 0; fi
cat $1/data/base/daily/project.json | /home/xc/dev-tools/jq -r '.data[]|[.id,.name]|join(",")' > $1/data/base/daily/project.list
curl -s "$2/dict/all" | /home/xc/dev-tools/jq -r . > $1/data/base/daily/dict.json
if [ $? -ne 0 ]; then exit 0; fi
cat $1/data/base/daily/dict.json | /home/xc/dev-tools/jq -r '.data[]|[.value,.label,.description]|join(",")' > $1/data/base/daily/dict.list

