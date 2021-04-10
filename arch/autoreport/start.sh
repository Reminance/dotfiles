#!/usr/bin/bash
# today=`date +'%Y-%m-%d'`
basepath=$(cd `dirname $0`; pwd)
# basepath=/home/xc/autoreport
url="http://172.16.1.27:8080/daily/api/v1"
echo -e "提交开始: `date "+%Y-%m-%d %H:%M:%S"`"
# 初始化工程与字典
source $basepath/init-daily.sh $basepath $url
# 初始化所有gitlab工程
source $basepath/init-gitlab.sh $basepath
# 初始化所有项目当天的提交
source $basepath/fetch-commit.sh $basepath
# 遍历user提交
source $basepath/each-user.sh $basepath $url
echo -e "提交完毕: `date "+%Y-%m-%d %H:%M:%S"`"
echo
echo
