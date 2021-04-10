#!/bin/bash

Jar_file=$1
#Jar=$(echo ${Jar_file}|awk -F. '{print $1}')
[ ! "$1" ] &&  echo -e "\033[41m====== 请输入：项目名称 如start.sh eureka.jar ======\033[0m" && exit 1

if [ -n "$2" ]
then
    echo 212 $2
    DebugPort=$2
fi

pushd /opt/apps/jar/ #&> /dev/null

#cd $Jar && [ -f "${Jar_file}" ] || { echo -e "\033[41m====== 不存在jar/war包：${Jar_file} =====\033[0m" ; exit 1 ;  }

#kill掉当前服务
Pid=`ps -ef | grep -v grep | grep "${Jar_file}" | grep java | awk '{print $2}'| head -1`
[ -n "$Pid" ] &&
[ ! -z $Pid ] && kill $Pid && echo -e "\033[41m------ 停止${Jar_file}，kill Pid: $Pid ------\033[0m"
sleep 2

Pid=`ps -ef | grep -v grep | grep "${Jar_file}" | grep java | awk '{print $2}'| head -1`
[ -n "$Pid" ] &&
[ ! -z $Pid ] && kill -9 $Pid && echo -e "\033[41m------ 停止${Jar_file}，kill -9 Pid: $Pid ------\033[0m"
sleep 1

#启动服务
[ ! -f "$Jar_file" ] && echo -e "\033[41m++++++ ${Jar_file}不存在 ++++++\033[0m"&& exit 2
echo -e "\033[42m++++++ 启动： ${Jar_file} ++++++\033[0m"
#/usr/bin/nohup
#/usr/bin/setsid
cat /dev/null > /logs/jenkins/${Jar}.log
if [ ! -z "${DebugPort}" ];then
    if [[ "${Jar}" =~ oa|user|trading|auth|offline ]];then
        echo 1h
        nohup java -Xms128m -Xmx500m -Dloader.path=/opt/apps/jar/lib/ -Xdebug -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=${DebugPort} -jar ${Jar_file} --spring.profiles.active=test &> nohup.out &
        sleep 1 && echo -e "\033[42m++++++ 启动完成 ++++++\033[0m"
    elif [[ "${Jar}" =~ "activity-api" ]];then
        echo 2h
        nohup java -Xms128m -Xmx500m -Xdebug -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=${DebugPort} -Dactive.schedule=true -jar ${Jar_file} --spring.profiles.active=test &> nohup.out  &
    else
        echo 3h
        nohup java -Xms128m -Xmx500m -Xdebug -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=${DebugPort} -jar ${Jar_file} --spring.profiles.active=test &> nohup.out  &
        sleep 1 && echo -e "\033[42m++++++ 启动完成 ++++++\033[0m"
    fi
else
    if [[ "${Jar}" =~ oa|user|trading|auth|offline ]];then
        echo 1
        nohup java -Xms128m -Xmx500m -Dloader.path=/opt/apps/jar/lib/ -jar ${Jar_file} --spring.profiles.active=test &> nohup.out &
        sleep 1 && echo -e "\033[42m++++++ 启动完成 ++++++\033[0m"
    elif [[ "${Jar}" =~ config ]];then
        echo 2
        nohup java -Xms128m -Xmx500m -jar ${Jar_file} --spring.profiles.active=native,test &> nohup.out  &
        sleep 1 && echo -e "\033[42m++++++ 启动完成 ++++++\033[0m"
    elif [[ "${Jar}" =~ "activity-api" ]];then
        echo 3
        nohup java -Xms128m -Xmx500m -jar ${Jar_file} --spring.profiles.active=test &> nohup.out  &
    else
        echo 4
        nohup java -Xms128m -Xmx500m -jar ${Jar_file} --spring.profiles.active=test &> nohup.out  &
        sleep 1 && echo -e "\033[42m++++++ 启动完成 ++++++\033[0m"
    fi
fi
#exit 0
#exit
