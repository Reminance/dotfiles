Jar=$1
Jar_file=${Jar}'-server-0.0.1-SNAPSHOT.jar'
DebugPort=$2
#Jar=$(echo ${Jar_file}|awk -F. '{print $1}')
[ ! "$Jar" ] &&  echo -e "\033[41m====== 请输入：项目名称 如start.sh app.jar ======\033[0m" && exit 1
cd /root/workspace/${Jar}/${Jar}-api
[ $? -ne 0 ] && echo "没有这个${Jar}目录" && exit 1
git pull
[ $? -ne 0 ] && echo "git pull 无法更新" && exit 1
mvn clean install -U -pl ${Jar}-server/ -am -amd -Dmaven.test.skip=true
[ $? -ne 0 ] && echo "打包失败" && exit 1

#cd $Jar && [ -f "${Jar_file}" ] || { echo -e "\033[41m====== 不存在jar/war包：${Jar_file} =====\033[0m" ; exit 1 ;  }

#kill掉当前服务
Pid=`ps -ef | grep -v grep | grep "${Jar_file}" | grep java | awk '{print $2}'| head -1`
[ -n "$Pid" ] && [ ! -z $Pid ] && /usr/bin/kill $Pid && echo -e "\033[41m------ 停止${Jar_file}，kill Pid: $Pid ------\033[0m"
sleep 1

#[[ $Jar =~ miniapp|miniapp1 ]] && sleep 20

Pid=`ps -ef | grep -v grep | grep "${Jar_file}" | grep java | awk '{print $2}'| head -1`
[ -n "$Pid" ] && [ ! -z $Pid ] && /usr/bin/kill -9 $Pid && echo -e "\033[41m------ 停止${Jar_file}，kill -9 Pid: $Pid ------\033[0m"
sleep 1

#启动服务
cd ${Jar}-server/target
[ ! -f "$Jar_file" ] && echo -e "\033[41m++++++ ${Jar_file}不存在 ++++++\033[0m"&& exit 2
echo -e "\033[42m++++++ 启动： ${Jar_file} ++++++\033[0m"

if [ ! -z ${DebugPort} ];then
    echo -e "\033[42m++++++ debug端口： ${DebugPort} ++++++\033[0m"
    nohup java -Xms128m -Xmx500m -Xdebug -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=${DebugPort} -jar ${Jar_file} --spring.profiles.active=dev  &> nohup.out &
else
    nohup java -Xms128m -Xmx500m -jar ${Jar_file} --spring.profiles.active=dev  &> nohup.out &
fi

sleep 1 && echo -e "\033[42m++++++ 启动完成 ++++++\033[0m"

tail -f 50 nohup.out
#exit 0
