#!/usr/bin/bash
# 脚本入参信息
# userID projectId createBy  action status estimatedTime content updateBy
url=$1
userId=$2
projectId=$3
createBy=$4
action=$5
ac_status=$6
estimatedTime=$7
content=$8
updateBy=$9

save() {
    result=$(curl -s $url/daily/save -d 'createBy='$createBy'&projectId='$projectId'&action='$action'&status='$ac_status'&estimatedTime='$estimatedTime'&content='$content'&userId='$userId'&updateBy='$updateBy  -X POST)
    error_code_save=`echo $result | /home/xc/dev-tools/jq -r '.errorCode'`
    if [ $error_code_save -ne 0 ]; then echo 提交失败; exit 0; else echo 提交成功; fi
}


modify() {
    result=$(curl -s $url/daily/modify -d 'id='$1'&content='$content  -X POST)
    error_code_modify=`echo $result | /home/xc/dev-tools/jq -r '.errorCode'`
    if [ $error_code_modify -ne 0 ]; then echo 修改失败; exit 0; else echo 修改成功; fi
}

# 查询提交历史，判断是否存在
today_commit="$(curl -s $url/daily/getMyDaily?current=1\&size=100\&userId=$userId)"
# 接口error_code不为0直接返回
error_code=`echo $today_commit | /home/xc/dev-tools/jq -r '.errorCode'`
if [ $error_code -ne 0 ]; then echo 获取当天提交失败; exit 0; fi
latest_createTime=`echo $today_commit | /home/xc/dev-tools/jq -r '.data.records[0].createTime'`
# 获取当前日期，判断提交内容中是否有当天数据
if
  echo $latest_createTime | grep $(date "+%Y-%m-%d")
then
    echo "开始修改"
    modify `echo $today_commit | /home/xc/dev-tools/jq -r '.data.records[0].id'`
else
    echo "开始提交"
    save
fi
