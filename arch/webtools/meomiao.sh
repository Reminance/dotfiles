#!/usr/bin/bash

# 生成随机email
declare -A email_map=(["1"]="@gmail.com" ["2"]="@yahoo.com" ["3"]="@126.com" ["4"]="@163.com")
email_idx=$[RANDOM%4+1]
email_prefix=$(echo $RANDOM |md5sum |cut -c 1-9)
random_email=$(echo $email_prefix${email_map[$email_idx]})
echo random_email:$random_email

# 密码123456789
password='123456789'
# 邀请码空
invite_code=''
# email编码
email_code=''
# meomiao地址
meomiao_url='https://api.meomiao.us/api/v1/'
register_api=$meomiao_url'passport/auth/register'
get_subscribe_api=$meomiao_url'user/getSubscribe'

# 注册
register_result=$(curl -c "cookie" $register_api -d 'email='$random_email'&password='$password'&invite_code='$invite_code'&email_code='$email_code  -X POST)
echo register_result:$register_result
data_register=`echo $register_result | jq -r '.data'`
echo data_register:$data_register
if [ "$data_register" != "true" ]; then echo 注册失败; exit 0; else echo 注册成功; fi

# 获取订阅地址
subscribe_result=$(curl -H "refer:https\://api.meomiao.us/" -b "cookie" $get_subscribe_api -X GET)
# echo subscribe_result:$subscribe_result
subscribe_url=`echo $subscribe_result | jq -r '.data.subscribe_url'`
if [ "$subscribe_url" == "null" ]; then echo 订阅失败; exit 0; else echo 订阅成功; fi
echo subscribe_url:$subscribe_url
echo $subscribe_url > subscribe_`date +'%Y-%m-%d|%H:%M:%S'`.txt

# 发送到邮件地址

