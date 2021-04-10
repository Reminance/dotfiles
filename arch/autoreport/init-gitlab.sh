#!/usr/bin/bash

if [ ! -d "$1/data/base/gitlab" ];then
  echo -e "\033[36m==== 初始化git项目信息 ====\033[0m"
  mkdir -p $1/data/base/gitlab
fi
curl -s --header "PRIVATE-TOKEN: C2-EoQ_iCmyPXXsyAREK" http://git.quanhoo.com/api/v4/projects?simple=true | /home/xc/dev-tools/jq -r . > $1/data/base/gitlab/project.json
if [ $? -ne 0 ]; then echo init gitlab error; exit 0; fi

