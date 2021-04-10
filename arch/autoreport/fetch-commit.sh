#!/usr/bin/bash
echo 正在缓存项目提交信息
echo
today=`date +'%Y-%m-%d'`
since_date=`date -u +"%Y-%m-%d"`T00:00:00.000Z
if [ ! -s "$1/project-common.conf" ];then echo project-common.conf; exit 0; fi
for((i=0;i<`cat $1/project-common.conf | /home/xc/dev-tools/jq 'length'`;i++));
do
    project=$(cat $1/project-common.conf | /home/xc/dev-tools/jq ".[$i]");
    project_name=$(echo $project | /home/xc/dev-tools/jq ".project");
    echo 当前缓存项目 $project_name
    project_id=$(cat $1/data/base/gitlab/project.json|/home/xc/dev-tools/jq ".[]|select(.name == "$project_name")|.id")
    branchs=$(echo $project | /home/xc/dev-tools/jq ".branchs");
    branchs_length=`echo $branchs | /home/xc/dev-tools/jq "length"`
    for((j=0;j<$branchs_length;j++));
    do
        branch=$(echo $branchs | /home/xc/dev-tools/jq ".[$j]"|sed 's/"//g');
        # echo 当前缓存分支 $branch
        project_path=$(echo $1/data/project/$today/`echo $project_name|sed 's/"//g'`/$branch)
        if [ ! -d "$project_path" ];then
          mkdir -p $project_path
        fi
        # 拉取project_id branch下当天的提交
        curl -s --header "PRIVATE-TOKEN: C2-EoQ_iCmyPXXsyAREK" http://git.quanhoo.com/api/v4/projects/$project_id/repository/commits?ref_name=$branch\&since=$since_date\&per_page=200 | /home/xc/dev-tools/jq -r . > $1/data/project/$today/`echo $project_name|sed 's/"//g'`/$branch/commit.log
        echo 缓存分支完成 $branch
    done
    echo 缓存项目完成 $project_name
    echo
done
echo 缓存项目提交信息完成
echo
echo
