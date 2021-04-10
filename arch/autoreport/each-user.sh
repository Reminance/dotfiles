#!/usr/bin/bash
today=`date +'%Y-%m-%d'`
if [ ! -s "$1/user.conf" ];then echo lack of user.conf; exit 0; fi

dealCommitData() {
    # 查询当前用户 当天 当前项目 当前分支 追加到 用户的当日提交(过滤merge)
    cat $1/data/project/$today/`echo $2|sed 's/"//g'`/`echo $3|sed 's/"//g'`/commit.log|/home/xc/dev-tools/jq "map(select(.author_name == "$4"))|.[].title"|grep -v "Merge "|while read -r line
    do
        echo -e "正在生成`echo $2|sed 's/"//g'`/`echo $3|sed 's/"//g'`提交信息"
        if [ ! -s "$5/commit" ];then
            mkdir -p $5
            echo $line|sed 's/"//g'|sed 's/ //g' > $5/commit
        else
            sed -i "1i `echo $line|sed 's/"//g'|sed 's/ //g'`" $5/commit
        fi
        echo -e "`echo $2|sed 's/"//g'`/`echo $3|sed 's/"//g'`提交信息完毕"
    done
}

for((i=0;i<`cat $1/user.conf | /home/xc/dev-tools/jq 'length'`;i++))
do
    user=$(cat $1/user.conf | /home/xc/dev-tools/jq ".[$i]")
    user_name=$(echo $user | /home/xc/dev-tools/jq ".user")
    echo $user_name 提交开始
    # echo git用户 $user_name
    daily_user_id=$(echo $user | /home/xc/dev-tools/jq ".daily_user_id")
    # echo 日报用户id $daily_user_id
    daily_user_name=$(echo $user | /home/xc/dev-tools/jq ".daily_user_name"|sed 's/"//g')
    # echo 日报用户名称 $daily_user_name
    daily_project_id=`cat $1/data/base/daily/project.json|/home/xc/dev-tools/jq ".data[]|select(.name == "$(echo $user | /home/xc/dev-tools/jq ".daily_project")")|.id"`
    # echo 日报项目id $daily_project_id
    daily_action_id=`cat $1/data/base/daily/dict.json|/home/xc/dev-tools/jq ".data[]|select(.label == "$(echo $user | /home/xc/dev-tools/jq ".daily_action")")|.value"`
    # echo 日报行为id $daily_action_id
    daily_status=`cat $1/data/base/daily/dict.json|/home/xc/dev-tools/jq ".data[]|select(.label == "$(echo $user | /home/xc/dev-tools/jq ".daily_status")")|.value"`
    # echo 日报状态 $daily_status
    projects=$(echo $user | /home/xc/dev-tools/jq ".projects")
    projects_length=`echo $projects | /home/xc/dev-tools/jq "length"`
    commit_path=$(echo $1/data/user/`echo $user_name|sed 's/"//g'`/$today)
    # 复位这次的提交flag
    if [ ! -s "$commit_path/needpost" ];then
        mkdir -p $commit_path
    fi
    echo 0 > $commit_path/needpost
    if [ -s "$commit_path/commit" ];then
        cat /dev/null > $commit_path/commit
    fi
    for((j=0;j<$projects_length;j++))
    do
        project=$(echo $projects | /home/xc/dev-tools/jq ".[$j].project")
        echo 正在采集项目 $project
        project_id=$(cat $1/data/base/gitlab/project.json|/home/xc/dev-tools/jq ".[]|select(.name == "$project")|.id")
        branchs=$(echo $projects | /home/xc/dev-tools/jq ".[$j].branchs")
        branchs_length=`echo $branchs | /home/xc/dev-tools/jq "length"`
        for((k=0;k<$branchs_length;k++))
        do
            branch=$(echo $branchs | /home/xc/dev-tools/jq ".[$k]")
            echo 正在采集分支 $branch
            current_branch_path=$(echo $1/data/project/$today/`echo $project|sed 's/"//g'`/`echo $branch|sed 's/"//g'`)
            latest_revision=$(cat $current_branch_path/commit.log|/home/xc/dev-tools/jq "map(select(.author_name == "$user_name"))|.[0].id")
            revision_path=$(echo -e "$commit_path/`echo $project|sed 's/"//g'`/`echo $branch|sed 's/"//g'`")
            if [ ! -s "$revision_path/revision" ];then
                mkdir -p $revision_path
                echo $latest_revision > $revision_path/revision
                echo -e "初始化当前`echo $project|sed 's/"//g'`/`echo $branch|sed 's/"//g'`分支的revision"
                echo 1 > $commit_path/needpost
                dealCommitData $1 $project $branch $user_name $commit_path
            else
                # 查询当前分支最新一条revision和远程是否一致  不一致则需要重新提交修改(将标志位echo到用户目录下)
                local_revision=$(cat $revision_path/revision)
                # echo latest_revision 是 $latest_revision
                # echo local_revision 是 $local_revision
                if [ "$latest_revision" == "$local_revision" ] ;then
                    echo -e "当前`echo $project|sed 's/"//g'`/`echo $branch|sed 's/"//g'`分支的提交已是最新"
                    continue
                else
                    echo 1 > $commit_path/needpost
                    dealCommitData $1 $project $branch $user_name $commit_path
                fi
            fi
            echo 采集分支完成 $branch
        done
        echo 采集项目完成 $project
    done

    if [ $(cat $commit_path/needpost) -eq 1 ] ;then
        if [ ! -s "$commit_path/commit" ];then
            echo $user_name提交内容为空
        else
            total_commit=$(cat $commit_path/commit)
            total_content=$(cat $commit_path/commit|awk 'BEGIN{RS="\n";ORS="%3Cbr%2F%3E"}{print $0}')
            # echo total_content--------------: $total_content
            echo $user_name 当天提交为: $total_commit
            source $1/post.sh $2 $daily_user_id $daily_project_id $daily_user_name $daily_action_id $daily_status 8 $total_content $daily_user_name
            echo $user_name 提交完成
        fi
    else
        echo $user_name已经是最新提交
    fi
    echo
done

