#!/usr/bin/bash

# 单文件更新的class文件名
REDEFINE_SINGLE_FILE_NAME=$1
# arthas安装目录(绝对路径)， 会使用到$ARTHAS_HOME/as.sh
ARTHAS_HOME=/home/xc/tools/arthas
# 此次要更新的class文件目录位置(绝对路径)
SOURCE_DIR=/home/xc/tools/source
# JAVA进程名字 用于arthas的选取attach
APP_NAME=test-app-name

# 一些校验
if [ ! -d $SOURCE_DIR ];then echo "没有$SOURCE_DIR目录"; exit 0; fi
if [ "$(ls $SOURCE_DIR)" == '' ]; then echo "$SOURCE_DIR目录为空"; exit 0; fi

# 这里适配 单文件/整个'$SOURCE_DIR'目录 更新
if [ ! -n "$REDEFINE_SINGLE_FILE_NAME" ] ;then
    REDEFINE_CLASSES=$(cd $SOURCE_DIR && find $PWD/* | xargs ls -d | tr '\n' ' ')
    $ARTHAS_HOME/as.sh --select ${APP_NAME} -c "redefine $REDEFINE_CLASSES"
    # 执行完了之后删除目录
    find $SOURCE_DIR -type f | xargs rm -f
else
    $ARTHAS_HOME/as.sh --select ${APP_NAME} -c "redefine $SOURCE_DIR/$1"
    # 执行完了之后删除指定文件
    rm -rf $SOURCE_DIR/$1
fi

