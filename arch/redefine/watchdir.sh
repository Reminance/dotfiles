#!/usr/bin/bash
#filename watchdir.sh

path=$1

# inotifywait -mr --timefmt '%d/%m/%y %H:%M' --format '%T %w %f' -e close_write,modify,delete,create,attrib $path | while read DATE TIME DIR FILE; do

# 开始监听目录
inotifywait -mr --timefmt '%d/%m/%y %H:%M' --format '%T %w %f' -e modify $path | while read DATE TIME DIR FILE; do

# 例如： FILECHANGE=source/Test.class
FILECHANGE=${DIR}${FILE}

# 如果需要记录日志
# echo "At ${TIME} on ${DATE}, file $FILECHANGE was change" >> /logs/redefine/update.log

echo "At ${TIME} on ${DATE}, file $FILECHANGE was change"

# 执行单个更新文件
/home/xc/tools/redefine.sh $FILE
done
