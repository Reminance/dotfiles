#!/usr/bin/bash

############ Command Arguments ############

# arthas安装目录(绝对路径)， 会使用到$ARTHAS_HOME/as.sh
ARTHAS_HOME=/home/xc/tools/arthas

# Verbose, print debug info.
VERBOSE=false

# select target process by classname or JARfilename
# JAVA进程名字 用于arthas的选取attach
SELECT=TestApp

# 此次要更新的class文件目录位置(绝对路径)
SOURCE_DIR=/home/xc/.config/arch/redefine/src

# 文件更新的class文件名, 多个文件以空格分隔
REDEFINE_CLASSES=

# 可定位的可选参数
# 文件更新的class文件名, 多个文件以空格分隔
POSITIONAL=()

# 是否inotify监听方式redefine
NOTIFY=false

doRedefine() {
    # 一些校验
    if [ ! -d $SOURCE_DIR ];then echo "没有$SOURCE_DIR目录"; exit 0; fi
    if [ "$(ls $SOURCE_DIR)" == '' ]; then echo "$SOURCE_DIR目录为空"; exit 0; fi

    # 这里适配 单文件/整个'$SOURCE_DIR'目录 更新
    if [ ! -n "$REDEFINE_CLASSES" ] ;then
        REDEFINE_CLASSES=$(cd $SOURCE_DIR && find $PWD/*.class | xargs ls -d | tr '\n' ' ')
        # echo redefine all file in src dir: $REDEFINE_CLASSES
        $ARTHAS_HOME/as.sh --select ${SELECT} -c "redefine $REDEFINE_CLASSES"
        # 执行完了之后删除更新的文件
        find $REDEFINE_CLASSES -type f | xargs rm -f
    else
        echo redefine specific classes: $REDEFINE_CLASSES
        $ARTHAS_HOME/as.sh --select ${SELECT} -c "redefine $REDEFINE_CLASSES"
        # 执行完了之后删除指定文件
        rm -f $REDEFINE_CLASSES
    fi
}

call_jps() {
    if [ "${VERBOSE}" = true ] ; then
        # "${JAVA_HOME}"/bin/jps -l -v
        jps -l -v
    else
        # "${JAVA_HOME}"/bin/jps -l
        jps -l
    fi
}

doNotify() {
    # inotifywait -mr --timefmt '%d/%m/%y %H:%M' --format '%T %w %f' \
    #     -e close_write,modify,delete,create,attrib $SOURCE_DIR | while read DATE TIME DIR FILE; do

    # 开始监听目录
    inotifywait -mr --timefmt '%d/%m/%y %H:%M' --format '%T %w %f' \
        -e modify $SOURCE_DIR | while read DATE TIME DIR FILE; do

        # 例如： REDEFINE_CLASSES=source/Test.class
        REDEFINE_CLASSES=${DIR}${FILE}

        # 如果需要记录日志
        # echo "At ${TIME} on ${DATE}, file $REDEFINE_CLASSES was change" >> /logs/redefine/update.log

        echo "At ${TIME} on ${DATE}, file $REDEFINE_CLASSES was change"

        # 执行更新文件
        doRedefine

    done
}

usage() {
    echo "
Usage:
    $0 [-h] [-v] [-n] [-d <value>] [--arthas-home <value>] [--select <value>]

Options and Arguments:
 -h,--help                      Print usage
 -d,--source-dir <value>        specific directory to load classes
 -v,--verbose                   Verbose, print debug info.
 -n,--notify                    start monitoring the source directory
    --arthas-home <value>       The arthas home
    --select <value>            select target process by classname or JARfilename

EXAMPLES:
  ./redefine.sh --select TestApp --arthas-home /home/xc/tools/arthas -d /home/xc/.config/arch/redefine/src -n
  ./redefine.sh --select TestApp --arthas-home /home/xc/tools/arthas -d /home/xc/.config/arch/redefine/src
  ./redefine.sh --select TestApp --arthas-home /home/xc/tools/arthas Test1.class Test2.class Test3.class

Here is the list of possible java process(es) to attatch:
"
call_jps | grep -v sun.tools.jps.Jps
}

parse_arguments() {
    echo 参数个数: $#, 参数: $@
    while [[ $# -gt 0 ]]
    do
    key="$1"

    case $key in
        -h|--help)
        usage
        exit 0
        ;;
        -v|--verbose)
        VERBOSE=true
        shift # past argument
        ;;
        -n|--notify)
        NOTIFY=true
        shift # past argument
        ;;
        --arthas-home)
        ARTHAS_HOME="$2"
        shift # past argument
        shift # past value
        ;;
        --select)
        SELECT="$2"
        shift # past argument
        shift # past value
        ;;
        -d|--source-dir)
        SOURCE_DIR="$2"
        shift # past argument
        shift # past argument
        ;;
        *)    # unknown option
        POSITIONAL+=("$SOURCE_DIR/$1") # save it in an array for later
        shift # past argument
        ;;
    esac
    done

    echo '$ARTHAS_HOME' $ARTHAS_HOME

    # Verbose, print debug info.
    echo '$VERBOSE' $VERBOSE

    # select target process by classname or JARfilename
    # JAVA进程名字 用于arthas的选取attach
    echo '$SELECT' $SELECT

    # 此次要更新的class文件目录位置(绝对路径)
    echo '$SOURCE_DIR' $SOURCE_DIR

    # Positional parameters
    echo 'Positional Params:' "${POSITIONAL[@]}"

    if [[ -n $POSITIONAL ]]; then
        REDEFINE_CLASSES=${POSITIONAL[@]}
    fi

    # # set parameters and list by position
    # set -- "${POSITIONAL[@]}" # restore positional parameters
    # echo 'Positional Params:' $1 $2 $3
}

main(){
    parse_arguments "${@}" ||  echo "$(usage)"

    if [ "${NOTIFY}" = true ] ; then
        doNotify
    else
        doRedefine
    fi
}

main "${@}"
