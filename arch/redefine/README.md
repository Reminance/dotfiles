# 使用 inotify 和 arthas 实现自动热更新

## `inotify`

- 参考博客

  > [linux 实时文件事件监听--inotify](https://www.cnblogs.com/sunsky303/p/8117864.html)

- inotify-wiki

  > [github-wiki](https://github.com/inotify-tools/inotify-tools/wiki)

## `arthas`

- arthas/redefine.html 说明
  > [arthas-wiki/redefine.html](https://arthas.gitee.io/redefine.html)

## 实现

### 目录结构

```
/home/xc/tools/
├── arthas
│   ├── arthas-agent.jar
│   ├── arthas-bin.zip
│   ├── arthas-boot.jar
│   ├── arthas-client.jar
│   ├── arthas-core.jar
│   ├── arthas-demo.jar
│   ├── arthas.properties
│   ├── arthas-spy.jar
│   ├── as.bat
│   ├── as-service.bat
│   ├── as.sh
│   ├── async-profiler
│   ├── install-local.sh
│   └── logback.xml
├── redefine.sh
├── source
│   ├── TestController.class
│   ├── TestInnerController$InnerClass.class
│   ├── TestInnerController$InnerStaticClass.class
│   ├── TestInnerController.class
│   └── TestStaticController.class
└── watchdir.sh

3 directories, 20 files
```

### watchdir.sh 监听 `source` 目录变化

```bash
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
```

### redefine.sh 用于单个更新或整个目录更新

```bash
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
```

### 运行

#### 需要预先配置的参数

- redefine.sh

  - REDEFINE_SINGLE_FILE_NAME // 可选参数, 单文件更新的 class 文件名
  - ARTHAS_HOME // arthas 安装目录(绝对路径)， 会使用到$ARTHAS_HOME/as.sh
  - SOURCE_DIR // 此次要更新的 class 文件目录位置(绝对路径)
  - APP_NAME // JAVA 进程名字 用于 arthas 的选取 attach

#### 两种执行方式

##### 方式一 监听`source`目录自动更新

```bash
./watchdir.sh source
```

#### 方式二 复制 class 文件到`source`目录后，手动更新

```bash
./redefine.sh
```
