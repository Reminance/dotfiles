# mirror-proxy for github

## 加速

```
Github国内加速克隆及下载
fastgit.org
https://doc.fastgit.org/

gitclone.com
https://gitclone.com/

gitee
https://gitee.com/mirrors

cnpmjs.org
https://github.com.cnpmjs.org/

克隆加速

#原地址
git clone https://github.com/kubernetes/kubernetes.git

#改为
git clone https://github.com.cnpmjs.org/kubernetes/kubernetes.git

#或者
git clone https://hub.fastgit.org/kubernetes/kubernetes.git

#或者
git clone https://gitclone.com/github.com/kubernetes/kubernetes.git

release下载加速

#原地址
wget https://github.com/goharbor/harbor/releases/download/v2.0.2/harbor-offline-installer-v2.0.2.tgz

#改为
wget https://hub.fastgit.org/goharbor/harbor/releases/download/v2.0.2/harbor-offline-installer-v2.0.2.tgz

免替换方法

git config --global url."https://hub.fastgit.org".insteadOf https://github.com

# ghproxy
git clone https://ghproxy.com/https://github.com/redis/redis.git

#测试
git clone https://github.com/kubernetes/kubernetes.git

查看git配置信息

git config --global --list

取消设置

git config --global --unset url.https://github.com/.insteadof

raw文件下载加速

#原地址：
wget https://raw.githubusercontent.com/kubernetes/kubernetes/master/README.md

#替换为
wget https://raw.staticdn.net/kubernetes/kubernetes/master/README.md

提供web界面的github资源加速网站：

GitHub 文件加速：https://gh.api.99988866.xyz/
Github仓库加速：https://github.zhlh6.cn/
Github仓库加速：http://toolwa.com/github/
```

# forked repo(github.com forked)

/home/xc/workspace/java
├── mybatis
├── netty
├── spring-framework
└── yudao-spring-framework

```
origin https://github.com/Reminance/mybatis.git (fetch)
origin https://github.com/Reminance/netty.git (fetch)
origin https://github.com/Reminance/spring-framework.git (fetch)
origin https://github.com/YunaiV/spring-framework.git (fetch)
```

/home/xc/workspace/clang/
└── nginx

```
origin	https://github.com/Reminance/nginx.git (fetch)
```

# update code from remote origin repo using upstream

```
git remote add upstream https://github.com/leisurelicht/wtfpython-cn
git fetch upstream
git merge upstream/master
git push origin master
```

# archfi

```
curl -L archfi.sf.net/archfi > archfi
or
curl -L matmoul.github.io/archfi > archfi
```

# archdi

```
curl -L archdi.sf.net/archdi > archdi
or if sourceforge is down :
curl -L matmoul.github.io/archdi > archdi
```

# fuzzy find

```
sudo pacman -S fzf

sudo pacman -S ripgrep

yay -S universal-ctags-git

sudo pacman -S the_silver_searcher

pacman -S fd
```

# competitive programming

```
HackerRank: https://www.hackerrank.com/
Learn typing: https://www.keybr.com/
Scratch: https://scratch.mit.edu/
Learn C++: https://www.youtube.com/watch?v=mUQZ1...
Competitive Programmer's Handbook: https://cses.fi/book/book.pdf
GeeksForGeeks: https://www.geeksforgeeks.org/fundame...
A2OJ Ladders: https://www.a2oj.com/Ladders.html
Mostafa Saad's Junior Training Sheet: https://goo.gl/unDETI
```

# delta: A viewer for git and diff output
```
# https://github.com/dandavison/delta#installation
sps git-delta-git
```

# building spring ref-docs
```sh
cd spring-framework && ./gradlew asciidoctor
```
> 访问 build\docs\asciidoc 目录中的内容。
> 然后双击 index.html 文件，就可以看到最新的编译内容和结果。

# 使用zulu-jdk8构建包含javafx依赖的项目
```
下载页面：
https://www.azul.com/downloads/zulu-community/?version=java-8-lts&package=jdk-fx

下载链接：
https://cdn.azul.com/zulu/bin/zulu8.54.0.21-ca-fx-jdk8.0.292-linux_x64.tar.gz
```

# windows 端口代理
```
netsh interface portproxy add v4tov4 listenaddress=0.0.0.0 listenport=22 connectaddress=172.18.240.75 connectport=22
```

# note 20210706
```
https://github.com/tallguyjenks/.dotfiles
https://github.com/LukeSmithxyz/voidrice
https://github.com/vivien/i3blocks-contrib
https://github.com/vegarab/varbs
https://github.com/vegarab/dotfiles
```

# jetbrains
This project provides a series of easy-to-use Linux configurations, including some interesting and useful programming-related codes and scripts.
The project also demonstrates how to manage and install these configurations through makefile or symblolic link and shell scripts.

# 有趣的项目
https://github.com/cstack/db_tutorial
https://github.com/visit1985/mdp
https://github.com/hairrrrr/C-CrashCourse
https://github.com/jesrui/althttpd-mirror.git
https://www.sqlite.org/althttpd/doc/trunk/althttpd.md
https://github.com/DoctorWkt/acwj
https://github.com/EZLippi/Tinyhttpd
https://github.com/qinguoyi/TinyWebServer
https://github.com/Light-City/CPlusPlusThings
https://github.com/TheAlgorithms/C-Plus-Plus
https://github.com/nothings/stb
https://github.com/oz123/awesome-c
https://github.com/fffaraz/awesome-cpp

# struct的sizeof计算, 字节对齐原则:
原则1、数据成员对齐规则：结构（struct或联合union）的数据成员，第一个数据成员放在offset为0的地方，以后每个数据成员存储的起始位置要从该成员大小的整数倍开始（比如int在32位机为4字节，则要从4的整数倍地址开始存储）。
原则2、结构体作为成员：如果一个结构里有某些结构体成员，则结构体成员要从其内部最大元素大小的整数倍地址开始存储。（struct a里存有struct b，b里有char，int，double等元素，那b应该从8的整数倍开始存储。）
原则3、收尾工作：结构体的总大小，也就是sizeof的结果，必须是其内部最大成员的整数倍，不足的要补齐。

在实际中，存储变量时地址要求对齐，编译器在编译程序时会遵循两条原则：
（1）结构体变量中成员的偏移量必须是成员数据类型大小的整数倍（0被认为是任何数的整数倍）
（2）结构体大小必须是所有成员大小的整数倍，也即所有成员大小的公倍数。

# Axure RP Mac
https://www.pc6.com/mac/662119.html
