export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
# export LC_ALL=C
# export LANG=zh_CN.UTF-8
# export LANGUAGE=zh_CN.UTF-8
# export LC_ALL=zh_CN.UTF-8

# Adds `$HOME/dotfiles/bin` to $PATH
# If user ID is greater than or equal to 1000 & if $HOME/bin exists and is a directory & if $HOME/bin is not already in your $PATH
# then export $HOME/bin to your $PATH.
[[ $UID -ge 1000 && -d $HOME/dotfiles/bin && -z $(echo $PATH | grep -o $HOME/dotfiles/bin) ]] && export PATH="${PATH}:$HOME/dotfiles/bin"
[[ $UID -ge 1000 && -d $HOME/dotfiles/bin/i3cmds && -z $(echo $PATH | grep -o $HOME/dotfiles/bin/i3cmds) ]] && export PATH="${PATH}:$HOME/dotfiles/bin/i3cmds"
[[ $UID -ge 1000 && -d $HOME/dotfiles/bin/statusbar && -z $(echo $PATH | grep -o $HOME/dotfiles/bin/statusbar) ]] && export PATH="${PATH}:$HOME/dotfiles/bin/statusbar"
# export PATH="$PATH:${$(find $HOME/dotfiles/bin -type d -printf %p:)%%:}"

export TERMINAL=alacritty
export BROWSER=google-chrome-stable

export WORKSPACE=$HOME/workspace

# terminal
# export TERM=xterm-256color

# VIM
export MYVIMRC=$HOME/.config/nvim/init.vim
NVIM=nvim
NVIM_NIGHTLY=$HOME/.config/sandbox/nvim-nightly/nvim-linux64/bin/nvim
# export VISUAL="$NVIM_NIGHTLY"
# export EDITOR="$VISUAL"
export VISUAL="$NVIM"
export EDITOR="$VISUAL"

# ranger
export RANGER_LOAD_DEFAULT_RC=FALSE

# GO
export GO111MODULE=on
export GOPROXY=https://goproxy.cn/
# export GOPROXY=https://mirrors.aliyun.com/goproxy
export GOROOT=/usr/lib/go
export GOPATH=$HOME/go
export GOBIN=$HOME/go/bin
# export PATH=$PATH:$GOROOT/bin
# export PATH=$PATH:$GOBIN
export PATH="$PATH:${$(find $GOROOT/bin -type d -printf %p:)%%:}"
export PATH="$PATH:${$(find $GOBIN -type d -printf %p:)%%:}"

# rust
export RUSTUP_DIST_SERVER=https://mirrors.tuna.tsinghua.edu.cn/rustup

# JAVA
# export JDK_HOME=/usr/lib/jvm/java-8-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-11-zulu-fx
# export JAVA_HOME=/usr/lib/jvm/java-16-zulu-fx
export JAVA_HOME=/usr/lib/jvm/java-8-zulu-fx
export M2_HOME=/opt/maven
export GRADLE_HOME=/usr/share/java/gradle
# this is for nvim-lsp env
# export JAR=$HOME/workspace/java/jdtls/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.0.v20200915-1508.jar
export JDTLS_HOME=$HOME/workspace/java/jdtls/eclipse.jdt.ls
export JAR=${$(find $JDTLS_HOME/plugins/org.eclipse.equinox.launcher_*.jar -type f -printf %p:)%%:}
export JDTLS_CONFIG=$JDTLS_HOME/config_linux
export WORKSPACE=$HOME/workspace

# could be in /etc/profile
#export TERMINAL=alacritty
#export BROWSER=google-chrome-stable
#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#export XMODIFIERS="@im=fcitx"
#export _JAVA_AWT_WM_NONREPARENTING=1
