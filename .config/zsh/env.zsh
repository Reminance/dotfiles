#export LANG=en_US.UTF-8
#export LANGUAGE=en_US.UTF-8
#export LC_ALL=C
export LANG=zh_CN.UTF-8
export LANGUAGE=zh_CN.UTF-8
#export LC_ALL=zh_CN.UTF-8

# Adds `~/.local/bin` to $PATH
export PATH="$PATH:${$(find ~/dotfiles/bin -type d -printf %p:)%%:}"

export WORKSPACE=$HOME/workspace

# terminal
#export TERM=alacritty
export TERM=xterm-256color
# export http_proxy=127.0.0.1:7890
# export https_proxy=127.0.0.1:7890

# VIM
export MYVIMRC=~/.config/nvim/init.vim
NVIM=nvim
NVIM_NIGHTLY=~/.config/sandbox/nvim-nightly/nvim-linux64/bin/nvim
# export VISUAL="$NVIM_NIGHTLY"
# export EDITOR="$VISUAL"
export VISUAL="$NVIM"
export EDITOR="$VISUAL"

# ranger
export RANGER_LOAD_DEFAULT_RC=FALSE

# GO
export GOPROXY=https://goproxy.cn/
export GOROOT=/usr/lib/go
# export GOPATH=~/go
# export GOBIN=~/go/bin
# export PATH=$PATH:$GOROOT/bin
# export PATH=$PATH:$GOBIN
# export PATH="$PATH:${$(find $GOROOT/bin -type d -printf %p:)%%:}"
# export PATH="$PATH:${$(find $GOBIN -type d -printf %p:)%%:}"

# rust
export RUSTUP_DIST_SERVER=https://mirrors.tuna.tsinghua.edu.cn/rustup

# JAVA
# export JDK_HOME=/usr/lib/jvm/java-8-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
export JAVA_HOME=/usr/lib/jvm/java-8-zulu-fx
export M2_HOME=/opt/maven
export GRADLE_HOME=$HOME/gradle
# this is for nvim-lsp env
# export JAR=~/.config/coc/extensions/coc-java-data/server/plugins/org.eclipse.equinox.launcher_-2.5.700.v20200207-2156.jar
# export JDTLS_CONFIG=~/.config/coc/extensions/coc-java-data/server/config_linux

# could be in /etc/profile
#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#export XMODIFIERS="@im=fcitx"
#export _JAVA_AWT_WM_NONREPARENTING=1
