#export LANG=en_US.UTF-8
#export LANGUAGE=en_US.UTF-8
#export LC_ALL=C
export LANG=zh_CN.UTF-8
export LANGUAGE=zh_CN.UTF-8
export LC_ALL=zh_CN.UTF-8
export MYVIMRC=~/.config/nvim/init.vim
export GOROOT=/usr/lib/go
export GOPATH=~/go
export GOBIN=~/go/bin
export PATH=$PATH:$GOROOT/bin:$GOBIN
export PATH=$PATH:~/.config/bin
export GOPROXY=https://goproxy.cn/
export RUSTUP_DIST_SERVER=https://mirrors.tuna.tsinghua.edu.cn/rustup
export RANGER_LOAD_DEFAULT_RC=FALSE
# Make VIM the global editor
NVIM=nvim
NVIM_NIGHTLY=~/.config/sandbox/nvim-nightly/nvim-linux64/bin/nvim
export VISUAL="$NVIM_NIGHTLY"
export EDITOR="$VISUAL"
#export TERM=alacritty
export TERM=xterm-256color
# export JDK_HOME=/usr/lib/jvm/java-8-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
# export http_proxy=127.0.0.1:7890
# export https_proxy=127.0.0.1:7890

# this is for nvim-lsp env
export JAR=~/.config/coc/extensions/coc-java-data/server/plugins/org.eclipse.equinox.launcher_-2.5.700.v20200207-2156.jar
export GRADLE_HOME=$HOME/gradle
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
export JDTLS_CONFIG=~/.config/coc/extensions/coc-java-data/server/config_linux
export WORKSPACE=$HOME/workspace
export M2_HOME=/opt/maven
