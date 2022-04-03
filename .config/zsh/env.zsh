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
[[ $UID -ge 1000 && -d $HOME/dotfiles/bin/tmux && -z $(echo $PATH | grep -o $HOME/dotfiles/bin/tmux) ]] && export PATH="${PATH}:$HOME/dotfiles/bin/tmux"
[[ $UID -ge 1000 && -d $HOME/dotfiles/bin/statusbar && -z $(echo $PATH | grep -o $HOME/dotfiles/bin/statusbar) ]] && export PATH="${PATH}:$HOME/dotfiles/bin/statusbar"
[[ $UID -ge 1000 && -d $HOME/workspace/work-tools && -z $(echo $PATH | grep -o $HOME/workspace/work-tools) ]] && export PATH="${PATH}:$HOME/workspace/work-tools"
# export PATH="$PATH:${$(find $HOME/dotfiles/bin -type d -printf %p:)%%:}"

export TERMINAL=alacritty
export BROWSER=google-chrome-stable

# terminal
# export TERM=xterm-256color

# VIM
export MYVIMRC=$HOME/.config/vim/init.vim
# export MYVIMRC=$HOME/.config/nvim/init.lua
# NVIM_NIGHTLY=$HOME/.config/sandbox/nvim-nightly/nvim-linux64/bin/nvim
export EDITOR=nvim

# ranger
export RANGER_LOAD_DEFAULT_RC=FALSE

# GO
export GO111MODULE=on
export GOPROXY=https://goproxy.cn,https://gocenter.io,https://goproxy.io,direct
# export GOPROXY=https://goproxy.cn/
# export GOPROXY=https://mirrors.aliyun.com/goproxy
# [[ -f /usr/lib/go ]] && export GOROOT=/usr/lib/go
# export GOPATH=$HOME/go
# export GOBIN=$HOME/go/bin
# export PATH=$PATH:$GOROOT/bin
# export PATH=$PATH:$GOBIN

# https://stackoverflow.com/questions/48362901/whats-the-mac-os-x-way-to-execute-this-command
# export PATH="$PATH:${$(find $GOROOT/bin -type d -printf %p:)%%:}"
# export PATH="$PATH:${$(find $GOBIN -type d -printf %p:)%%:}"

# rust
# export RUSTUP_DIST_SERVER=https://mirrors.tuna.tsinghua.edu.cn/rustup # use default server instead

# JAVA
# export JDK_HOME=/usr/lib/jvm/java-8-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
# export JAVA_HOME=/usr/lib/jvm/java-11-zulu-fx
# export JAVA_HOME=/usr/lib/jvm/java-16-zulu-fx
# export JAVA_HOME=/usr/lib/jvm/java-8-zulu-fx
# [[ -f /usr/lib/jvm/java-8-zulu-fx ]] && export JAVA_HOME=/usr/lib/jvm/java-8-zulu-fx
# [[ -f /usr/libexec/java_home ]] && export JAVA_HOME=/usr/libexec/java_home
# export JAVA_HOME=$(/usr/libexec/java_home)
# export M2_HOME=/opt/maven
# export GRADLE_HOME=/usr/share/java/gradle
# this is for nvim-lsp env
# export JAR=$HOME/workspace/java/jdtls/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.0.v20200915-1508.jar
# export JDTLS_HOME=$HOME/workspace/java/jdtls/eclipse.jdt.ls
# export JAR=${$(find $JDTLS_HOME/plugins/org.eclipse.equinox.launcher_*.jar -type f -printf %p:)%%:}
# export JDTLS_CONFIG=$JDTLS_HOME/config_linux

# could be in /etc/profile
#export TERMINAL=alacritty
#export BROWSER=google-chrome-stable
#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#export XMODIFIERS="@im=fcitx"
#export _JAVA_AWT_WM_NONREPARENTING=1
