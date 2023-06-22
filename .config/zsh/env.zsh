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
# [[ $UID -ge 1000 && -d $HOME/workspace/work-tools/shell && -z $(echo $PATH | grep -o $HOME/workspace/work-tools/shell) ]] && export PATH="${PATH}:$HOME/workspace/work-tools/shell"
[[ $UID -ge 1000 && -d $HOME/workspace/work-tools/jira && -z $(echo $PATH | grep -o $HOME/workspace/work-tools/jira) ]] && export PATH="${PATH}:$HOME/workspace/work-tools/jira"
[[ $UID -ge 1000 && -d $HOME/workspace/work-tools/python && -z $(echo $PATH | grep -o $HOME/workspace/work-tools/python) ]] && export PATH="${PATH}:$HOME/workspace/work-tools/python"
# export PATH="$PATH:${$(find $HOME/dotfiles/bin -type d -printf %p:)%%:}"

export TERMINAL=alacritty
export BROWSER=google-chrome-stable

# gpg
export GPG_TTY=$(tty)

# terminal
# export TERM=xterm-256color

# VIM
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
export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk-17.jdk/Contents/Home

# could be in /etc/profile
#export TERMINAL=alacritty
#export BROWSER=google-chrome-stable
#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#export XMODIFIERS="@im=fcitx"
#export _JAVA_AWT_WM_NONREPARENTING=1

# C/C++
# os specific
# for ZSH  `OSTYPE` is set by ZSH the shell itself.
case "$OSTYPE" in
  darwin*)
    # for homebrew
    export CPATH=/opt/homebrew/include
    export LIBRARY_PATH=/opt/homebrew/lib
    export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
    export LDFLAGS="-L/opt/homebrew/opt/llvm/lib -L/opt/homebrew/opt/openssl@3/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/llvm/include -I/opt/homebrew/opt/openssl@3/include"
    export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.aliyun.com/homebrew/homebrew-bottles
    # for openssl
    export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@3/lib/pkgconfig"
    export PATH="/opt/homebrew/opt/openssl@3/bin:$PATH"
    ;;
  linux*)
    # do nth for now
    ;;
  dragonfly*|freebsd*|netbsd*|openbsd*)
    # do nth for now
    ;;
esac
