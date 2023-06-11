alias a='alias'

# alias ls='logo-ls'

# alias ls='exa --icons'
# alias la='ls -lah'
# alias lls='ls -lahS'

#file access
alias cp="cp -riv"
alias mv="mv -iv"
alias mkdir="mkdir -vp"

alias llt='ls -lht'
alias lls='ls -lAhS'

# EDITOR from ~/.config/zsh/env.zsh
alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias e='env TERM=xterm-256color LC_CTYPE=zh_CN.UTF-8 emacsclient -nw -c'
alias ec='env TERM=xterm-256color LC_CTYPE=zh_CN.UTF-8 emacsclient -nw -c'
# alias ec='env LC_CTYPE=zh_CN.UTF-8 emacsclient -c &'
alias te='env TERM=xterm-256color LC_CTYPE=zh_CN.UTF-8 emacsclient -nw -c'

alias lg='lazygit'
alias ra='ranger'

alias cat='bat'

# alias proxys="export https_proxy=http://127.0.0.1:7890 http_proxy=http://127.0.0.1:7890 all_proxy=socks5://127.0.0.1:7890 && curl ip.sb"
# alias proxys="export https_proxy=http://127.0.0.1:7890 http_proxy=http://127.0.0.1:7890 all_proxy=socks5://127.0.0.1:7890"
alias proxys="export https_proxy=http://192.168.0.101:7890 http_proxy=http://192.168.0.101:7890 all_proxy=socks5://192.168.0.101:7890"
alias proxyu="unset http_proxy https_proxy all_proxy"

# pacman
alias spsyu="sudo pacman -Syu"
alias sps="sudo pacman -S "
alias spss="sudo pacman -Ss "
alias spr="sudo pacman -R "

# Search your history
alias h='history | grep'

# Find a file?
alias f="find . | grep"

# Search running processe
alias p="ps aux | grep"

# Search a font
alias fcl="fc-list"
alias fcg="fc-list | grep"

# Search where an alias was defined(eg. falias 'git pull')
alias fa='zsh -ixc : 2>&1 | grep'

# Search bindkey
alias fk='bindkey | fzf'

# Search using grep
# __grep_file() { grep -iRl $1 ./ }
__grep_file() { grep -iR $1 ./ }
alias g=__grep_file

# python virtualenv
alias pv='virtualenv venv'
alias pva='. venv/bin/activate'
alias pvd=deactivate

# sort && uniq file
alias sort-uniq='sort -u '

# https://github.com/man-pages-zh/manpages-zh
alias cman='man -M /usr/local/share/man/zh_CN'

# jupyter lab, pip install jupyterlab
alias jl='jupyter lab'

# jupyter notebook, pip install notebook
alias jnb='jupyter notebook'

