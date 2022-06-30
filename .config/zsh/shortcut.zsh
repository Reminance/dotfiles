# toggle background/foreground
fancy-ctrl-z () {
if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
else
    zle push-input
    zle clear-screen
fi
}
zle -N fancy-ctrl-z
bindkey '^z' fancy-ctrl-z

# fkill - kill process
fkill() {
    local pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    [ "x$pid" != "x" ] && echo $pid | xargs sudo kill -${1:-9}
    zle reset-prompt
}
zle -N fkill
bindkey '^Q' fkill

# edit config file
edit-config-file () {
    # local files=($(cat ~/dotfiles/config-file.list | fzf --query="$1" --select-1 --exit-0))
    local files=($(eval "echo \"$(cat ~/dotfiles/config-file.list)\"" | fzf --query="$1" --select-1 --exit-0))
    [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
    zle reset-prompt
}
zle -N edit-config-file
bindkey '\ee' edit-config-file

# copy password
copy_password () {
    local gpg=($(eval "cd ~/dotfiles/password-store.symlink && print -rl -- **/*.gpg(D.om)" | sed 's/.gpg//g' | fzf --query="$1" --select-1 --exit-0))
    [[ -n "$gpg" ]] && pass -c "${gpg[@]}"
    zle reset-prompt
}
zle -N copy_password
bindkey '\ep' copy_password

# edit config file
git-worktree-add-branch () {
    local branch=($(git branch -a | fzf --query="$1" --select-1 --exit-0))
    [[ -n "$branch" ]] && git worktree add -b "${branch##*/}" ../"${branch##*/}" "${branch[@]}"
    zle reset-prompt
}
zle -N git-worktree-add-branch
bindkey '\eg' git-worktree-add-branch

# mycli-login
mycli-login() {
    local db=($(echo -e 'localhost\ndocker\naliyun\ndev\ntest\nwmd-test' | fzf --query="$1" --select-1 --exit-0 --height 8))
    [[ -n "$db" ]] && mycli --login-path "${db[@]}" < $TTY
    zle reset-prompt
}
zle -N mycli-login
bindkey '\em' mycli-login

# file search&edit
fe() {
    IFS=$'\n' local files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
    [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# file directory&enter; won't use this function, use sps fd instead
#fd() {
#  local dir
#  dir=$(find ${1:-.} -path '*/\.*' -prune \
#				  -o -type d -print 2> /dev/null | fzf +m) &&
#  cd "$dir"
#}

# tm - create new tmux session, or switch to existing one. Works from within tmux too. (@bag-man)
# `tm` will allow you to select your tmux session via fzf.
# `tm irc` will attach to the irc session (if it exists), else it will create it.
tm() {
    [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
    if [ $1 ]; then
        tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1"); return
    fi
    session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --exit-0) &&  tmux $change -t "$session" || echo "No sessions found."
}

# fs [FUZZY PATTERN] - Select selected tmux session
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fs() {
    local session
    session=$(tmux list-sessions -F "#{session_name}" | \
        fzf --query="$1" --select-1 --exit-0) &&
        tmux switch-client -t "$session"
    }

# sudo pacman -S lf
# A terminal file manager inspred by ranger written in Go
# lfcd () {
#     tmp="$(mktemp)"
#     lf -last-dir-path="$tmp" "$@"
#     if [ -f "$tmp" ]; then
#         dir="$(cat "$tmp")"
#         rm -f "$tmp"
#         [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
#     fi
# }
# bindkey -s '^o' 'lfcd\n'

# Edit line in vim with ctrl-v
# autoload edit-command-line; zle -N edit-command-line
# bindkey '^v' edit-command-line

urlencode() {
  local length="${#1}"
  for (( i = 0; i < length; i++ )); do
    local c="${1:i:1}"
    case $c in
      [a-zA-Z0-9.~_-]) printf "$c" ;;
    *) printf "$c" | xxd -p -c1 | while read x;do printf "%%%s" "$x";done
  esac
done
}

function urldecode() { : "${*//+/ }"; echo -e "${_//%/\\x}"; }



###---------- ARCHIVE EXTRACT ----------###

ex ()
{
    if [ -f $1 ] ; then
      case $1 in
        *.tar.bz2)   tar xjf $1   ;;
        *.tar.gz)    tar xzf $1   ;;
        *.bz2)       bunzip2 $1   ;;
        *.rar)       unrar x $1   ;;
        *.gz)        gunzip $1    ;;
        *.tar)       tar xf $1    ;;
        *.tbz2)      tar xjf $1   ;;
        *.tgz)       tar xzf $1   ;;
        *.zip)       unzip $1     ;;
        *.Z)         uncompress $1;;
        *.7z)        7za e x $1   ;;
        *.deb)       ar x $1      ;;
        *.tar.xz)    tar xf $1    ;;
        *.tar.zst)   unzstd $1    ;;
        *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# cat data.csv | sed 's/,/ ,/g' | column -t -s, | less -S
# function pretty_csv_sh {
#     column -t -s, -n "$@" | less -F -S -X -K
# }

function pretty_csv_sh {
    perl -pe 's/((?<=,)|(?<=^)),/ ,/g;' "$@" | column -t -s, | less  -F -S -X -K
}

