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
    local files=($(cat ~/dotfiles/config-file.list | fzf --query="$1" --select-1 --exit-0))
    [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
    zle reset-prompt
}
zle -N edit-config-file
bindkey '\ee' edit-config-file

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
