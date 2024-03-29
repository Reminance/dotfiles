export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --inline-info'
# export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
# export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
# export FZF_DEFAULT_COMMAND='fd -t f -H -I'

#To apply the command to CTRL-T as well
# export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_COMMAND="fd -t f -H -I --exclude \".git\""

# export FZF_COMPLETION_TRIGGER='**'

# export FZF_ALT_C_COMMAND="fd -t d --hidden --follow --exclude \".git\" . $HOME"
export FZF_ALT_C_COMMAND="fd -t d --hidden --follow --exclude \".git\""

# for preview
export FZF_CTRL_T_OPTS='--bind "ctrl-y:preview-up,ctrl-e:preview-down,?:toggle-preview" --preview "bat --style=numbers --color=always --line-range :200 {}"'
export FZF_ALT_C_OPTS='--preview "tree -C {} | head -50"'

export FZF_TMUX=1
export FZF_TMUX_HEIGHT='80%'

# for macos
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# for archlinux
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
