export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --inline-info'
#export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
# export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
#export FZF_DEFAULT_COMMAND='fd -t f -H -I'

#To apply the command to CTRL-T as well
#export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_COMMAND="fd -t f -H -I"

# export FZF_COMPLETION_TRIGGER='**'

#for preview
export FZF_CTRL_T_OPTS='--preview "bat --style=numbers --color=always --line-range :200 {}"'
export FZF_ALT_C_OPTS='--preview "tree -C {} | head -50"'

export FZF_TMUX=1
export FZF_TMUX_HEIGHT='80%'

source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
