export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --inline-info'
# export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAULT_COMMAND='fd -t f -I'
export FZF_COMPLETION_TRIGGER='\'
export FZF_TMUX=1
export FZF_TMUX_HEIGHT='80%'

source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
