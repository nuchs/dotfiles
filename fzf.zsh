# Setup fzf
# ---------
if [[ ! "$PATH" == */home/nuchs/etc/fzf/bin* ]]; then
  export PATH="$PATH:/home/nuchs/etc/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/nuchs/etc/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/nuchs/etc/fzf/shell/key-bindings.zsh"

