# -------------------
# FZF 
# -------------------
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'


# -------------------
# Shell keybindings
# -------------------
bindkey -v
bindkey '^w' backward-kill-word
bindkey -M vicmd '/' fzf-history-widget
bindkey -M viins 'jk' vi-cmd-mode  
 

# -------------------
# Basic Shell Cmds
# -------------------
alias ls='ls  -p --color=always'
alias ll='ls -lh'
alias la='ls -la'
alias lrt='ls -lrt'
alias tt='tree'

alias md='mkdir -p'
alias rd='rmdir'

alias rm='rm -i'
alias rmd='rm -rf'

alias lns='ln -s'

alias pso='ps -eo pid,cmd | fzf'

alias up='ping -c 2 www.google.com'
alias ex='exit'

alias mm='offlineimap -o'

# -------------------
# Lastpass
# -------------------
alias lpi='lpass login `cat $MYMAILACCOUNTS/mygmail`'
alias lpo='lpass logout'
alias lps='lastPassShow'
alias lpc='lastPassCopy'

function lastPassShow
{
   lpass show --password $(lpass ls  | fzf | awk '{print $(NF)}' | sed 's/\]//g') 
}

function lastPassCopy
{
   lpass show -c --password $(lpass ls  | fzf | awk '{print $(NF)}' | sed 's/\]//g') 
}

# -------------------
# Navigation
# -------------------
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome autolist

alias -g    ...='../..'
alias -g   ....='../../..'
alias -g  .....='../../../..'
alias -g ......='../../../../..'

alias p='popd'
alias pp='pushd .'
alias d='dirs -v'

alias dl='cd $MYDOWNLOADS'
alias docs='cd $MYDOCS'
alias etc='cd $MYETC'
alias bin='cd $MYBIN'
alias org='cd $MYORG'

alias ww='cd $MYWORK'
alias so='cd $MYWORK/so'
alias toy='cd $MYWORK/toys'
alias bb='cd $MYWORK/blog'

function tardis()
{
  ssh tardis;
  dark
}


# -------------------
# History options
# -------------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups hist_ignore_space append_history
alias h='history'
alias hg='history | rg'
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search


# -------------------
# Completion
# -------------------
fpath+=~/.zfunc
autoload -U compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(/home/nuchs/.local/bin/stack --bash-completion-script stack)"
zstyle ':completion:*' menu select
setopt extended_glob


# -------------------
# Turn off annoyances
# -------------------
unsetopt beep nomatch

# -------------------
# System controls
# -------------------
alias sc='systemctl'
alias scu='systemctl --user'

alias bt='bluetoothctl'
alias home='sudo netctl start wlp2s0-PLUSNET-P68MQQ'

function cleanReboot
{
    systemctl stop --user emacs
    reboot
}

function cleanShutdown
{
    systemctl stop --user emacs
    shutdown now
}

alias rb='cleanReboot'
alias sd='cleanShutdown'

# -------------------
# Editors
# -------------------
alias nn='nvim'
alias bounce='systemctl restart --user emacs'


# -------------------
# Manage Config Files
# -------------------
alias nv='nvim $MYETC/neovimrc'
alias nx='nvim $MYETC/xinitrc $MYETC/Xresources'
alias nz='nvim $MYETC/zshrc $MYETC/zprofile'
alias eem='ee $MYETC/emacs.d/init.el'

alias sz='source $MYETC/zshrc'
alias sp='source $MYETC/zprofile'

function rem()
{
  rg "$@" $MYETC/* | fzf;
}

function zem()
{
  rg "$@" $MYETC/zsh* /etc/zsh/* /etc/profile | fzf;
}


# -------------------
# Package management
# -------------------
alias pm='aura'
alias pms='sudo aura'
alias lsorphans='pacman -Qdt'
alias explicit='pacman -Qei | awk '"'"'/^Name/ { name=$3 } /^Groups/ { if ( $3 != "base" && $3 != "base-devel" ) { print name } }'"'"
alias rh='rehash'


# -------------------
# X Windows stuff
# -------------------
alias x='startx'

alias bright='sudo brightness.sh 100'
alias dull='sudo brightness.sh 20'


# -------------------
# Git
# -------------------
alias gs='git status'
alias ga='git add'
alias gd='git diff'
alias gc='git commit'
alias gpom='git push origin master'


# -------------------
# Prompt
# -------------------
setopt prompt_subst prompt_percent
autoload -U colors && colors # Enable colors in prompt

vim_ins_mode="[I]"
vim_cmd_mode="[N]"
vim_mode=$vim_ins_mode

function zle-keymap-select {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
  zle reset-prompt
}
zle -N zle-keymap-select

function zle-line-finish {
  vim_mode=$vim_ins_mode
}
zle -N zle-line-finish

PROMPT='%{$fg[green]%}${vim_mode}%{$reset_color%} %* %{$fg[blue]%}%n@%m%{$reset_color%}:%~
$ '


