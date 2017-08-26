# -------------------
# FZF configtes before removing from the heat. Finely grate and stir through most of the Parmesan, th
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

alias md='mkdir -p'
alias rd='rmdir'

alias rm='rm -i'
alias rmd='rm -rf'

alias lns='ln -s'

alias pso='ps -eo pid,cmd | fzf'

alias ex='exit'
alias rb='reboot'
alias sd='shutdown now'

alias mm='offlineimap -o'

# -------------------
# Lastpass
# -------------------
alias lpi='lpass login `cat $MYMAILACCOUNTS/mygmail`'
alias lpo='lpass logout'
alias lpz='lpass ls | fzf'
alias lps='lpass show --password -c'


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
alias work='cd $MYWORK'
alias bin='cd $MYBIN'
alias org='cd $MYORG'

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
autoload -U compinit
compinit
zstyle ':completion:*' menu select
setopt extended_glob
source $HOME/.dynamic-colors/completions/dynamic-colors.zsh


# -------------------
# Turn off annoyances
# -------------------
unsetopt beep nomatch

# -------------------
# Systemd
# -------------------
alias sc='systemctl'
alias scu='systemctl --user'

# -------------------
# Editors
# -------------------
alias nn='nvim'
alias bounce='systemctl restart --user emacs'

# -------------------
# Manage Config Files
# -------------------
alias nv='nvim $MYETC/neovimrc'
alias nx='nvim $MYETC/xinitrc $MYETC/Xresources $MYETC/xmonad.hs $MYETC/xmobarrc'
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
alias xx='xmonad --recompile'

alias dark='/home/nuchs/.dynamic-colors/bin/dynamic-colors switch solarized-dark'
alias light='/home/nuchs/.dynamic-colors/bin/dynamic-colors switch solarized-light'

dark


# -------------------
# Sound
# -------------------
alias bt='bluetoothctl'


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


