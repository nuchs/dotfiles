# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
alias viv='vim ~/.vimrc'
alias vb='vim ~/.bashrc'
alias vp='vim ~/.profile'
alias sb='source ~/.bashrc'
alias sp='source ~/.profile'

function rem() {
  grep $@ ~/.bashrc ~/.profile
}

# ----------------------------------------------------------------------
# Basic Commands
# ----------------------------------------------------------------------

alias l='exa'
alias ll='exa -l'
alias lla='exa -la'
alias la='exa -lad .?*'
alias lrt='exa -lus accessed'

alias md='mkdir -p'
alias rd='rmdir'
alias rm='rm -i'
alias rmd='rm -rf'

alias h='history'
alias hg='history | rg'

alias lns='ln -s'

alias up="ping -c 2 www.google.com"
alias x='exit'

alias tt='tmux'
alias ps='procs'
alias ddg='ddgr -n 6'
alias df='duf -only local'
alias pv="fzf --preview='bat {}' --bind shift-up:preview-page-up,shift-down:preview-page-down"
alias n='nnn'

# ----------------------------------------------------------------------
# History
# ----------------------------------------------------------------------

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=3000
HISTFILESIZE=3000

# append to the history file, don't overwrite it
shopt -s histappend

# ----------------------------------------------------------------------
# Display
# ----------------------------------------------------------------------

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# ----------------------------------------------------------------------
# Navigation
# ----------------------------------------------------------------------

alias ww='cd $MYWORK'
alias ee='cd $MYETC'
alias bun='cd $MYBIN'
alias rr='cd "$(git rev-parse --show-toplevel)"'

alias p='pushd .'
alias pp='popd'
alias d='dirs -v'

alias ex='explorer.exe .'

# ** matches all directories
shopt -s globstar

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


# ----------------------------------------------------------------------
# Dev
# ----------------------------------------------------------------------

alias v='vim'

# git
alias ga='git add'
alias gc='git commit'
alias gd='git diff'
alias gds='git diff --cached'
alias gs='git status'
alias gp='git push'
alias gup='git push -u origin'
alias gpl='git pull'
alias gb='git branch'
alias go='git checkout'
alias gob='git checkout -b'
alias gom='git checkout main'
alias gff='git merge --ff-only origin/master'
alias gl='git log -n 10 --all --graph --format=format:"%C(bold blue)%h%Creset - %C(bold cyan)%a%D%C(auto)%d%n    %s%n    %C(dim white)- %an <%ae> %C(auto)%G?"'
alias gla='git log --all --graph --format=format:"%C(bold blue)%h%Creset - %C(bold cyan)%a%D%C(auto)%d%n    %s%n    %C(dim white)- %an <%ae> %C(auto)%G?"'

# docker
alias dk='docker'
alias dkc='docker container'
alias dki='docker image'
alias dkn='docker network'
alias dkv='docker volume'
alias dkp='docker-compose'

# k8s
alias kc='kubectl'

# ----------------------------------------------------------------------
# Prompt
# ----------------------------------------------------------------------

RESET="\e[0m"
BOLD="\e[1m"
DIM="\e[2m"
INVERT="\e[7m"
TURQ="\e[36m"
PURPLE="\e[35m"
GREEN="\e[32m"
YELLOW="\e[33m"
RED="\e[91m"

function print_battery {
  if [ ! -f /sys/class/power_supply/BAT1/capacity ]
  then
    return 0
  fi

  LEVEL=$(cat /sys/class/power_supply/BAT1/capacity)
  CHARGING=$(cat /sys/class/power_supply/BAT1/status)
  BCOL=""

  if   [[ $CHARGING == "Charging" ]]; then BCOL="$BOLD${GREEN}"
  elif [[ $LEVEL -lt 15 ]];           then BCOL="$BOLD${RED}"
  elif [[ $LEVEL -lt 30 ]];           then BCOL="$BOLD${YELLOW}"
  fi

  printf "$BCOL$LEVEL%%${RESET} "
}

function git_prompt_read {
  local f="$1"
  shift
  [[ -r "$f" ]] && read -r "$@" <"$f"
}

function print_git_status {
  STATUS="$(git status 2> /dev/null)"
  if [[ $? -ne 0 ]]; then printf ""; return; fi

  SPACER=""
  printf " $RESET$BOLD|$RESET $PURPLE"
  if echo $STATUS | grep -c "branch is ahead" &> /dev/null
  then
    printf "$RESET$GREEN<"
    SPACER=" "
  fi
  if echo $STATUS | grep -c "branch is behind" &> /dev/null
  then
    printf "$RESET$RED>"
    SPACER=" "
  fi
  printf "$SPACER"

  REF=$(git symbolic-ref HEAD 2> /dev/null)
  printf "$RESET$PURPLE${REF#refs/heads}"

  GIT_DIR="$(git rev-parse --git-dir 2>/dev/null)"
  STATE=""
  STEP=""
  TOTAL=""
  if [[ -d "$GIT_DIR/rebase-merge" ]]; then
    git_prompt_read "$GIT_DIR/rebase-merge/msgnum" STEP
    git_prompt_read "$GIT_DIR/rebase-merge/end" TOTAL
    if [[ -f "$GIT_DIR/rebase-merge/interactive" ]]; then
      STATE="|REBASE-i"
    else
      STATE="|REBASE-m"
    fi
  else
    if [[ -d "$GIT_DIR/rebase-apply" ]]; then
      git_prompt_read "$GIT_DIR/rebase-apply/next" STEP
      git_prompt_read "$GIT_DIR/rebase-apply/last" TOTAL
      if [[ -f "$GIT_DIR/rebase-apply/rebasing" ]]; then
        STATE="|REBASE"
      elif [[ -f "$GIT_DIR/rebase-apply/applying" ]]; then
        STATE="|AM"
      else
        STATE="|AM/REBASE"
      fi
    elif [[ -f "$GIT_DIR/MERGE_HEAD" ]]; then
      STATE="|MERGING"
    elif [[ -f "$GIT_DIR/CHERRY_PICK_HEAD" ]]; then
      STATE="|CHERRY_PICKING"
    elif [[ -f "$GIT_DIR/REVERT_HEAD" ]]; then
      STATE="|REVERTING"
    elif [[ -f "$GIT_DIR/BISECT_LOG" ]]; then
      STATE="|BISECTING"
    fi
  fi

  if [[ -n "$STEP" ]] && [[ -n "$TOTAL" ]]; then
    printf "$INVERT$STATE $STEP/$TOTAL$RESET"
  fi

  CHANGES=""
  if [[ $STATUS =~ "fix conflicts"   ]]; then CHANGES="$RED${INVERT}X";           fi
  if [[ $STATUS =~ "Untracked files" ]]; then CHANGES="$CHANGES$RESET${RED}?";    fi
  if [[ $STATUS =~ "not staged"      ]]; then CHANGES="$CHANGES$RESET${YELLOW}+"; fi
  if [[ $STATUS =~ "to be committed" ]]; then CHANGES="$CHANGES$RESET${GREEN}*";  fi
  if [[ ! -z $CHANGES ]]; then printf "$RESET$PURPLE [$CHANGES$PURPLE]"; fi
}

PS_INFO="$DIM\t $(print_battery)${BOLD}|${RESET} $TURQ\w"
PS_USERLINE="$RESET\n↳ "
export PS1="$PS_INFO\$(print_git_status)$PS_USERLINE"

# ----------------------------------------------------------------------
# Load 3rd Party configs
# ----------------------------------------------------------------------

# FZF
[ -f ~/.fzf.bash ] && source ~/.fzf.bash 
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# Zoxide
eval "$(zoxide init bash)"
