#!/bin/bash

# ========== Terminal Config {{{1
case $- in
*i*) ;;
*) return ;;
esac

# auto-attach/new a tmux session on interactive shells
if [[ -z $NO_TMUX_AUTO && -z $TMUX && $- == *i* ]] && command -v tmux >/dev/null; then
  if tmux has-session 2>/dev/null; then
    exec tmux attach
  else
    tmux start-server
    ~/.tmux/plugins/tmux-continuum/scripts/continuum_restore.sh
    exec tmux
  fi
fi

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# ** matches all directories
shopt -s globstar

# Turn on vi keybindings
set -o vi
bind '"jk":vi-movement-mode'

# Adding wsl-open as a browser for WSL
if [[ $(uname -r) =~ (m|M)icrosoft ]]; then
  if [[ -z $BROWSER ]]; then
    export BROWSER=wsl-open
  else
    export BROWSER=$BROWSER:wsl-open
  fi
fi

# ========== Load 3rd party config  {{{1
export GPG_TTY=$(tty)

# Rust
. "$HOME/.cargo/env"

# FZF
eval "$(fzf --bash)"

# Zoxide
eval "$(zoxide init bash)"

# ========== Configuration {{{1
alias sb='source ~/.bashrc'
alias sp='source ~/.bash_profile'

function rem() {
  rg -i -g '!nvim' $@ $MYETC | fzf
}

# ========== Commands {{{1
alias n='nvim'

alias ls="ls --classify=always --group-directories-first --color=always"
alias ll="ls -hl --classify=always --group-directories-first --color=always"
alias la="ls -hAl --classify=always --group-directories-first --color=always"
alias tt='tree -FC --filesfirst'

alias md='mkdir -p'
alias rd='rmdir'
alias rmd='rm -rf'

alias lns='ln -s'

alias up="ping -c 1 www.google.com"
alias x='exit'

alias cat='bat'
alias b='bat'

alias no='noted'
alias non='noted --new'
alias nos='noted --search'

function archive {
  if [ -z "$1" ]; then
    cd "$MYARCH" || return
  fi

  mv $@ "$MYARCH"
}
alias aa='archive'

function dlcp() {
  local D=/mnt/c/Users/sjbro/Downloads
  find "$D" -mindepth 1 -maxdepth 1 -printf '%f\0' \
  | fzf --read0 --print0 --multi --height=40% --reverse \
  | xargs -0 -I{} cp -iv -- "$D/{}" .
}

# ========== Management {{{1
alias pm='paru'
alias orphans='pacman -Qtdq'

# ========== History {{{1

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=3000
HISTFILESIZE=3000

# append to the history file, don't overwrite it
shopt -s histappend
alias hh='history'
alias hg='$(history | fzf | awk '"'"'{$1=""}1'"'"')'

# ========== Navigation {{{1
function record_and_move() {
  z $@
  echo "BASH_LAST_DIR=$PWD" >~/.bash_lastdir
}
alias lastd='bat ~/.bash_lastdir'

alias rr='cd "$(git rev-parse --show-toplevel)"'
alias zz='zi'
alias zx='zoxide query -l'
alias z='record_and_move'
alias cd='record_and_move'

# FZF options for Zoxide
export _ZO_FZF_OPTS='--no-sort --bind=ctrl-z:ignore,btab:up,tab:down --cycle --keep-right --border=sharp --height=45% --info=inline --layout=reverse --tabstop=1 --exit-0 --select-1 --delimiter="\t" --nth=2 --read0 --preview="command ls -Cp --color=always --group-directories-first {2..}" --preview-window=down,30%,sharp'

# Start in the last used directory
if [ "$PWD" == "$HOME" -a -f ~/.bash_lastdir ]; then
  source ~/.bash_lastdir
  record_and_move $BASH_LAST_DIR
fi

# ========== Tmux {{{1
alias t='tmux'
alias tl='tmux ls'
alias tk='tmux kill-session -t'
alias ta='tmux attach-session'

function open-popup {
  if [ -z "$1" ]; then
    echo "usage: open-popup <command> [args...]"
    return
  fi
  CMD=$1
  shift

  if [[ -z "$TMUX" ]]; then
    command -v $CMD >/dev/null || { echo "$CMD not found"; return 127; }
    exec $CMD $@
  fi

 tmux display-popup -E -w 90% -h 90% -d "#{pane_current_path}" "bash -lc '$CMD $@'"
}

alias lg='open-popup lazygit'
function k9 {
  open-popup "ps -ef | fzf | awk '{print \$2}' | xargs kill -9"
}

# ========== Dev {{{1

# --- git {{{2
function git-ignore() {
  if [ -z "$1" ]; then
    echo "usage: git-ignore <language> | -l "
    return
  fi

  if [ "$1" = "-l" ]; then
    ls $MYDOC/templates
    return
  fi

  cp $MYDOC/templates/$1.gitignore .gitignore
}

function github-clone {
  if [ -n "$2" ]; then
    who="$1"
    shift
  else
    who="nuchs"
  fi

  what="$1"

  git clone -v git@github.com:${who}/${what}.git
}

alias ghc='github-clone'
alias gcf="git diff --name-only"
alias gi='git-ignore'
alias ga='git add'
alias gc='git commit'
alias gd='git diff --color-words'
alias gds='git diff --cached --color-words'
alias gf='git fetch'
alias gs='git status'
alias gp='git push'
alias gup='git push -u origin HEAD'
alias gpl='git pull'
alias gb='git branch'
alias gco='git checkout'
alias gcob='git checkout -b'
alias gcom='git checkout main'
alias gff='git merge --ff-only origin/master'
alias gunlock='rm .git/index.lock'
alias gl='git log -n 10 --all --graph --format=format:"%C(bold blue)%h%Creset - %C(bold cyan)%aD%C(auto)%d%n    %s%n    %C(dim white)- %an <%ae> %C(auto)%G?"'
alias gla='git log --all --graph --format=format:"%C(bold blue)%h%Creset - %C(bold cyan)%a%D%C(auto)%d%n    %s%n    %C(dim white)- %an <%ae> %C(auto)%G?"'
alias gls='git log --oneline --name-status --'

# --- go {{{2
alias dlvd='dlv debug --headless --listen :8888 --api-version 2'
alias dlvt='dlv test --headless --listen :8888 --api-version 2'
alias dlvc='dlv connect :8888'

function my-go-mod() {
  if [ -z "$1" ]; then
    echo "usage: my-go-mod <name>"
    return
  fi
  go mod init "github.com/nuchs/$1"
}

alias gomi='my-go-mod'
alias gomt='go mod tidy'
alias got='swatch go test -count=1 ./...| colourise -p gotest'
alias clogs='colourise -p slog'

# --- docker {{{2
alias dk='docker'
alias dkc='docker container'
alias dki='docker image'
alias dkn='docker network'
alias dkv='docker volume'
alias dkp='docker-compose'

# --- k8s {{{2
alias kc='kubectl'

# --- Python {{{2
alias py='python3'

# --- Dotnet {{{2
alias dn='dotnet'

# --- Ollama {{{2
alias ol='ollama'

# ========== Prompt {{{1
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
  if [ ! -f /sys/class/power_supply/BAT1/capacity ]; then
    return
  fi

  LEVEL=$(cat /sys/class/power_supply/BAT1/capacity)
  CHARGING=$(cat /sys/class/power_supply/BAT1/status)
  BCOL=""

  if [[ $CHARGING == "Charging" ]]; then
    BCOL="$BOLD${GREEN}"
  elif [[ $LEVEL -lt 15 ]]; then
    BCOL="$BOLD${RED}"
  elif [[ $LEVEL -lt 30 ]]; then
    BCOL="$BOLD${YELLOW}"
  fi

  printf "$BCOL$LEVEL%%${RESET} "
}

function git_prompt_read {
  local f="$1"
  shift
  [[ -r "$f" ]] && read -r "$@" <"$f"
}

function print_git_status {
  STATUS="$(git status 2>/dev/null)"
  if [[ $? -ne 0 ]]; then return; fi

  printf "$SEP"
  if echo $STATUS | grep -c "branch is ahead" &>/dev/null; then
    printf "$GREEN< $RESET"
  fi
  if echo $STATUS | grep -c "branch is behind" &>/dev/null; then
    printf "$RED> $RESET"
  fi

  REF=$(git symbolic-ref HEAD 2>/dev/null)
  printf "$PURPLE${REF#refs/heads}$RESET "

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
    printf "$INVERT$STATE $STEP/$TOTAL$RESET "
  fi

  CHANGES=""
  if [[ $STATUS =~ "fix conflicts" ]]; then CHANGES="$RED${INVERT}X$RESET"; fi
  if [[ $STATUS =~ "Untracked files" ]]; then CHANGES="$CHANGES$RED?$RESET"; fi
  if [[ $STATUS =~ "not staged" ]]; then CHANGES="$CHANGES$YELLOW+$RESET"; fi
  if [[ $STATUS =~ "to be committed" ]]; then CHANGES="$CHANGES$GREEN*$RESET"; fi
  if [[ ! -z $CHANGES ]]; then printf "$PURPLE[$CHANGES$PURPLE]$RESET "; fi
}

SEP="$BOLD|$RESET "
TIME="$DIM\t$RESET "
DIR="$TURQ\w$RESET "
export PS1="$RESET$TIME\$(print_battery)$SEP$DIR\$(print_git_status)\n↳ "
