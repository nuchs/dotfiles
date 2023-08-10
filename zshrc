# -------------------
# ZSH Settings
# -------------------

bindkey -v
bindkey '^w' backward-kill-word
bindkey -M vicmd '/' fzf-history-widget
bindkey -M viins 'jk' vi-cmd-mode

eval `dircolors ~/.dircolors`

unsetopt beep nomatch

DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome autolist

# Completion
fpath+=~/.zfunc
autoload -U compinit && compinit
autoload -U +X bashcompinit && bashcompinit
zstyle ':completion:*' menu select
setopt extended_glob
source ~/.kube/zsh_completion

# History options
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups hist_ignore_space append_history
alias h='history'
alias hg='history | rg'
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# -------------------
# Shell Cmds
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

alias up='ping -c 2 www.google.com'
alias x='exit'

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
alias ww='cd $MYWINWORK'
alias wl='cd $MYWORK'
alias rr='cd "$(git rev-parse --show-toplevel)"'

# -------------------
# Programs & Scripts
# -------------------

alias ex='explorer.exe .'
alias vv='vim'
alias mit='rlwrap -r -c -f /mnt/d/Work/SICP/scheme.txt scheme'
alias dn='dotnet.exe'
alias tx='tmux'
alias vsc='code -r'

# GPG
export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# Kubernetes
alias kc='kubectl'

# Docker
alias dk='docker'
alias dki='docker image'
alias dkc='docker container'
alias dkcls='docker container ls --format  "table {{.ID}}\t{{.Names}}\t{{.Status}}"'
alias dkv='docker volume'
alias dkn='docker network'
alias dkp='docker compose'
alias dkm="sudo mount -t drvfs '\\\\wsl$\\docker-desktop-data\\version-pack-data\\community\\docker' /mnt/wsl/docker-desktop-data/data/docker -o ro,umask=022"

# Git
alias gs='git status'
alias ga='git add'
alias gd='git diff'
alias gc='git commit'
alias gpl='git pull'
alias gp='git push'
alias gup='git push --set-upstream origin HEAD'
alias gl='git log --graph --format=format:"%C(bold blue)%h%Creset - %C(bold cyan)%aD%C(auto)%d%n          %s%n          %C(dim white)- %an <%ae> %C(auto)%G?"'
alias gb='git branch'
alias got='git checkout'
alias gom='git checkout master'
alias gob='git checkout -b'
alias glo="find .git/objects -not -name 'pack*' -type f | awk 'BEGIN { FS=\"/\" } ; {print $3$4}'"

# -------------------
# Manage Config Files
# -------------------
alias vr='vim $HOME/.vimrc'
alias vb='vim $MYETC/zshrc $MYETC/zprofile'
alias vt='vim $HOME/.tmux.conf'

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
# Prompt
# -------------------

setopt prompt_subst
autoload -U colors && colors # Enable colors in prompt

# Echoes a username/host string when connected over SSH (empty otherwise)
ssh_info() {
  [[ "$SSH_CONNECTION" != '' ]] && echo '%(!.%{$fg[red]%}.%{$fg[yellow]%})%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:' || echo ''
}

# Echoes information about Git repository status when inside a Git repository
git_info() {

  # Exit if not inside a Git repository
  ! git rev-parse --is-inside-work-tree > /dev/null 2>&1 && return

  # Git branch/tag, or name-rev if on detached head
  local GIT_LOCATION=${$(git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD)#(refs/heads/|tags/)}

  local AHEAD="%{$fg[red]%}⇡NUM%{$reset_color%}"
  local BEHIND="%{$fg[cyan]%}⇣NUM%{$reset_color%}"
  local MERGING="%{$fg[magenta]%}⚡︎%{$reset_color%}"
  local UNTRACKED="%{$fg[red]%}●%{$reset_color%}"
  local MODIFIED="%{$fg[yellow]%}●%{$reset_color%}"
  local STAGED="%{$fg[green]%}●%{$reset_color%}"

  local -a DIVERGENCES
  local -a FLAGS

  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    DIVERGENCES+=( "${AHEAD//NUM/$NUM_AHEAD}" )
  fi

  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    DIVERGENCES+=( "${BEHIND//NUM/$NUM_BEHIND}" )
  fi

  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    FLAGS+=( "$MERGING" )
  fi

  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    FLAGS+=( "$UNTRACKED" )
  fi

  if ! git diff --quiet 2> /dev/null; then
    FLAGS+=( "$MODIFIED" )
  fi

  if ! git diff --cached --quiet 2> /dev/null; then
    FLAGS+=( "$STAGED" )
  fi

  local -a GIT_INFO
  GIT_INFO+=( "\033[38;5;15m±" )
  [ -n "$GIT_STATUS" ] && GIT_INFO+=( "$GIT_STATUS" )
  [[ ${#DIVERGENCES[@]} -ne 0 ]] && GIT_INFO+=( "${(j::)DIVERGENCES}" )
  [[ ${#FLAGS[@]} -ne 0 ]] && GIT_INFO+=( "${(j::)FLAGS}" )
  GIT_INFO+=( "\033[38;5;15m$GIT_LOCATION%{$reset_color%}" )
  echo "${(j: :)GIT_INFO}"

}

# Use ❯ as the non-root prompt character; # for root
# Change the prompt character color if the last command had a nonzero exit code
PS1='%{$fg[yellow]%}[%D{%L:%M:%S}] %{$fg[magenta]%}%~%u $(git_info)
%(?.%{$fg[blue]%}.%{$fg[red]%})%(!.#.❯)%{$reset_color%} '
