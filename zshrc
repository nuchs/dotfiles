bindkey -e

# ----------------
# Navigation
# ----------------
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome autolist

alias -g    ...='../..'
alias -g   ....='../../..'
alias -g  .....='../../../..'
alias -g ......='../../../../..'

alias p='popd'
alias pp='pushd .'
alias d='dirs -v'

alias dl='cd $HOME/downloads'
alias docs='cd $HOME/docs'
alias etc='cd $HOME/etc'
alias work='cd $HOME/work'

# ----------------
# History options
# ----------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups hist_ignore_space append_history
alias h='history'
alias hg='history | rg'
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# ----------------
# Completion
# ----------------
fpath+=~/.zfunc
autoload -U compinit
compinit
zstyle ':completion:*' menu select
setopt extended_glob

# ----------------
# FZF config
# ----------------
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# ----------------
# Turn off annoyances
# ----------------
unsetopt beep nomatch

# ----------------
# Aliases
# ----------------
alias vim='nvim'
alias vi='nvim'
alias nn='nvim'
alias -s tex='ee'
alias -s html='ee'
alias -s css='ee'
alias -s js='ee'
alias -s hs='ee'

alias nv='nvim $MYETC/neovimrc'
alias ex='ee $MYETC/xinitrc $MYETC/Xdefaults $MYETC/xmonad.hs $MYETC/xmobarrc'
alias ez='ee $MYETC/zshrc $MYETC/zshenv $MYETC/zprofile'
alias em='ee $MYETC/emacs.d/init.el' $MYETC/emacs.d/config/*

alias bounce='systemctl restart --user emacs'

alias sz='source $MYETC/zshrc'
alias senv='source $MYETC/zshenv'
alias sprof='source $MYETC/zprofile'

alias ls='ls  -p --color=always'
alias ll='ls -lh'
alias la='ls -la'
alias lrt='ls -lrt'

alias tt='tree -C'

alias md='mkdir -p'
alias rd='rmdir'

alias rm='rm -i'
alias rmd='rm -rf'
alias lns='ln -s'

alias pso='ps -eo pid,cmd'

alias ex='exit'
alias rb='sudo reboot'
alias sd='sudo shutdown now'

alias pm='aura'
alias pms='sudo aura'
alias lsorphans='sudo pacman -Qdt'
alias explicit='pacman -Qei | awk '"'"'/^Name/ { name=$3 } /^Groups/ { if ( $3 != "base" && $3 != "base-devel" ) { print name } }'"'"

alias re='rustc --explain'

alias rh='rehash'

alias x='startx'
alias xx='xmonad --recompile'

# ----------------
# Functions
# ----------------

function rem()
{
  rg "$@" $MYETC/*;
}

function zem()
{
  rg "$@" $MYETC/zsh* /etc/zsh/* /etc/profile;
}

# ----------------
# Prompt
# ----------------
setopt prompt_subst prompt_percent
autoload -U colors && colors # Enable colors in prompt

# Modify the colors and symbols in these variables as desired.
GIT_PROMPT_SYMBOL="%{$fg[blue]%}±"
GIT_PROMPT_PREFIX="%{$fg[green]%}[%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[green]%}]%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[red]%}ANUM%{$reset_color%}"
GIT_PROMPT_BEHIND="%{$fg[cyan]%}BNUM%{$reset_color%}"
GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}⚡︎%{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}●%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}●%{$reset_color%}"
GIT_PROMPT_STAGED="%{$fg_bold[green]%}●%{$reset_color%}"

# Show Git branch/tag, or name-rev if on detached head
parse_git_branch() {
  (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}

# Show different symbols as appropriate for various Git repository states
parse_git_state() {

  # Compose this value via multiple conditional appends.
  local GIT_STATE=""

  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
  fi

  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
  fi

  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
  fi

  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
  fi

  if ! git diff --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
  fi

  if ! git diff --cached --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
  fi

  if [[ -n $GIT_STATE ]]; then
    echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
  fi

}

# If inside a Git repository, print its branch and state
git_prompt_string() {
  local git_where="$(parse_git_branch)"
  [ -n "$git_where" ] && echo "$GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX%{$fg[yellow]%}${git_where#(refs/heads/|tags/)}$GIT_PROMPT_SUFFIX"
}

# Set the right-hand prompt
RPS1='$(git_prompt_string)'
PROMPT='%B%*%b %{$fg[blue]%}%n@%m%{$reset_color%} : %~
%B→%b '

# -----------------------------
# Oh my zsh - git ignore plugin
# -----------------------------

function gi() { curl -sL https://www.gitignore.io/api/$@ ;}

_gitignoreio_get_command_list() {
  curl -sL https://www.gitignore.io/api/list | tr "," "\n"
}

_gitignoreio () {
  compset -P '*,'
  compadd -S '' `_gitignoreio_get_command_list`
}

compdef _gitignoreio gi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
