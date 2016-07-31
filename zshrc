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

alias apps='cd $HOME/apps'
alias buns='cd $HOME/bin'
alias conf='cd $HOME/etc'
alias dl='cd $HOME/downloads'
alias docs='cd $HOME/docs/files'
alias etc='cd $HOME/etc'
alias tmp='cd $HOME/tmp'
alias work='cd $HOME/work'
alias vf='cd /home/sbrown/.vim/bundle/'

# ----------------
# History options
# ----------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups hist_ignore_space append_history
alias h='history'
alias hg='history | ag'
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# ----------------
# Completion
# ----------------
autoload -U compinit
compinit
zstyle ':completion:*' menu select
setopt extended_glob

# ----------------
# Turn off annoyances
# ----------------
unsetopt beep nomatch

# ----------------
# Aliases
# ----------------

alias ff='firefox'
alias vim='vim --servername vim'
alias vi='vim'
alias gg='gvim'
alias vv='vim'
alias gvd='gvimdiff'
alias vvd='vimdiff'
alias vvz='callvim $MYETC/zshrc'
alias vve='callvim $MYETC/zshenv'
alias vvv='callvim $MYETC/vimrc'
alias vvt='callvim $MYETC/tmux.conf'
alias viz='vim $MYETC/zshrc'
alias vie='vim $MYETC/zshenv'
alias viv='vim $MYETC/vimrc'
alias vit='vim $MYETC/tmux.conf'
alias szsh='source $MYETC/zshrc'
alias senv='source $MYETC/zshenv'

alias lisp='rlwrap -r -c -f $MYETC/scheme-bindings mit-scheme'

alias ls='ls  -p --color=always'
alias ll='ls -lh'
alias la='ls -la'
alias lrt='ls -lrt'

alias md='mkdir -p'
alias rd='rmdir'

alias rm='rm -i'
alias rmd='rm -rf'
alias lns='ln -s'

alias x='chmod 755'
alias all='chmod 777'

alias pso='ps -eo pid,cmd'

alias ex='exit'

alias -s tex='vim'
alias -s html='vim'
alias -s css='vim'
alias -s js='vim'
alias -s hs='vim'

alias pm='pacman'
alias pmq='pacman -Qs'
alias pmm='pacman -Qm'
alias pms='pacman -Ss'
alias pmi='sudo pacman -S --needed'
alias pmu='sudo pacman -U'
alias pmy='sudo pacman -Syu'
alias pmr='sudo pacman -Rns'
alias pma='makepkg -sri'
alias lsorphans='sudo pacman -Qdt'
alias explicit='pacman -Qei | awk '"'"'/^Name/ { name=$3 } /^Groups/ { if ( $3 != "base" && $3 != "base-devel" ) { print name } }'"'"

alias tt='tmux'

alias ww='w3m'
alias wg='w3m www.google.co.uk'

alias af='ag -i -g'

alias gog='cd $HOME/apps/GoGrinder; java -jar GoGrinder.jar'
alias gp='cd $HOME/apps/GoGrinder/problems'

alias re='rustc --explain'

# ----------------
# Functions
# ----------------

function rem()
{
	ag "$@" $MYETC/*;
}

function zem()
{
	ag "$@" $MYETC/zsh* /etc/zsh/* /etc/profile;
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

# ---------------------------
# Oh my zsh - colorize plugin
# ---------------------------

alias col='colorize_via_pygmentize'

colorize_via_pygmentize() {
    if [ ! -x "$(which pygmentize)" ]; then
        echo "package \'pygmentize\' is not installed!"
        return -1
    fi

    if [ $# -eq 0 ]; then
        pygmentize -g $@
    fi

    for FNAME in $@
    do
        filename=$(basename "$FNAME")
        lexer=`pygmentize -N \"$filename\"`
        if [ "Z$lexer" != "Ztext" ]; then
            pygmentize -l $lexer "$FNAME"
        else
            pygmentize -g "$FNAME"
        fi
    done
}

# ----------------------------------
# Oh my zsh - vim interaction plugin
# ----------------------------------

function resolveFile
{
  if [ -f "$1" ]; then
    echo $(readlink -f "$1")
  elif [[ "${1#/}" == "$1" ]]; then
    echo "$PWD/$1"
  else
    echo $1
  fi
}

function callvim
{
  if [[ $# == 0 ]]; then
    cat <<EOH
usage: callvim [-b cmd] [-a cmd] [file ... fileN]
  -b cmd     Run this command in GVIM before editing the first file
  -a cmd     Run this command in GVIM after editing the first file
  file       The file to edit
  ... fileN  The other files to add to the argslist
EOH
    return 0
  fi

  local cmd=""
  local before="<esc>"
  local after=""
  while getopts ":b:a:" option
  do
    case $option in
      a) after="$OPTARG"
         ;;
      b) before="$OPTARG"
         ;;
    esac
  done
  shift $((OPTIND-1))
  if [[ ${after#:} != $after && ${after%<cr>} == $after ]]; then
    after="$after<cr>"
  fi
  if [[ ${before#:} != $before && ${before%<cr>} == $before ]]; then
    before="$before<cr>"
  fi
  local files=""
  for f in $@
  do
    files="$files $(resolveFile $f)"
  done
  if [[ -n $files ]]; then
    files=':args! '"$files<cr>"
  fi
  cmd="$before$files$after"
  vim --remote-send "$cmd"
  if typeset -f postCallVim > /dev/null; then
    postCallVim
  fi
}

alias v=callvim
alias vvs="callvim -b':vsp'"
alias vhs="callvim -b':sp'"
alias vk="callvim -b':wincmd k'"
alias vj="callvim -b':wincmd j'"
alias vl="callvim -b':wincmd l'"
alias vh="callvim -b':wincmd h'"

