echo '+---------+'
echo '| Aliases |'
echo '+---------+'

# ----------------
# Programs
# ----------------

alias gg='gvim'
alias gvd='gvimdiff'
alias ee='UBUNTU_MENUPROXY= eclipse'
alias id='idea.sh'
alias eclim='$HOME/apps/eclipse/eclimd'
alias go='google'

# ----------------
# Commands
# ----------------

alias via='gvim $HOME/.myconfig/bash_aliases'
alias vib='gvim $HOME/.myconfig/bashrc'
alias viv='gvim $HOME/.myconfig/vimrc'
alias sal='source $HOME/.myconfig/bash_aliases'
alias sba='source $HOME/.myconfig/bashrc'

alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -la'
alias lrt='ls -lrt'

alias p='pushd .'
alias pp='pushd'

alias md='mkdir -p'
alias rd='rmdir'

alias rm='rm -i'
alias rmd='rm -rf'
alias lns='ln -s'

alias x='chmod 755'
alias all='chmod 777'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ack='ack-grep'

alias h='history'
alias hg='history | grep'

alias pso='ps -eo pid,cmd'

alias ex='exit'

# ----------------
# Functions
# ----------------

function rem()
{
	grep $@ $HOME/.myconfig/bash*;
}

function google() {
  search=""
  echo "Googling: $@"
  for term in $@; do
      search="$search%20$term"
  done
  xdg-open "http://www.google.com/search?q=$search"
}

function nupdate()
{
  node -v
  sudo npm cache clean -f
  sudo npm install -g n
  sudo n stable
  node -v
}

# ----------------
# Locations
# ----------------

alias docs='cd $HOME/documents/files'
alias work='cd $HOME/work'
alias apps='cd $HOME/apps'
alias buns='cd $HOME/bin'
alias tmp='cd $HOME/tmp'
alias dl='cd $HOME/downloads'

