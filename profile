export EDITOR='nvim'
export PAGER='less'

export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"


export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8

export win="/mnt/c/Users/sjbro/"
export MYBIN="$HOME/bin"
export MYETC="$HOME/etc"
export MYDOC="$HOME/docs"
export MYARCH="$HOME/archive/"
export MYAPPDATA="/mnt/c/Users/sjbro/AppData/Roaming/"
export GOPATH="$HOME/go"
export PATH="$MYBIN:$GOPATH/bin:$PATH"

. "$HOME/.bashrc"
. "$HOME/.cargo/env"
