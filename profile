eval `ssh-agent`
ssh-add

export EDITOR='nvim'
export PAGER='less'

export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export MYBIN="$HOME/bin"
export MYETC="$HOME/etc"
export MYDOC="$HOME/docs"
export MYAPPDATA="/mnt/c/Users/sjbro/AppData/Roaming/"
export PNPM_HOME="/home/nuchs/.local/share/pnpm"
export GOPATH="$HOME/go"
export PATH="$MYBIN:$PNPM_HOME:$GOPATH/bin:$PATH"

. "$HOME/.bashrc"
