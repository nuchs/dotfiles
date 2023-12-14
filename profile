eval `ssh-agent`
ssh-add

export EDITOR='nvim'
export PAGER='less'

export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export MYBIN="$HOME/bin"
export MYETC="$HOME/etc"
export MYDOC="$HOME/docs"
export GOPATH="$HOME/go"
export PATH="$MYBIN:$GOPATH/bin:$PATH"
export MANPATH="$NPM_PACKAGES/shareman:$MANPATH"

. "$HOME/.bashrc"
