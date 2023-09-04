export EDITOR='vim'
export PAGER='less'

export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export MYBIN="$HOME/bin"
export MYETC="$HOME/etc"
export NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="$MYBIN:$NPM_PACKAGES/bin:$PATH:$PATH"
export MANPATH="$NPM_PACKAGES/shareman:$MANPATH"

. "$HOME/.bashrc"
