export GPG_TTY=$(tty)
export EDITOR='vim'
export PAGER='most'

export MYETC="$HOME/etc"
export MYBIN="$HOME/bin"
export NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$MYBIN:$NPM_PACKAGES/bin:$PATH:$PATH"
export MANPATH="$NPM_PACKAGES/shareman:$MANPATH"

. "$HOME/.bashrc"
