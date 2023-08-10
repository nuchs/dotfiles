export GPG_TTY=$(tty)
export EDITOR='vim'
export PAGER='most'

export MYETC="$HOME/etc"
export MYWORK="$HOME/work"
export MYBIN="$HOME/bin"
export NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$NPM_PACKAGES/bin:$HOME/bin:$PATH"
export MANPATH="$NPM_PACKAGES/shareman:$MANPATH"

. "$HOME/.bashrc"
