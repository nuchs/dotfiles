export EDITOR='vim'
export PAGER='less'

export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export MYBIN="$HOME/bin"
export MYETC="$HOME/etc"
export NPM_PACKAGES="${HOME}/.npm-packages"
export RUBY_BIN="${HOME}/.local/share/gem/ruby/3.0.0/bin"
export PATH="$MYBIN:$NPM_PACKAGES/bin:$RUBY_BIN:$PATH:$PATH"
export MANPATH="$NPM_PACKAGES/shareman:$MANPATH"

. "$HOME/.bashrc"
