export MYETC="$HOME/etc"
export MYWORK="$HOME/work"
export MYBIN="$HOME/bin"
export MYDOCS="$HOME/docs"
export MYDOWNLOADS="$HOME/downloads"
export MYORG="$HOME/Dropbox/org"
export MYMAIL="$HOME/mail"
export MYMAILACCOUNTS="$HOME/.mailaccounts"

typeset -U path
path=( $MYBIN ~/.local/bin ~/.gem/ruby/2.4.0/bin ~/.cargo/bin ~/.local/bin $path)
export path

export KEYTIMEOUT=10
export EDITOR='nvim'
export PAGER='less'
export QT_AUTO_SCREEN_SCALE_FACTOR=1

export CARGO_HOME='/home/nuchs/.cargo'
export RUST_SRC_PATH='/home/nuchs/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
