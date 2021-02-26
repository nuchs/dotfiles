export MYETC="$HOME/etc"
export MYWORK="$HOME/work"
export MYBIN="$HOME/bin"
export MYDOCS="$HOME/docs"
export MYDOWNLOADS="$HOME/downloads"

typeset -U path
path=( $MYBIN ~/.local/bin ~/.cargo/bin /mnt/c/go/bin $path)
export path

export KEYTIMEOUT=10
export EDITOR='vim'
export PAGER='less'
export QT_AUTO_SCREEN_SCALE_FACTOR=1

export CARGO_HOME='/home/nuchs/.cargo'
export RUST_SRC_PATH='/home/nuchs/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
