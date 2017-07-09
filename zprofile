export MYETC="$HOME/etc"
export MYWORK="$HOME/work"
export MYBIN="$HOME/bin"

typeset -U path
path=( $MYBIN ~/.cargo/bin ~/.local/bin $path)
export path

export EDITOR='nvim'
export PAGER='less'
export QT_AUTO_SCREEN_SCALE_FACTOR=1

export CARGO_HOME='/home/nuchs/.cargo'
export RUST_SRC_PATH='/home/nuchs/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
