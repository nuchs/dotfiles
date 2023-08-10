export MYETC="$HOME/etc"
export MYWORK="$HOME/work"
export MYWINWORK="$HOME/winwork"
export MYBIN="$HOME/bin"
export MYDOCS="$HOME/docs"
export MYDOWNLOADS="$HOME/downloads"

typeset -U path
path=( $MYBIN ~/.local/bin /usr/local/go/bin /home/nuchs/work/etcd/bin $path)
export path

export KEYTIMEOUT=10
export EDITOR='vim'
export PAGER='less'

cd $HOME
