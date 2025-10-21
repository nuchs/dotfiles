#!/bin/bash
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
export EDITOR='nvim'
export PAGER='less'
export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8

export COLORTERM=truecolor

export MYBIN="$HOME/.local/bin"
export MYETC="$HOME/etc"
export MYTEMPLATES="$HOME/etc"
export MYDOC="$HOME/docs"
export MYMEMORIES="$MYDOC/memories"
export MYARCH="$HOME/archive"

export GOPATH="$HOME/go"
export PATH="$MYBIN:$GOPATH/bin:$PATH"

. "$HOME/.bashrc"
