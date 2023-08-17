#!/bin/bash
#
# Script to setup my dev environment

# Packages
#
# sxiv
# vim
# httpie
# xsel
#ddgr
# ripgrep
# bat
# git
# python3
# most
# duf
# gpg
# rust
# joplin

# Source
#
# nnn
# zoxide

# npm
#
# yarn
# livedown


# === Helper Functions {{{1
function snap_install() {
  if [ $(snap list | awk '{print $1}' | grep "\b$1\b") ]; then
    echo "$1 is already installed"
  else
    echo "Installing $1"
    sudo snap install $1
  fi
}

function link_config() {
  if [[ ! -f $HOME/.$1 ]]; then
    ln -s $HOME/etc/$1 $HOME/.$1
  fi
}

# === Main Script {{{1

# Initial Setup
mkdir $HOME/bin
mkdir $HOME/work
mkdir $HOME/sauce

link_config "bashrc"
ln -s

# Install packages from apt
sudo apt update
sudo apt upgrade
# sxiv

# Install snaps
sudo snap refresh
snap_install "node"
snap_install "jq"
snap_install "procs"

# Install from source
