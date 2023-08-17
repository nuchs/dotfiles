#!/bin/bash
#
# Script to setup my dev environment

# === Config {{{1
VERBOSE=0
DRY_RUN=0
SOURCE_DIR="$HOME/sauce"
BIN_DIR="$HOME/bin"
NPM_DIR="$HOME/.npm-packages"

# === Helper Functions {{{1

function help() {
  echo "Set up development environment, make sure all necessary programs are"
  echo "installed and configured correctly"
  echo
  echo "devup.sh [-dhv]"
  echo
  echo "options:"
  echo "d    dry run mode, just print out what would happen"
  echo "h    Print this help message"
  echo "v    Verbose mode, log extra message"
}

function run() {
  CMD=$1
  shift
  if [ $DRY_RUN -eq 1 ]; then
    echo "DRY RUN | $CMD $@"
  else
    $CMD $@
  fi
}

function log() {
  if [[ $VERBOSE > 0 ]]; then
    echo $@
  fi
}

function snap_install() {
  if snap list | awk '{print $1}' | grep "\b$1\b" &> /dev/null; then
    log "Snap $1 is already installed, skipping"
  else
    log "Installing snap $1"
    run sudo snap install $1
  fi
}

function apt_install() {
  if dpkg -s $1 &> /dev/null; then
    log "Apt package $1 is already installed, skipping"
  else
    log "Installing apt package $1"
    run sudo apt -y install $1
  fi
}

function npm_install() {
  if  [ -e "$NPM_DIR/bin/$1" ]; then
    log "NPM executable $1 already exists, skipping"
  else
    log "NPM installing $1"
    run npm install -g $1
  fi
}

function link_binary() {
  if [ -L "$BIN_DIR/$2"  ]; then
    log "Link $BIN_DIR/$2 already exists, skipping"
    return
  fi

  log "Linking binary : $1"
  run ln -s $1 $BIN_DIR/$2
}

function link_config() {
  if [ -L "$HOME/.$1"  ]; then
    log "$HOME/.$1 already exists, skipping"
    return
  fi

  if [ -f "$HOME/.$1" ]; then
    log "$HOME/.$1 already exists, deleting"
    run rm -f "$HOME/.$1"
  fi

  log "Linking config file: $1"
  run ln -s $HOME/etc/$1 $HOME/.$1
}

function make_directory() {
  if [ ! -d "$1" ]; then
    log "Making directory $1"
    run mkdir -p $1
  else
    log "Directory $1 already exists, skipping"
  fi

}

function clone() {
 if [ -d "$SOURCE_DIR/$2" ]; then
   log "Repo has already been cloned, skipping"
 else
   log "Cloning $1 to $2"
   run git clone $1 $2
 fi
}

# === Main Script {{{1

while getopts "hdv" option; do
  case $option in
    h) # display help
      help
      exit;;
    v) # bump up the log level
      VERBOSE=1
      ;;
    d) # dry run mode
      DRY_RUN=1
      VERBOSE=1
      ;;
    \?) # Unrecognised option
      echo "Error: invalid option"
      help
      exit;;
  esac
done

if [[ -f "$HOME/.ssh/id_rsa" && -f "$HOME/.ssh/id_rsa.pub" ]]; then
  log "SSH keys found, good to go"
else 
  echo "SSH keys are not installed, cannot proceed"
  exit 1
fi

# Initial Setup {{{2
make_directory $HOME/bin
make_directory $HOME/work
make_directory $SOURCE_DIR
make_directory $NPM_DIR/bin
make_directory $NPM_DIR/lib

link_config "bashrc"
link_config "gitconfig"
link_config "inputrc"
link_config "profile"
link_config "tmux.conf"
link_config "vimrc"

run touch $HOME/.hushlogin

# Install software {{{2

link_binary "$HOME/etc/devup.sh" "devup"

# Apt Packages {{{3
run sudo apt -y update
run sudo apt -y upgrade

apt_install gpg
apt_install git
apt_install python3
apt_install vim
apt_install sxiv
apt_install httpie
apt_install xsel
apt_install ddgr
apt_install ripgrep
apt_install bat
link_binary "/usr/bin/batcat" "bat"
apt_install most
apt_install duf
apt_install joplin
link_binary "$HOME/.joplin-bin/bin/joplin" "joplin"

# Snaps Packages {{{3
run sudo snap refresh
snap_install "node"
snap_install "jq"
snap_install "procs"

# Install/Update Vim Plugins {{{3
if [ -f "$HOME/.vim/autoload/plug.vim" ]; then
  run vim -c PlugUpdate -c q -c q
else
  run curl -fLo ~/.vim/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  run vim -c PlugInstall -c q -c q
fi

link_binary "$HOME/.vim/plugged/fzf/bin/fzf"

# Setup TMUX plugins {{{3
clone "https://github.com/tmux-plugins/tpm" "$HOME/.tmux/plugins/tpm"

# Build from source {{{3
if which rustc; then
  log "Rust $(rustc -V | awk '{print $2}') already installed, skipping"
else
  log "Installing rust"
  run curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  log "Rust $(rustc -V | awk '{print $2}') installed,"
fi

if [ ! -L "$BIN_DIR/nnn" ]; then
  clone 'git@github.com:jarun/nnn.git' 'nnn'
  run pushd "."
  run cd nnn
  run make
  run popd
  link_binary "$SOURCE_DIR/nnn/nnn" "nnn"
else
  log "NNN is already installed, skipping"
fi

if [ ! -L "$BIN_DIR/zoxide" ]; then
  clone 'git@github.com:ajeetdsouza/zoxide.git' 'zoxide'
  run pushd .
  run cd zoxide
  run cargo build --release
  run ./install.sh
  run popd
  link_binary "$HOME/.local/bin/zoxide" "zoxide"
else
  log "Zoxide is already installed, skipping"
fi

# Npm apps {{{3
npm_install yarn
npm_install livedown


# === Done {{{1
log "Have you installed fonts & gpg keys?"
log "We're cocked, locked and ready to rock"
