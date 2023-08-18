#!/bin/bash
#
# Script to setup my dev environment

# === Config {{{1
VERBOSITY=0
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

function write_log() {
  LEVEL=$1; shift
  PREFIX=$1; shift
  MSG="$(date +'%Y-%m-%d %T.%3N') | $PREFIX | $@"

  if [ $LEVEL -le $VERBOSITY ]; then
    echo $MSG
  fi

  echo $MSG >> /tmp/devup.log
}

function debug() {
  write_log 1 "DBG" $@
}

function log() {
  write_log 0 "INF" $@
}

function error() {
  write_log 0 "ERR" $@
}

function run() {
  CMD=$1
  shift
  if [ $DRY_RUN -eq 1 ]; then
    debug "DRY RUN | $CMD $@"
  else
    $CMD $@
  fi
}


function snap_install() {
  if snap list | awk '{print $1}' | grep "\b$1\b" &> /dev/null; then
    debug "Snap $1 is already installed, skipping"
  else
    debug "Installing snap $1"
    run sudo snap install $1
  fi
}

function apt_install() {
  if dpkg -s $1 &> /dev/null; then
    debug "Apt package $1 is already installed, skipping"
  else
    debug "Installing apt package $1"
    run sudo apt -y install $1
  fi
}

function npm_install() {
  if  [ -e "$NPM_DIR/bin/$1" ]; then
    debug "NPM executable $1 already exists, skipping"
  else
    debug "NPM installing $1"
    run npm install -g $1
  fi
}

function link_binary() {
  if [ -L "$BIN_DIR/$2"  ]; then
    debug "Link $BIN_DIR/$2 already exists, skipping"
    return
  fi

  debug "Linking binary : $1"
  run ln -s $1 $BIN_DIR/$2
}

function link_config() {
  if [ -L "$HOME/.$1"  ]; then
    debug "$HOME/.$1 already exists, skipping"
    return
  fi

  if [ -f "$HOME/.$1" ]; then
    debug "$HOME/.$1 already exists, deleting"
    run rm -f "$HOME/.$1"
  fi

  debug "Linking config file: $1"
  run ln -s $HOME/etc/$1 $HOME/.$1
}

function make_directory() {
  if [ ! -d "$1" ]; then
    debug "Making directory $1"
    run mkdir -p $1
  else
    debug "Directory $1 already exists, skipping"
  fi

}

function clone() {
 if [ -d "$SOURCE_DIR/$2" ]; then
   debug "Repo has already been cloned, skipping"
 else
   debug "Cloning $1 to $2"
   run git clone $1 $2
 fi
}

# === Main Script {{{1

# Start up {{{2
while getopts "hdv" option; do
  case $option in
    h) # display help
      help
      exit;;
    v) # bump up the log level
      VERBOSITY=1
      ;;
    d) # dry run mode
      DRY_RUN=1
      VERBOSITY=1
      ;;
    \?) # Unrecognised option
      error "Invalid option"
      help
      exit 1;;
  esac
done

log "Synchronising development environment"

if [[ -f "$HOME/.ssh/id_rsa" && -f "$HOME/.ssh/id_rsa.pub" ]]; then
  debug "SSH keys found, good to go"
else 
  error "SSH keys are not installed, cannot proceed"
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
apt_install vim-gtk3
apt_install sxiv
apt_install httpie
apt_install xsel
apt_install ddgr
apt_install ripgrep
apt_install bat
link_binary "/usr/bin/batcat" "bat"
apt_install duf
apt_install chafa
apt_install w3m

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

link_binary "$HOME/.vim/plugged/fzf/bin/fzf" "fzf"

# Setup TMUX plugins {{{3
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
  debug "Installing TMUX plugin manager"
  clone "https://github.com/tmux-plugins/tpm" "$HOME/.tmux/plugins/tpm"
  run ~/.tmux/plugins/tpm/bin/install_plugins
else
  debug "TMUX plugin manager already installed, updating plugins"
  run ~/.tmux/plugins/tpm/bin/update_plugins all
fi

# Build from source {{{3
if command -v rustc &> /dev/null; then
  debug "Rust $(rustc -V | awk '{print $2}') already installed, skipping"
else
  debug "Installing rust"
  run curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  debug "Rust $(rustc -V | awk '{print $2}') installed,"
fi

if [ ! -L "$BIN_DIR/nnn" ]; then
  clone 'git@github.com:jarun/nnn.git' 'nnn'
  run pushd "."
  run cd nnn
  run make
  run popd
  link_binary "$SOURCE_DIR/nnn/nnn" "nnn"
  run "$SOURCE_DIR/nnn/plugins/getplugs"
else
  debug "NNN is already installed, skipping"
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
  debug "Zoxide is already installed, skipping"
fi

# Npm apps {{{3
npm_install yarn
npm_install livedown
npm_install joplin

# === Done {{{1
debug "Have you installed fonts & gpg keys?"
log "We're cocked, locked and ready to rock"
