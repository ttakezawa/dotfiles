#!/usr/bin/env bash
set -euo pipefail

SOURCE="${BASH_SOURCE[0]}"
dir="$( builtin cd -P "$( dirname "$SOURCE" )" && builtin pwd )"
cd $dir

append_if_not_exists() {
  if [[ $# -ne 2 ]]; then
    echo "Usage: append_if_not_exists: <new_line> <file>"
    return 1
  fi
  if [[ ! -e "$2" ]]; then
     touch "$2"
  fi
  grep -qxF "$1" "$2" || echo "$1" >> "$2"
}

IS_64=
IS_DAWRIN=
IS_LINUX=

if [[ "$(uname -m)" =~ 64 ]]; then
  IS_64=1
fi

if [[ $IS_64 ]]; then
  PROCESSOR=amd64
else
  PROCESSOR=386
fi

case $OSTYPE in
  darwin*) IS_DARWIN=1 ;;
  linux*)  IS_LINUX=1  ;;
esac

# Linux
if [[ $IS_LINUX ]]; then
  append_if_not_exists 'source ~/.dotfiles/.bashrc' ~/.bashrc
fi

# macOS
if [[ $IS_DARWIN ]]; then
  append_if_not_exists 'source ~/.dotfiles/.bashrc' ~/.bash_profile
fi

# Common
ln -fs  ~/.dotfiles/.ssh_config ~/.ssh/config
ln -fs  ~/.dotfiles/.inputrc ~/
ln -fs  ~/.dotfiles/.gitconfig ~/
ln -fs  ~/.dotfiles/.gitignore_global ~/
ln -fs  ~/.dotfiles/.gitattributes_global ~/
ln -fs  ~/.dotfiles/.tigrc ~/
ln -fs  ~/.dotfiles/.tmux.conf ~/
ln -nfs ~/.dotfiles/.emacs.d ~/
ln -fs  ~/.dotfiles/.vimrc ~/
ln -fs  ~/.dotfiles/.cobra.yaml ~/
ln -fs  ~/.dotfiles/.agignore ~/
ln -fs  ~/.dotfiles/.aspell.conf ~/.dotfiles/.aspell.en.pws ~/
ln -fs  ~/.dotfiles/.gemrc ~/

ln -fs ~/.dotfiles/.byobu-tmux.conf ~/
mkdir -p ~/.byobu
if ! grep -qs byobu-tmux.conf ~/.byobu/.tmux.conf; then
  echo 'source $HOME/.byobu-tmux.conf' >> ~/.byobu/.tmux.conf
fi

mkdir -p ~/.atom
ln -fs ~/.dotfiles/.atom/keymap.cson ~/.atom/

if type -P sshrc >/dev/null; then
  # setup sshrc
  mkdir -p .sshrc.d
  cp -p .emacs.d/init-builtin.el ./.sshrc.d/init.el
  cp -p .vimrc .sshrc.d/
  cp -p ./bin/ec2-hosts.sh .sshrc.d/
  cp -p .inputrc .sshrc.d/
  cp -p ./bin/run-tmux .sshrc.d/
fi

# make login faster
touch ~/.hushlogin

if [[ $IS_DARWIN ]]; then
  ./.macos
fi
