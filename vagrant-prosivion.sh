#!/bin/bash

# SYNOPSIS
# config.vm.provision "shell", path: "https://raw.githubusercontent.com/ttakezawa/dotfiles/master/vagrant-prosivion.sh"

# configuration
PREFIX="$HOME/dev/src/github.com"
REPO_USER="ttakezawa"
REPO_NAME="dotfiles"

# main
REPO=$REPO_USER/$REPO_NAME

case $(id -un) in
  root)
    echo Become vagrant user
    sudo -u vagrant -i $0
    exit
    ;;
  vagrant)
    echo Rrunning as vagrant user
    ;;
  *)
    echo Unknown user: $(id -un)
    exit 1
esac

set -xe

if ! type -P git >/dev/null; then
  sudo apt-get update
  sudo apt-get install git -y
fi

if [[ ! -d $PREFIX/$REPO/.git ]]; then
  mkdir -p $PREFIX/${REPO_USER}
  git clone https://github.com/${REPO} ${PREFIX}/${REPO}
fi

ln -fs $PREFIX/$REPO ~/.dotfiles

if [[ ! -f ~/.bashrc.orig ]]; then
  cp ~/.bashrc ~/.bashrc.orig
fi
if ! grep -q .dotfiles/.bashrc .bashrc; then
  echo 'source ~/.dotfiles/.bashrc' >> ~/.bashrc
fi

ln -fs ~/.dotfiles/.inputrc ~/
ln -fs ~/.dotfiles/.gitconfig ~/
ln -fs ~/.dotfiles/.gitignore_global ~/
ln -fs ~/.dotfiles/.gitattributes_global ~/
ln -fs ~/.dotfiles/.tmux.conf ~/
ln -fs ~/.dotfiles/.emacs.d ~/
ln -fs ~/.dotfiles/.vimrc ~/
ln -fs ~/.dotfiles/.agignore ~/
ln -fs ~/.dotfiles/.aspell.conf ~/.dotfiles/.aspell.en.pws ~/
ln -fs ~/.dotfiles/.gemrc ~/

if ! type -P bsdtar >/dev/null; then
  sudo apt-get install bsdtar -y
fi

~/.dotfiles/install.sh

echo "Provisioning done"
