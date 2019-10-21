#!/usr/bin/env bash
set -euo pipefail

install() {
  mkdir -p ~/go/src/github.com/ttakezawa
  git clone git@github.com:ttakezawa/dotfiles.git ~/go/src/github.com/ttakezawa/dotfiles
  ln -nfs ~/go/src/github.com/ttakezawa/dotfiles ~/.dotfiles

  ~/.dotfiles/deploy.sh
}

install
