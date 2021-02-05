#!/usr/bin/env bash
set -euo pipefail

install() {
  mkdir -p ~/go/src/github.com/ttakezawa
  if [[ ! -d ~/go/src/github.com/ttakezawa/dotfiles/.git ]]; then
    git clone git@github.com:ttakezawa/dotfiles.git ~/go/src/github.com/ttakezawa/dotfiles
  fi
  ln -nfs ~/go/src/github.com/ttakezawa/dotfiles ~/.dotfiles

  ~/.dotfiles/deploy.sh
}

install
