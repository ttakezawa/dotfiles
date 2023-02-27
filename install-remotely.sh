#!/usr/bin/env bash
set -euo pipefail

install() {
  devpath="$HOME/dev"
  mkdir -p "${devpath}/src/github.com/ttakezawa"
  if [[ ! -d "${devpath}/src/github.com/ttakezawa/dotfiles/.git" ]]; then
    git clone git@github.com:ttakezawa/dotfiles.git "${devpath}/src/github.com/ttakezawa/dotfiles"
  fi
  ln -nfs "${devpath}/src/github.com/ttakezawa/dotfiles" ~/.dotfiles

  ~/.dotfiles/deploy.sh
}

install
