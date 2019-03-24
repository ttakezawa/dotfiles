#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"
dir="$( builtin cd -P "$( dirname "$SOURCE" )" && builtin pwd )"
cd $dir

if [[ "$(uname -m)" =~ 64 ]]; then
  IS_64=1
fi

if [[ $IS_64 ]]; then
  PROCESSOR=amd64
else
  PROCESSOR=386
fi

if type -P sshrc >/dev/null; then
  # setup sshrc
  mkdir -p .sshrc.d
  cp -p .emacs.d/init-builtin.el ./.sshrc.d/init.el
  cp -p .vimrc .sshrc.d/
  cp -p ./bin/ec2-hosts.sh .sshrc.d/
fi

# install gocomplete
go get -u github.com/posener/complete/gocomplete

