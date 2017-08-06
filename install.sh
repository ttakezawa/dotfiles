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

# Install exa manpage
mkdir -p $HOME/local/share/man/man1
curl -L https://raw.githubusercontent.com/ogham/exa/master/contrib/man/exa.1 -o $HOME/local/share/man/man1/exa.1
