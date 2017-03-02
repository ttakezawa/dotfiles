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

# Install ghq from https://github.com/motemen/ghq/releases
GHQ_VERSION=v0.7.2
echo Install ghq $GHQ_VERSION
if [[ "$OSTYPE" =~ linux ]]; then
  curl -L https://github.com/motemen/ghq/releases/download/${GHQ_VERSION}/ghq_linux_${PROCESSOR}.zip \
    | bsdtar -xzf - -C $dir/bin --strip=0 './ghq'
elif [[ "$OSTYPE" =~ darwin ]]; then
  curl -L https://github.com/motemen/ghq/releases/download/${GHQ_VERSION}/ghq_darwin_${PROCESSOR}.zip \
    | bsdtar -xzf - -C $dir/bin --strip=0 './ghq'
fi
chmod +x $dir/bin/ghq
