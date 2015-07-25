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
GHQ_VERSION=v0.6
echo Install ghq $GHQ_VERSION
if [[ "$OSTYPE" =~ linux ]]; then
  curl -L https://github.com/motemen/ghq/releases/download/${GHQ_VERSION}/ghq_linux_${PROCESSOR}.tar.gz \
    | tar xz -C $dir/bin --strip=1 --wildcards '*/ghq' --no-same-owner --no-same-permissions
elif [[ "$OSTYPE" =~ darwin ]]; then
  curl -L https://github.com/motemen/ghq/releases/download/${GHQ_VERSION}/ghq_darwin_${PROCESSOR}.zip \
    | bsdtar -xzf - -C $dir/bin --strip=1 '*/ghq'
fi

# Install peco from https://github.com/peco/peco/releases
PECO_VERSION=v0.3.3
echo Install peco $PECO_VERSION
if [[ "$OSTYPE" =~ linux ]]; then
  curl -L https://github.com/peco/peco/releases/download/${PECO_VERSION}/peco_linux_${PROCESSOR}.tar.gz \
    | tar xz -C $dir/bin --strip=1 --wildcards '*/peco' --no-same-owner --no-same-permissions
elif [[ "$OSTYPE" =~ darwin ]]; then
  curl -L https://github.com/peco/peco/releases/download/${PECO_VERSION}/peco_darwin_${PROCESSOR}.zip \
    | bsdtar -xzf - -C $dir/bin --strip=1 '*/peco'
fi
