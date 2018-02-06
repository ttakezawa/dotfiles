#!/bin/bash

readlink() {
  if [[ -z $1 ]]; then
    echo -n '.'
  else
    echo -n $(command readlink -f $1)
  fi
}

target=$(readlink $1)

dir=$(readlink $(git rev-parse --show-cdup))
cd $dir

target=$(realpath $target --relative-base=$dir)
absdir=$(realpath $dir)

conf=''

if [[ -f .gometalinter.conf ]]; then
  conf=.gometalinter.conf
elif [[ -f .gometalinter.json ]]; then
  conf=.gometalinter.json
fi

if [[ -f $conf ]]; then
  if [[ "$target" = "." ]]; then
    gometalinter --config $conf -s vendor ./...
  else
    gometalinter --config $conf -s vendor $(dirname $target) | sed -r "s|^(.+)\$|${absdir}/\1|"
    echo ${absdir}/${target}:99999:1:error: dummy
  fi
fi
