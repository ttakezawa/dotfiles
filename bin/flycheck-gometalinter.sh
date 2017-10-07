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

if [[ -f .gometalinter.conf ]]; then
  if [[ "$target" = "." ]]; then
    gometalinter --config=.gometalinter.conf -s vendor ./...
  else
    gometalinter --config .gometalinter.conf -s vendor $(dirname $target) | sed -r "s|^(.+)\$|${absdir}/\1|"
    echo ${absdir}/${target}:99999:1:error: dummy
  fi
fi
