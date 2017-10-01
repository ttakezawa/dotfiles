#!/bin/bash

target=$1

target=$(readlink -f $target)

dir=$(readlink -f $(git rev-parse --show-cdup))
cd $dir

target=$(realpath $target --relative-base=$dir)

if [[ -f .gometalinter.conf ]]; then
  gometalinter --config .gometalinter.conf -s vendor $(dirname $target) | sed -r "s|^(.+)\$|${dir}/\1|"
  echo ${dir}/${target}:99999:1:error: dummy
fi
