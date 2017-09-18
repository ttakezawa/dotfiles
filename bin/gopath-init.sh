#!/bin/bash

set -euo pipefail

pkg=$(go list -e -f '{{.ImportPath}}')
# gopath=$PWD/.gopath
gopath=$HOME/dev/.gopath/$(echo $pkg | tr / _)

if [[ -d $gopath ]]; then
  echo $gopath already exists.
  exit 1
fi

if [[ -f $PWD/.envrc ]]; then
  echo $PWD/.envrc already exists.
  exit 1
fi

pkgdir=$gopath/src/$pkg

# Init GOPATH Directory
mkdir -p $(dirname $pkgdir)
mv $PWD $pkgdir
ln -sfv $pkgdir $PWD

# Init .envrc
cat <<EOF > $pkgdir/.envrc
export GOPATH=${gopath/$HOME/\$HOME}
export PATH=\$GOPATH/bin:\$PATH
EOF

direnv allow $pkgdir/.envrc
direnv allow $PWD/.envrc
