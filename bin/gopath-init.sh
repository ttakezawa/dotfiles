#!/bin/bash

set -euo pipefail

if [[ -d $PWD/.gopath ]]; then
  echo $PWD/.gopath already exists.
  exit 1
fi

if [[ -f $PWD/.envrc ]]; then
  echo $PWD/.envrc already exists.
  exit 1
fi

pkg=$(go list -e -f '{{.ImportPath}}')
pkgdir=$PWD/.gopath/src/$pkg

cat <<'EOF' > $PWD/.envrc
export GOPATH=$PWD/.gopath
PATH_add $GOPATH/bin
EOF

direnv allow

mkdir -p $(dirname $pkgdir)
ln -sfv $PWD $pkgdir
