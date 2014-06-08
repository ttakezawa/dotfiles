#!/bin/bash
SOURCE="${BASH_SOURCE[0]}"
dir="$( builtin cd -P "$( dirname "$SOURCE" )" && builtin pwd )"
cd $dir

curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash > $dir/git-completion.bash
curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh > $dir/git-prompt.sh
