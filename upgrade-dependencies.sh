#!/bin/bash -ex
# -*- coding:utf-8; mode:sh; sh-basic-offset:2; sh-indentation:2; -*-

SOURCE="${BASH_SOURCE[0]}"
dir="$( builtin cd -P "$( dirname "$SOURCE" )" && builtin pwd )"
cd $dir

# .bash.d/git-completion.bash
curl -L https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash > .bash.d/git-completion.bash

# .bash.d/git-prompt.sh
curl -L https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh > .bash.d/git-prompt.sh

# .bash.d/completion-ruby/
(
  cd .bash.d &&
  git submodule update --init completion-ruby &&
  cd completion-ruby &&
  git checkout master &&
  git pull
)

# .bash.d/ag.bashcomp.sh
curl -L https://raw.githubusercontent.com/ggreer/the_silver_searcher/master/ag.bashcomp.sh > .bash.d/ag.bashcomp.sh
sed -i 's/have ag &&//' .bash.d/ag.bashcomp.sh

# bin/diff-highlight
curl -L https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight > bin/diff-highlight
chmod +x bin/diff-highlight
