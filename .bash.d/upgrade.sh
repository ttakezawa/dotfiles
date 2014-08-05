#!/bin/bash
# -*- coding:utf-8; mode:sh; sh-basic-offset:2; sh-indentation:2; -*-

SOURCE="${BASH_SOURCE[0]}"
dir="$( builtin cd -P "$( dirname "$SOURCE" )" && builtin pwd )"
cd $dir

curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash > $dir/git-completion.bash
curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh > $dir/git-prompt.sh

( # update completion-ruby
  git submodule update --init completion-ruby &&
  cd completion-ruby &&
  git checkout master &&
  git pull
)

curl https://raw.githubusercontent.com/ggreer/the_silver_searcher/master/ag.bashcomp.sh > $dir/ag.bashcomp.sh
sed -i 's/have ag &&//' $dir/ag.bashcomp.sh
