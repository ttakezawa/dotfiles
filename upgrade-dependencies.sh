#!/bin/bash -ex
# -*- coding:utf-8; mode:sh; sh-basic-offset:2; sh-indentation:2; -*-

SOURCE="${BASH_SOURCE[0]}"
dir="$( builtin cd -P "$( dirname "$SOURCE" )" && builtin pwd )"
cd $dir

# .bash.d/git-completion.bash
curl -L https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash > .bash.d/git-completion.bash

# .bash.d/git-prompt.sh
curl -L https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh > .bash.d/git-prompt.sh

# .bash.d/exa-completions.bash
curl -L https://raw.githubusercontent.com/ogham/exa/master/contrib/completions.bash -o .bash.d/exa-completions.bash

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

# .bash.d/gibo-completion.bash
curl -L https://raw.githubusercontent.com/simonwhitaker/gibo/master/shell-completions/gibo-completion.bash -o .bash.d/gibo-completion.bash

# .bash.d/android.sh
curl -L https://raw.githubusercontent.com/mbrubeck/android-completion/master/android -o .bash.d/android.sh

# bin/diff-highlight
# SEE: https://github.com/git/git/blob/master/contrib/diff-highlight/Makefile
echo '#!/usr/bin/perl' > bin/diff-highlight
curl -L https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/DiffHighlight.pm >> bin/diff-highlight
curl -L https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight.perl >> bin/diff-highlight
chmod +x bin/diff-highlight

# bin/diff-so-fancy
curl -L https://raw.githubusercontent.com/so-fancy/diff-so-fancy/master/third_party/build_fatpack/diff-so-fancy -o bin/diff-so-fancy
chmod +x bin/diff-so-fancy

# update gocomplete
go get -v -u github.com/posener/complete/gocomplete

# update goimports
go get -v -u golang.org/x/tools/cmd/goimports

# update gotests
go get -v -u github.com/cweill/gotests/gotests

# update asdf
if type asdf >/dev/null; then
  asdf update || :
  asdf plugin-update --all
fi

# update ghq
go get -v -u github.com/motemen/ghq
