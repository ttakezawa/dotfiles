#!/bin/bash

if git remote show upstream -n | grep -q github; then
  echo "upstream already exists."
  exit 1
fi

github_user=$(ssh -T git@github.com 2>&1 | sed -r 's/^.*Hi (.*)!.*$/\1/')

if echo $github_user | grep -qP ' '; then
  echo "Could not fetch github user name: $github_user"
  exit 1
fi

fork=$(git remote show origin -n | grep -oP "Fetch URL: .*" | sed -r "s|^.*URL: (.*[/:])[^/]*/([^/]*)$|\1${github_user}/\2|")

echo "Set remote rename origin upstream"
git remote rename origin upstream

echo "Set remote $github_user: $fork"
git remote add $github_user $fork
