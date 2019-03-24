## Setup

```bash
# installation
mkdir -p ~/go/src/github.com/ttakezawa
git clone -p git@github.com:ttakezawa/dotfiles.git ~/go/src/github.com/ttakezawa/dotfiles
ln -nfs ~/go/src/github.com/ttakezawa/dotfiles ~/.dotfiles

~/.dotfiles/install.sh

# misc
git config --global pr-release.template .pr-release.erb
git config --global pr-release.branch.staging development
# ln -s ~/.dotfiles/.pr-release.erb ./
```
