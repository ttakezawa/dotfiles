## Setup

```bash
git clone git@github.com:ttakezawa/dotfiles.git ~/.dotfiles

# Mac OS X
echo 'source $HOME/.dotfiles/.bashrc' >> ~/.bash_profile

# Linux
echo 'source $HOME/.dotfiles/.bashrc' >> ~/.bashrc

# Common
ln -s ~/.dotfiles/.inputrc ~/
ln -s ~/.dotfiles/.gitconfig ~/
ln -s ~/.dotfiles/.gitignore_global ~/
ln -s ~/.dotfiles/.gitattributes_global ~/
ln -s ~/.dotfiles/.tmux.conf ~/
ln -s ~/.dotfiles/.emacs.d ~/
ln -s ~/.dotfiles/.peco ~/
ln -s ~/.dotfiles/.agignore ~/
ln -s ~/.dotfiles/.aspell.conf ~/.dotfiles/.aspell.en.pws ~/
```
