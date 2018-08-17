## Setup

```bash
mkdir -p ~/dev/src/github.com/ttakezawa
git clone git@github.com:ttakezawa/dotfiles.git ~/dev/src/github.com/ttakezawa/dotfiles
ln -s ~/dev/src/github.com/ttakezawa/dotfiles ~/.dotfiles

# Mac OS X
echo 'source $HOME/.dotfiles/.bashrc' >> ~/.bash_profile

# Linux
echo 'source ~/.dotfiles/.bashrc' >> ~/.bashrc

# Common
ln -s ~/.dotfiles/.ssh_config ~/.ssh/config
ln -s ~/.dotfiles/.inputrc ~/
ln -s ~/.dotfiles/.gitconfig ~/
ln -s ~/.dotfiles/.gitignore_global ~/
ln -s ~/.dotfiles/.gitattributes_global ~/
ln -s ~/.dotfiles/.byobu-tmux.conf ~/
mkdir -p ~/.byobu && echo 'source $HOME/.byobu-tmux.conf' >> .byobu/.tmux.conf
ln -s ~/.dotfiles/.tmux.conf ~/
ln -s ~/.dotfiles/.emacs.d ~/
ln -s ~/.dotfiles/.vimrc ~/
ln -s ~/.dotfiles/.cobra.yaml ~/
ln -s ~/.dotfiles/.agignore ~/
ln -s ~/.dotfiles/.aspell.conf ~/.dotfiles/.aspell.en.pws ~/
ln -s ~/.dotfiles/.gemrc ~/

mkdir -p ~/.atom
ln -s ~/.dotfiles/.atom/keymap.cson ~/.atom/
```
