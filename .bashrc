# -*- coding:utf-8; mode:sh; sh-basic-offset:2; sh-indentation:2; -*-

#### basic
# Inspect system environment
[[ $- == *i* ]] && IS_INTERACTIVE_SH=1
if [[ $SHLVL -gt 1 ]]; then
  [[ -n $STY  ]] && IS_SCREEN=1
  [[ -n $TMUX ]] && IS_TMUX=1
fi
case $OSTYPE in
  darwin*) IS_DARWIN=1 ;;
  linux*)  IS_LINUX=1  ;;
esac

if [[ $IS_INTERACTIVE_SH ]]; then
  # disable stty bindings
  stty quit undef
  stty susp undef
  stty erase undef
  stty werase undef
  stty stop undef
  stty start undef
fi

# Find REALDIR of this script
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
SOURCE_DIR="$(cd -P "$( dirname "$SOURCE" )" && pwd)"
export PATH="$PATH:$SOURCE_DIR/bin"

#### load environment resource
[[ $IS_DARWIN ]] && source $SOURCE_DIR/.bashrc.darwin

#### basic tweaks
export LANG=ja_JP.UTF-8
# use $HOME/local
export PATH="$HOME/local/bin:$PATH"
export MANPATH="$HOME/local/share/man:$MANPATH"
export LD_LIBRARY_PATH="$HOME/local/lib:$LD_LIBRARY_PATH"
# prefer $HOME/bin and $HOME/man
export PATH="$HOME/bin:$PATH"
export MANPATH="$HOME/man:$MANPATH"

#### [ history ]
# share accross sessions
function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND="share_history;$PROMPT_COMMAND"
shopt -u histappend
# customize history
export HISTFILE=~/.bash_history
export HISTSIZE=100000
export HISTFILESIZE=1000000
export HISTCONTROL=ignoredups
export HISTTIMEFORMAT='%Y-%m-%d %T '

#### [ prompt ]
export PROMPT_DIRTRIM=6
if [[ $IS_SCREEN ]]; then
  # screen
  PROMPT_SCREEN='\[\033k\033\\\]'
fi
_PROMPT1='\[\e[0;36m\]\t \[\e[34m\]\h \[\e[31m\]${?##0}\[\e[33m\]\w\[\e[0m\]'
_PROMPT2="\\n$PROMPT_SCREEN\$ "
export PS1=$_PROMPT1$_PROMPT2

# git prompt
source $SOURCE_DIR/.bash.d/git-prompt.sh
if [[ "$(type -t __git_ps1)" ]]; then
  export GIT_PS1_SHOWDIRTYSTATE=true
  export GIT_PS1_SHOWUPSTREAM="verbose"
  export GIT_PS1_SHOWSTASHSTATE=true
  export GIT_PS1_SHOWUNTRACKEDFILES=true
  export GIT_PS1_SHOWCOLORHINTS=true
  export GIT_PS1_DESCRIBE_STYLE=describe
  PROMPT_COMMAND="__git_ps1 '$_PROMPT1' '$_PROMPT2';$PROMPT_COMMAND"
fi

#### completions
source $SOURCE_DIR/.bash.d/git-completion.bash

#### awscli
if type -P aws_completer >/dev/null; then
  complete -C aws_completer aws
fi

#### emacs cask
export PATH="$HOME/.cask/bin:$PATH"

#### peco
if type -P peco >/dev/null; then
  _replace_by_history() {
    # 頻度が多く、新しいコマンド順にリストする
    local l=$(HISTTIMEFORMAT='' history | sort -rk2 | uniq -cf1 | sort -rn | sed -r 's/^\s*[0-9]+\s+[0-9]+\s*//' | peco --query "$READLINE_LINE")
    READLINE_LINE="$l"
    READLINE_POINT=${#l}
  }
  bind -x '"\C-r": _replace_by_history'
fi

#### misc tweaks
export GREP_OPTIONS='--color=auto'
alias ls="ls --color=tty"
