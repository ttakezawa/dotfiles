# -*- coding:utf-8; mode:sh; sh-basic-offset:2; sh-indentation:2; -*-

#### [ base ]
export PATH="$HOME/bin:$PATH"
export MANPATH="$HOME/man:$MANPATH"

# Find REALDIR of this script
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
export MYHOME="$( builtin cd -P "$( dirname "$SOURCE" )" && builtin pwd )"

if [[ $- == *i* ]]; then
  IS_INTERACTIVE_SH=1
else
  IS_INTERACTIVE_SH=
fi

if [[ $IS_INTERACTIVE_SH ]]; then
  # disable stty bindings
  stty quit undef
  stty susp undef
  stty erase undef
  stty werase undef
  stty stop undef
  stty start undef
fi

#### [ history ]
#  share
function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND="share_history;$PROMPT_COMMAND"
shopt -u histappend
#  customize
export HISTFILE=~/.bash_history
export HISTSIZE=100000
export HISTFILESIZE=1000000
export HISTCONTROL=ignoredups
export HISTTIMEFORMAT='%Y-%m-%d %T '

#### [ prompt ]
export PROMPT_DIRTRIM=6
# if [[ -n $STY ]] && [[ $SHLVL -gt 1 ]]; then
#   # screen
#   PROMPT_SCREEN='\[\033k\033\\\]'
# fi
_PROMPT1='\[\e[0;36m\]\t \[\e[34m\]\h \[\e[31m\]${?##0}\[\e[33m\]\w\[\e[0m\]'
_PROMPT2="\\n$PROMPT_SCREEN\$ "
export PS1=$_PROMPT1$_PROMPT2

# git prompt
source $MYHOME/.bash.d/git-prompt.sh
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
source $MYHOME/.bash.d/git-completion.bash


export PATH="$HOME/.cask/bin:$PATH"
