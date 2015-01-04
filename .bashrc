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
IGNOREEOF=3

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
HISTFILE=~/.bash_history
HISTSIZE=100000
HISTFILESIZE=1000000
HISTCONTROL=ignoredups
HISTTIMEFORMAT='%Y-%m-%d %T '

#### [ prompt ]
PROMPT_DIRTRIM=6
if [[ $IS_SCREEN ]]; then
  # screen
  prompt_screen='\[\033k\033\\\]'
fi
_PROMPT1='\[\e[0;36m\]\t \[\e[34m\]\h \[\e[31m\]${?##0}\[\e[33m\]\w\[\e[0m\]'
_PROMPT2="\\n$prompt_screen\$ "
PS1=$_PROMPT1$_PROMPT2

# git prompt
source $SOURCE_DIR/.bash.d/git-prompt.sh
if [[ "$(type -t __git_ps1)" ]]; then
  GIT_PS1_SHOWDIRTYSTATE=true
  GIT_PS1_SHOWUPSTREAM="verbose"
  GIT_PS1_SHOWSTASHSTATE=true
  GIT_PS1_SHOWUNTRACKEDFILES=true
  GIT_PS1_SHOWCOLORHINTS=true
  GIT_PS1_DESCRIBE_STYLE=describe
  PROMPT_COMMAND="__git_ps1 '$_PROMPT1' '$_PROMPT2';$PROMPT_COMMAND"
fi

#### completions
# git-competion
source $SOURCE_DIR/.bash.d/git-completion.bash

# completion-ruby-all
if [[ -r $SOURCE_DIR/.bash.d/completion-ruby/completion-ruby-all ]]; then
  source $SOURCE_DIR/.bash.d/completion-ruby/completion-ruby-all
fi

#### ruby
cdgem () {
  local bundle_gems=$(bundle list | grep '\*' | sed -e 's/^ *\* *//g')
  if [[ -n "$bundle_gems" ]]; then
    gem=$(echo "$bundle_gems" | peco | cut -d \  -f 1)
    [[ -z "$gem" ]] && return 1
    cd $(bundle show $gem)
  else
    gem=$(gem list | peco | cut -d \  -f 1)
    [[ -z "$gem" ]] && return 1
    if ruby --version | grep 'ruby 2' >/dev/null; then
      cd $(ruby -e 'puts Gem::Specification.find_by_name(ARGV[0]).full_gem_path' -- $gem)
    else
      cd $(ruby -e 'puts Gem.source_index.find_name(ARGV[0]).last.full_gem_path' -- $gem)
    fi
  fi
}

#### awscli
if type -P aws_completer >/dev/null; then
  complete -C aws_completer aws
fi

#### emacs cask
export PATH="$HOME/.cask/bin:$PATH"

#### golang
export GOPATH=$HOME/dev
export PATH=$GOPATH/bin:$PATH
if type -P go >/dev/null && [[ -r $(go env GOROOT)/misc/bash/go ]]; then
  # completion
  source $(go env GOROOT)/misc/bash/go
fi

#### peco
if (( ${BASH_VERSINFO[0]} >= 4 )) && type -P peco >/dev/null; then
  # READLINE_* は Bash4 で実装されている

  _replace_by_history() {
    local -a args
    if [[ -n "$READLINE_LINE" ]]; then
      args=(${args[@]} "--query $READLINE_LINE")
    fi

    # 頻度が多く、新しいコマンドの順にリストする
    #  ASCII文字([\t -~])で構成されたコマンドのみ対象とする
    local l=$(HISTTIMEFORMAT='' history | grep -E '^[\t -~]+$' \
                | sort -rk2 | uniq -cf1 | sort -rn \
                | sed -r 's/^\s*[0-9]+\s+[0-9]+\s*//' | peco ${args[@]})
    READLINE_LINE="$l"
    READLINE_POINT=${#l}
  }
  bind -x '"\C-r": _replace_by_history'

  g() {
    local l=$(ghq list | peco)
    [[ -n "$l" ]] && cd $(ghq root)/$l
  }
fi

#### ag
if [[ -r $SOURCE_DIR/.bash.d/ag.bashcomp.sh ]]; then
  source $SOURCE_DIR/.bash.d/ag.bashcomp.sh
fi

#### misc tweaks
alias grep="grep --color=auto"
alias ls="ls --color=tty"

function conv-time () {
  for arg in "$@"; do
    if ( echo "$arg" | $(type -p ggrep grep | head -1) -qsP '^\d+$' ); then
      $(type -p gdate date | head -1) -d "1970-1-1 GMT +$arg secs"
    else
      $(type -p gdate date | head -1) +%s -d "$arg"
    fi
  done
}

#### command time
# trap ... DEBUG を上書きして実装している
# また、PROMPT_COMMANDをいじる都合上、最後に設定するようにする
function timer_start {
  timer=${timer:-$SECONDS}
}
function timer_stop {
  timer_show=$(($SECONDS - $timer))
  unset timer
  if (( $timer_show > 3 )); then
    echo -e '\e[1;31m'"TOO SLOW: $timer_show secs."'\e[m'
  fi
}
trap 'timer_start' DEBUG
PROMPT_COMMAND=$(echo -n "timer_stop; $PROMPT_COMMAND; unset timer" | sed -e 's/;;/;/')
