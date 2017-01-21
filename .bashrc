#!/bin/bash
# shellcheck disable=SC1090
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

function prettify_exit_code {
  # Taken from https://github.com/bric3/nice-exit-code/blob/master/nice-exit-code.plugin.zsh
  local exit_status="$?"
  # nothing to do here
  [[ -z $exit_status || $exit_status == 0 ]] && return;

  local msg;

  case $exit_status in
    # is this a signal name (error code = signal + 128) ?
    129)  msg=SIGHUP  ;;
    130)  msg=SIGINT  ;;
    131)  msg=SIGQUIT ;;
    132)  msg=SIGILL  ;;
    134)  msg=SIGABRT ;;
    136)  msg=SIGFPE  ;;
    137)  msg=SIGKILL ;;
    139)  msg=SIGSEGV ;;
    141)  msg=SIGPIPE ;;
    143)  msg=SIGTERM ;;

    # usual exit codes
    1)    msg=MISCERROR     ;; # Miscellaneous errors, such as "divide by zero"
    2)    msg=BUILTINMISUSE ;; # misuse of shell builtins (pretty rare)
    126)  msg=CCANNOTINVOKE ;; # cannot invoke requested command (ex : source script_with_syntax_error)
    127)  msg=CNOTFOUND     ;; # command not found (ex : source script_not_existing)
    255)  msg=FATAL         ;;

    # assuming we are on an x86 system here
    # this MIGHT get annoying since those are in a range of exit codes
    # programs sometimes use.... we'll see.
    19)  msg=SIGSTOP ;;
    20)  msg=SIGTSTP ;;
    21)  msg=SIGTTIN ;;
    22)  msg=SIGTTOU ;;
  esac

  if [[ -n $msg ]]; then
    echo -n "${exit_status}(${msg}) "
  else
    echo -n "${exit_status} "
  fi
}

_PROMPT1='\[\e[0;36m\]\t \[\e[34m\]\h \[\e[31m\]$(prettify_exit_code)\[\e[33m\]\w\[\e[0m\]'
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
# rbenv
if [[ -d "$HOME/.rbenv/bin" ]]; then
  export PATH="$HOME/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi

# change the current directory to a rubygem directory
cdgem () {
  local bundle_gems="$(bundle list | grep '\*' | sed -e 's/^ *\* *//g')"
  if [[ -n "$bundle_gems" ]]; then
    gem=$(echo "$bundle_gems" | fzf --reverse | cut -d \  -f 1)
    [[ -z "$gem" ]] && return 1
    cd $(bundle show $gem)
  else
    gem=$(gem list | fzf --reverse | cut -d \  -f 1)
    [[ -z "$gem" ]] && return 1
    if ruby --version | grep 'ruby 2' >/dev/null; then
      cd $(ruby -e 'puts Gem::Specification.find_by_name(ARGV[0]).full_gem_path' -- $gem)
    else
      cd $(ruby -e 'puts Gem.source_index.find_name(ARGV[0]).last.full_gem_path' -- $gem)
    fi
  fi
}

#### nodejs
# nvm
if [[ -s $HOME/.nvm/nvm.sh ]]; then
  source $HOME/.nvm/nvm.sh
  [[ -s $NVM_DIR/bash_completion ]] && . $NVM_DIR/bash_completion
fi

# npm
if type -P npm >/dev/null; then
  . <(npm completion)
fi

#### perl
if [[ -d "$HOME/.plenv/bin" ]]; then
  export PATH="$HOME/.plenv/bin:$PATH"
  eval "$(plenv init -)"
fi

#### awscli
if type -P aws_completer >/dev/null; then
  complete -C aws_completer aws
fi

#### emacs cask
export PATH="$HOME/.cask/bin:$PATH"

#### golang
export GOPATH=$HOME/dev
export PATH=$PATH:$GOPATH/bin
if [[ -f $SOURCE_DIR/.bash.d/go-pkg-complete.bash.inc ]]; then
  # completion
  source $SOURCE_DIR/.bash.d/go-pkg-complete.bash.inc
fi

#### fzf
if [[ -f ~/.fzf.bash ]]; then
  source ~/.fzf.bash
  export FZF_DEFAULT_OPTS='--bind ctrl-k:kill-line'

  g() {
    local l=$(ghq list | fzf --reverse)
    [[ -n "$l" ]] && cd $(ghq root)/$l
  }

  # Taken from https://github.com/junegunn/fzf/wiki/Examples#git
  unalias l
  function l() {
    git log --graph --color=always \
        --format="%C(auto)%h%d %C(green)%an %C(reset)%s %C(black)%C(bold)%cr" "$@" |
    fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
        --bind "ctrl-m:execute:
                  (grep -o '[a-f0-9]\{7\}' | head -1 |
                  xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                  {}
FZF-EOF"
  }

  # Remove duplicates from history selection
  #   Taken from https://github.com/junegunn/fzf/issues/808 and key-bindings.bash
  # Requires nauniq
  #   mkdir -p ~/bin && curl https://raw.githubusercontent.com/perlancar/perl-App-nauniq/master/bin/nauniq | sed 's?#!perl?#!/usr/bin/env perl?' > ~/bin/nauniq && chmod +x ~/bin/nauniq
  __fzf_history__() (
    local line
    shopt -u nocaseglob nocasematch
    countskip="$(echo $(($(wc -l $HISTFILE | grep -E '^[0-9]+' -o) / 2)) | wc -c)"
    countskip="$(( countskip + 1 ))"
    line=$(
      HISTTIMEFORMAT= history |
      tac |
      nauniq --skip-chars="$countskip" |
      tac |
      FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS +s --tac --no-reverse -n2..,.. --tiebreak=index --toggle-sort=ctrl-r $FZF_CTRL_R_OPTS +m" $(__fzfcmd) |
      command grep '^ *[0-9]') &&
      if [[ $- =~ H ]]; then
        sed 's/^ *\([0-9]*\)\** .*/!\1/' <<< "$line"
      else
        sed 's/^ *\([0-9]*\)\** *//' <<< "$line"
      fi
  )
fi

#### fasd
eval "$(fasd --init bash-hook bash-ccomp bash-ccomp-install)"

z() {
  local dir
  dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

#### ag
if [[ -r $SOURCE_DIR/.bash.d/ag.bashcomp.sh ]]; then
  source $SOURCE_DIR/.bash.d/ag.bashcomp.sh
fi

#### coreutils
alias grep="grep --color=auto"
alias ls="ls --color=tty"
# +e は zless において末尾で自動終了させないための設定
export LESS="-n -R +e"

#### misc tweaks
function conv-time () {
  for arg in "$@"; do
    if ( echo "$arg" | $(type -P ggrep grep | head -1) -qsP '^\d+$' ); then
      $(type -P gdate date | head -1) -d "1970-1-1 GMT +$arg secs"
    else
      $(type -P gdate date | head -1) +%s -d "$arg"
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
