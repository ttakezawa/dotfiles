#!/bin/bash
# shellcheck disable=SC1090
# -*- coding:utf-8; mode:sh; sh-basic-offset:2; sh-indentation:2; -*-

#### basic
# Find REALDIR of this script
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
SOURCE_DIR="$(cd -P "$( dirname "$SOURCE" )" && pwd)"
export PATH="$PATH:$SOURCE_DIR/bin"

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

if type -P mount.vboxsf &>/dev/null; then
  IS_VBOX=1
fi

if [[ ( ! $IS_VBOX ) && $IS_LINUX && $IS_INTERACTIVE_SH ]] && type -P curl &>/dev/null; then
  if $(curl --connect-timeout 0.01 -s http://169.254.169.254/1.0); then
    IS_EC2=1
  fi
fi

# inspect host type
if [[ $IS_EC2 ]]; then
  host_type="ec2"
elif [[ $IS_VBOX ]]; then
  host_type="vbox"
elif [[ $IS_DARWIN ]]; then
  host_type="mac"
elif [[ $IS_LINUX ]]; then
  host_type="linux"
else
  host_type="unknown-type"
fi
# save host_type for .byobu-tmux.conf
echo $host_type > /tmp/${USER}-host_type
export BYOBU_NO_TITLE=1

if [[ $IS_INTERACTIVE_SH ]]; then
  # set window title
  echo -ne "\e]0;${host_type}\a"
fi

# common utility
ignore_warn=''

warn() {
  [[ $ignore_warn ]] && return 0
   echo 1>&2 $*
}

#### load environment resource
[[ $IS_DARWIN ]] && source $SOURCE_DIR/.bashrc.darwin
[[ -r $SOURCE_DIR/.env.sh ]] && source $SOURCE_DIR/.env.sh

#### basic tweaks
export TZ="Asia/Tokyo"
export LANG=ja_JP.UTF-8
# use $HOME/local
export PATH="$HOME/local/bin:$PATH:$HOME/.local/bin"
export MANPATH="$HOME/local/share/man:$MANPATH"
export LD_LIBRARY_PATH="$HOME/local/lib:$LD_LIBRARY_PATH"
# prefer $HOME/bin and $HOME/man
export PATH="$HOME/bin:$PATH"
export MANPATH="$HOME/man:$MANPATH"
IGNOREEOF=3
# ディストリによっては設定されていることがあるので初期化する
PROMPT_COMMAND=''

#### [ history ]
## configure history
HISTFILE=~/.bash_history_x
HISTSIZE=10000
HISTFILESIZE=$HISTSIZE
# NOTE: HISTTIMEFORMATをセットしてタイムスタンプを記録すると、history -w の動作がおかしくなる
HISTTIMEFORMAT='%Y-%m-%d %eT | '
HISTIGNORE="fg*:bg*:history:cd:ps:exit:ls:ls -al:tig:git status:git log:git diff:git dfc:g"

## share history accross sessions
shopt -u histappend
function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND="share_history;$PROMPT_COMMAND"
HISTCONTROL=ignoredups

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

# Set window title as a side effect of $prompt_screen
host_label="\[\e[32m\]${host_type}\[\e[0m\]:\[\e[34m\]$HOSTNAME"

_PROMPT1='\[\e[0;36m\]\t \[\e[34m\]'${host_label}' \[\e[31m\]$(prettify_exit_code)\[\e[33m\]\w\[\e[0m\]'
_PROMPT2="\\n$prompt_screen\$ "
PS1=$_PROMPT1$_PROMPT2

# git prompt
if ! type -t __git_ps1 &>/dev/null; then
  source $SOURCE_DIR/.bash.d/git-prompt.sh
fi
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
# Stop improperly $ escaping
shopt -s direxpand

# git-completion
if [[ -r $SOURCE_DIR/.bash.d/git-completion.bash ]]; then
  source $SOURCE_DIR/.bash.d/git-completion.bash
fi

# gibo-completion
source $SOURCE_DIR/.bash.d/gibo-completion.bash

# completion-ruby-all
if [[ -r $SOURCE_DIR/.bash.d/completion-ruby/completion-ruby-all ]]; then
  source $SOURCE_DIR/.bash.d/completion-ruby/completion-ruby-all
fi

## asdf
if [[ -r $HOME/.asdf/asdf.sh ]]; then
  . $HOME/.asdf/asdf.sh
  . $HOME/.asdf/completions/asdf.bash
fi
if type -t asdf &>/dev/null; then
  asdfi() {
    local lang=${1}
    if [[ ! $lang ]]; then
      lang=$(asdf plugin-list | fzf)
    fi
    if [[ $lang ]]; then
      local versions=$(asdf list-all $lang | fzf --tac -m)
      if [[ $versions ]]; then
        for version in $(echo $versions); do
          asdf install $lang $version
        done
      fi
    fi
  }
fi

#### direnv
if type -P direnv &>/dev/null; then
  eval "$(direnv hook bash)"
fi

#### android
# platform-tools (adb)
if [[ -d "$HOME/Library/Android/sdk/platform-tools" ]]; then
  export PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
fi
source $SOURCE_DIR/.bash.d/android.sh

#### ruby
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
    if ruby --version | grep 'ruby 2' &>/dev/null; then
      cd $(ruby -e 'puts Gem::Specification.find_by_name(ARGV[0]).full_gem_path' -- $gem)
    else
      cd $(ruby -e 'puts Gem.source_index.find_name(ARGV[0]).last.full_gem_path' -- $gem)
    fi
  fi
}

# #### npm
# if ! type -t _npm_completion &>/dev/null; then
#   if type -P npm &>/dev/null; then
#     source <(npm completion)
#   fi
# fi

# #### pip
# if type -P pip &>/dev/null; then
#   eval "$(pip completion --bash 2>/dev/null)"
# fi

#### perl
if [[ -d "$HOME/.plenv/bin" ]]; then
  export PATH="$HOME/.plenv/bin:$PATH"
  eval "$(plenv init -)"
fi

#### awscli
if type -P aws_completer &>/dev/null; then
  complete -C aws_completer aws
fi

#### Emacs
export EDITOR="emacsclient -c -nw --alternate-editor="
alias edit="$EDITOR"
complete -F _fzf_file_completion -o default -o bashdefault edit
alias kill-emacs="emacsclient -e '(kill-emacs)'"
# evm
if [[ -d "$HOME/.evm/bin" ]]; then
  export PATH="$HOME/.evm/bin:$PATH"
  if ! type -P emacsclient &>/dev/null; then
    echo "emacsclient not found." >&2
    # You can shim emacsclient
    # cd $(dirname $(which emacs)); sed 's|bin/emacs|bin/emacsclient|g' emacs > emacsclient; chmod +x emacsclient
  fi
fi

#### Golang
export GOPATH=$HOME/dev
export PATH=$GOPATH/bin:$PATH

## https://github.com/posener/complete
if type -P gocomplete &>/dev/null; then
  complete -C gocomplete go
fi

## goenv
#if [[ -d "$HOME/.goenv/bin" ]]; then
#  export PATH="$HOME/.goenv/bin:$PATH"
#  eval "$(goenv init -)"
#  export GOROOT="$(goenv prefix)"
#fi

#### fzf
if [[ -f ~/.fzf.bash ]]; then
  source ~/.fzf.bash
  export FZF_DEFAULT_OPTS='--bind ctrl-k:kill-line --height 70% --preview "preview {}"'
  export FZF_CTRL_T_COMMAND='fd -L -H -E \.git/ -E dev/pkg/ -E Library/Caches -E Library/Containers -E backups -E Quiver.qvlibrary/ -t d -t f -t l'
  export FZF_ALT_C_COMMAND=' fd -L -H -E \.git/ -E dev/pkg/ -E Library/Caches -E Library/Containers -E backups -E Quiver.qvlibrary/ -t d'

  if ! type -P ghq &>/dev/null; then
    echo "ghq not found." >&2
  fi

  g() {
    # # use tree command
    # local l=$(ghq list --full-path | sed "s|$HOME/||" | fzf --reverse --preview "LANG=C tree -C $HOME/{} -I _tools")

    # use exa command
    local l=$(ghq list --full-path | sed "s|$HOME/||" | fzf --reverse --preview "preview ~/{}")

    [[ -n "$l" ]] && cd "$HOME/$l"
  }

  gg() {
    local item="$*"
    if [[ ! -e "$item" ]]; then
      item="$(fd -H -E '\.git/' | fzf -1 -q "$item" --preview "preview {}")"
      if [[ -z "$item" ]]; then
        return 0
      fi
      echo "selected: $item"
    fi
    if [[ -f "$item" ]]; then
      item="$(dirname $item)"
    fi
    cd "$item"
    echo "goto: $item"
    exa -al
  }
  complete -F _fzf_path_completion -o default -o bashdefault gg

  # Synopsis
  #   e [-c] /path/to/file
  #   e -
  e() {
    # option "-" : The file to edit is read from stdin.
    if [[ "$1" == "-" ]]; then
      TMP="$(mktemp /tmp/stdin-XXX)"
      cat >$TMP
      edit $TMP
      rm $TMP
      return
    fi

    # option "-c" : File search is skipped.
    local skip_file_search
    if [[ "$1" = "-c" ]]; then
       # skip file search
      skip_file_search=1
      shift
    fi

    local arg="$*"
    if [[ -n "$skip_file_search" || -e "$arg" ]]; then
      edit "$arg"
      return
    fi

    local results selected
    IFS=$'\n' results=($(fd -t f -H -E '\.git/' | fzf --print-query -q "$arg" --preview "preview {}"))
    if [[ -n "${results[1]}" ]]; then
      selected="${results[1]}"
    elif [[ -n "${results[0]}" ]]; then
      selected="${results[0]}"
    fi
    if [[ -z "$selected" ]]; then
      return 0
    fi
    echo "selected: $selected"
    edit "$selected"
  }
  complete -F _fzf_path_completion -o default -o bashdefault e

  # Taken from https://github.com/junegunn/fzf/wiki/Examples#git
  unalias l 2>/dev/null
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

  # uniqにするために次のフィルタを追加: perl -ne 'print unless $seen{$_}++'
  __fzf_history__() {
    local output
    output=$(
      builtin fc -lnr -2147483648 | perl -ne 'print unless $seen{$_}++' |
        last_hist=$(HISTTIMEFORMAT='' builtin history 1) perl -p -l0 -e 'BEGIN { getc; $/ = "\n\t"; $HISTCMD = $ENV{last_hist} + 1 } s/^[ *]//; $_ = $HISTCMD - $. . "\t$_"' |
        FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m --read0" $(__fzfcmd) --query "$READLINE_LINE"
    ) || return
    READLINE_LINE=${output#*$'\t'}
    if [ -z "$READLINE_POINT" ]; then
      echo "$READLINE_LINE"
    else
      READLINE_POINT=0x7fffffff
    fi
  }
else
  warn "~/.fzf.bash not found."
fi

# #### fasd
# if type -P fasd &>/dev/null; then
#   eval "$(fasd --init bash-hook bash-ccomp bash-ccomp-install)"
#
#   z() {
#     local dir
#     dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
#   }
# else
#   warn "fasd not found."
# fi

#### hub
if type -P hub &>/dev/null; then
  alias git=hub
  # work around of hub completion bug
  alias __git=hub
fi

if type -P git-switch-trainer &>/dev/null; then
  alias git=git-switch-trainer
fi

#### ag
if ! type -t _ag &>/dev/null; then
  source $SOURCE_DIR/.bash.d/ag.bashcomp.sh
fi

#### ripgrep
if ! type -t _rg &>/dev/null; then
  source $SOURCE_DIR/.bash.d/rg.bash-completion
fi

#### mfacodegen
if [[ -r $SOURCE_DIR/bin/_mfacodegen ]]; then
  source $SOURCE_DIR/bin/_mfacodegen
fi

#### gomi
if type -P gomi &>/dev/null; then
  alias rm=gomi
fi

#### net-tools deprecation (Taken from http://blog.livedoor.jp/sonots/archives/38589335.html )
net_tools_deprecated_message () {
  echo -n 'net-tools is deprecated.'
}
arp () {
  net_tools_deprecated_message
  echo 'Use `ip n`'
}
ifconfig () {
  net_tools_deprecated_message
  echo 'Use `ip a`, `ip link`, `ip -s link`'
}
netstat () {
  net_tools_deprecated_message
  echo 'Use `ss`, `ip route` (for netstat -r), `ip -s link` (for netstat -i), `ip maddr` (for netstat -g)'
}
route () {
  net_tools_deprecated_message
  echo 'Use `ip r`'
}

#### coreutils
alias grep="grep --color=auto"
alias ls="ls --color=tty"
# +e は zless において末尾で自動終了させないための設定
export LESS="-n -R +e"

#### misc tweaks
_dotenv()
{
  local cur prev cword
  _get_comp_words_by_ref -n : cur prev cword
  if [ "${cword}" -eq 1 ]; then
    COMPREPLY=( $(compgen -f -- "${cur}") )
  elif [ "${cword}" -eq 2 ]; then
    COMPREPLY=( $(compgen -c -- "${cur}") )
  else
    COMPREPLY=( $(compgen -f -- "${cur}") )
  fi
}
complete -F _dotenv dotenv

function conv-time () {
  for arg in "$@"; do
    if ( echo "$arg" | $(type -P ggrep grep | head -1) -qsP '^\d+$' ); then
      $(type -P gdate date | head -1) -d "1970-1-1 GMT +$arg secs"
    else
      $(type -P gdate date | head -1) +%s -d "$arg"
    fi
  done
}

function sleep-until {
  local given="$(date -d "$*" +%s)"
  while [[ "$(date +%s)" < "$given" ]]; do
    local left=$(( $(date -d "$*" +%s) - $(date +%s) ))
    echo -ne "\r$(date +"%Y-%m-%d %H:%M:%S %Z") ⏳$(date -d"0+$left sec" +%H:%M:%S)"
    sleep 1
  done
}

alias stracev="strace -f -T -tt -v -x -y -yy"

if type exa &>/dev/null; then
  alias ls=exa
  source <(type _exa | sed -e '1d; s/_exa ()/_exa_alias ()/; s/$1/exa/')
  complete -o filenames -o bashdefault -F _exa_alias ls

  alias ll="exa --time-style long-iso"
  alias la="exa -aghl@ --git --time-style long-iso"
fi

if type -P sshrc &>/dev/null; then
  _completion_loader ssh 2>/dev/null # for bash-completion >= 1.90, bash >= 4.1
  eval $(complete -p ssh | sed 's/ ssh$/ sshrc/')
  export SSHHOME=$SOURCE_DIR
fi

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
