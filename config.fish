set fish_greeting

export TZ='Asia/Tokyo'
export LANG='ja_JP.UTF-8'

# paths
fish_add_path $HOME/.dotfiles/bin $HOME/local/bin $HOME/.local/bin $HOME/bin
! set -q MANPATH; and set MANPATH ''; set -gx MANPATH "$HOME/local/share/man" "$HOME/man" $MANPATH;
! set -q LD_LIBRARY_PATH; and set -gx LD_LIBRARY_PATH ''; set -gx LD_LIBRARY_PATH "$HOME/local/lib" $LD_LIBRARY_PATH;

# History
function history-merge --on-event fish_preexec
    history --save
    history --merge
end

# Homebrew
if test -x /opt/homebrew/bin/brew && test -z "$HOMEBREW_PREFIX"
    /opt/homebrew/bin/brew shellenv fish | source
    # Homebrewをprependする
    ! set -q MANPATH; and set MANPATH ''; set -gx MANPATH "/opt/homebrew/share/man" $MANPATH;
end

# coreutils
abbr -a grep "ggrep --color=auto"
abbr -a ls "ls --color=tty"

# +e は zless において末尾で自動終了させないときに使う
export LESS="-n -R -M"
export LESSCHARSET=utf-8

# Golang
export GOPATH="$HOME/dev"
fish_add_path "$GOPATH/bin"

# ghq
function g
    set l (ghq list --full-path | sed "s|$HOME/||" | fzf --reverse --preview "preview ~/{}")
    test -n "$l"; and cd "$HOME/$l"
end

# macrm https://github.com/satosystems/macrm
if type -q macrm
    abbr -a rm macrm
else
    echo "macrm not found."
    echo "Please install. brew install satosystems/tap/macrm"
end

# eza
if type -q eza
    abbr -a ls eza
end

# mise
if type -q mise
    # PATH for IDE
    fish_add_path "$HOME/.local/share/mise/shims"
    mise activate fish | source
end

# bat --list-themes
if type -q bat
    export BAT_THEME='Dracula'
    export PAGER='bat'
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
end

# adb
fish_add_path "$HOME/Library/Android/sdk/platform-tools"

# starship
if type -q starship
    starship init fish | source
end
