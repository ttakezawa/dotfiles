set fish_greeting

export TZ='Asia/Tokyo'
export LANG='ja_JP.UTF-8'
export PAGER='bat'

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
alias grep="grep --color=auto"
alias ls="ls --color=tty"
# +e は zless において末尾で自動終了させないための設定
export LESS="-n -R -M +e"
export LESSCHARSET=utf-8

# Golang
export GOPATH="$HOME/dev"
fish_add_path "$GOPATH/bin"

# macrm https://github.com/satosystems/macrm
if type -q macrm
    alias rm=macrm
else
    echo "macrm not found."
    echo "Please install. brew install satosystems/tap/macrm"
end

# eza
if type -q eza
    alias ls=eza
end

# mise
if type -q mise
    # PATH for IDE
    fish_add_path "$HOME/.local/share/mise/shims"
    mise activate fish | source
end

# bat --list-themes
export BAT_THEME='Dracula'

# adb
fish_add_path "$HOME/Library/Android/sdk/platform-tools"

# starship
if type -q starship
    starship init fish | source
end
