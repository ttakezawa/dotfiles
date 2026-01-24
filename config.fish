set fish_greeting

export TZ='Asia/Tokyo'
export LANG='ja_JP.UTF-8'

# cache dir
set cachedir /private/tmp/fish-cache
mkdir -p $cachedir

# paths
fish_add_path --move --path $HOME/.dotfiles/bin $HOME/local/bin $HOME/.local/bin $HOME/bin
! set -q MANPATH; and set MANPATH ''; set -gx MANPATH "$HOME/local/share/man" "$HOME/man" $MANPATH;
! set -q LD_LIBRARY_PATH; and set -gx LD_LIBRARY_PATH ''; set -gx LD_LIBRARY_PATH "$HOME/local/lib" $LD_LIBRARY_PATH;

# History
function history-merge --on-event fish_preexec
    history --save
    history --merge
end

# Ctrl-D での誤終了防止:
# - 入力中は 1 文字削除
# - 何も入力していない状態で N 回押すとシェルを終了
# - 新しいプロンプトごとにカウンタをリセット
set -g __cd_exit_required_presses 3  # Ctrl-D 何回で終了するか

# Ctrl-D 押下時の処理関数（N 回で終了）
function __cd_n_times_exit --description 'Delete char or exit after N Ctrl-D presses'
    set -l N $__cd_exit_required_presses

    # 入力中: 通常どおり 1 文字削除してカウンタをリセット
    if commandline | string collect > /dev/null
        commandline -f delete-char
        set -g __cd_press_count 0
        return
    end

    # 空行: Ctrl-D 連打回数をカウント
    if not set -q __cd_press_count
        set -g __cd_press_count 0
    end
    set -g __cd_press_count (math $__cd_press_count + 1)

    if test $__cd_press_count -ge $N
        exit 0
    else
        set -l remain (math $N - $__cd_press_count)
        printf 'Press Ctrl-D %d more time(s) to exit\n\n' $remain
        commandline -f repaint
    end
end

# 新しいプロンプトが出たらカウンタをクリア
function __cd_reset_counter --on-event fish_prompt
    set -e __cd_press_count
end

# Ctrl-D のバインドを差し替え（ユーザー定義は既定より優先される）
bind \cd __cd_n_times_exit
bind -M insert \cd __cd_n_times_exit
bind -M default \cd __cd_n_times_exit

# Homebrew
if not type -q brew && test -x /opt/homebrew/bin/brew
    if not test -s "$cachedir/brew_shellenv" # brewの補完をキャッシュする
        /opt/homebrew/bin/brew shellenv fish > "$cachedir/brew_shellenv"
    end
    source "$cachedir/brew_shellenv"
    # Homebrewをprependする
    ! set -q MANPATH; and set MANPATH ''; set -gx MANPATH "/opt/homebrew/share/man" $MANPATH;
end

if type -q brew
  # prepend Homebrew PATH
  fish_add_path --move --path /opt/homebrew/bin
end

# +e は zless において末尾で自動終了させないときに使う
export LESS="-n -R -M"
export LESSCHARSET=utf-8

# Golang
export GOPATH="$HOME/dev"
fish_add_path --move --path "$GOPATH/bin"

# ghq
function g
    set l (ghq list --full-path | sed "s|$HOME/||" | fzf --reverse --ansi --preview "preview ~/{}")
    test -n "$l" && cd "$HOME/$l"
end

# gwq
if type -q gwq
    function gw
        set l (gwq get)
        test -n "$l" && cd "$l"
    end
end

function gg
    set l "$(git rev-parse --show-cdup)"
    test -n "$l" && cd "$l"
end

# configure `fisher install decors/fish-ghq`
set -g GHQ_SELECTOR_OPTS --reverse --ansi --preview "preview {}"

### asdf
export ASDF_GOLANG_MOD_VERSION_ENABLED=true

# setup asdf
# if test -z $ASDF_DATA_DIR
#     set _asdf_shims "$HOME/.asdf/shims"
# else
#     set _asdf_shims "$ASDF_DATA_DIR/shims"
# end
# if not contains $_asdf_shims $PATH
#     set -gx --prepend PATH $_asdf_shims
# end
# set --erase _asdf_shims

# completions for asdf
if type -q asdf && not test -e "$HOME/.config/fish/completions/asdf.fish"
    asdf completion fish > "$HOME/.config/fish/completions/asdf.fish"
end

# bat --list-themes
if type -q bat
    export BAT_THEME='Dracula'
    export PAGER='bat'
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
end

# adb
fish_add_path --move --path "$HOME/Library/Android/sdk/platform-tools"

# JetBrains
if test -d "$HOME/Library/Application Support/JetBrains/Toolbox/scripts"
    fish_add_path --move --path "$HOME/Library/Application Support/JetBrains/Toolbox/scripts"
end

# mysql-client
if test -x /opt/homebrew/opt/mysql-client/bin/mysql
    fish_add_path --move --path /opt/homebrew/opt/mysql-client/bin
end

# libpq
if test -d /opt/homebrew/opt/libpq/bin
    fish_add_path --move --path /opt/homebrew/opt/libpq/bin
end

# Browser Use: Disable telemetry
export ANONYMIZED_TELEMETRY=false

if status is-interactive
    # coreutils
    if type -q brew
        # /opt/homebrew/opt/coreutils/bin にあるファイルのうちgから始まるものをすべてgなしでfishのabbrに登録する
        for f in /opt/homebrew/opt/coreutils/bin/g*
            set filename (string replace '/opt/homebrew/opt/coreutils/bin/' '' -- $f)
            set cmd (string replace -r '^g' '' -- $filename)
            abbr -a $cmd $filename
        end
    end

    # grep options
    if type -q ggrep
        abbr -a grep "ggrep --color=auto"
    else
        abbr -a grep "grep --color=auto"
    end

    # eza
    if type -q eza
        abbr -a ls eza
    end

    # macrm https://github.com/satosystems/macrm
    if type -q macrm
        abbr -a rm macrm
    else if test (uname -s) = "Darwin" # Macならエラーを出力
        echo "macrm not found."
        echo "Please install. brew install satosystems/tap/macrm"
    end

    function go-install
        go install golang.org/dl/go$argv[1]@latest
        and go$argv[1] download
        and ln -fs $GOPATH/bin/go$argv[1] $GOPATH/bin/go
    end

    # starship
    type -q starship && starship init fish | source

    # ent
    type -q ent && ent completion fish | source

    # atlas
    if type -q atlas
        if not test -e "$cachedir/atlas_completion" # atlasの補完をキャッシュする
            atlas completion fish > "$cachedir/atlas_completion"
        end
        source "$cachedir/atlas_completion"
    end

    # git wt
    if type -q git-wt
        git wt --init fish | source
    end

    # git wt shortcut
    function wt
        git wt (git wt | tail -n +2 | fzf --height 40% --reverse | awk '{print $(NF-1)}')
    end

    # aws
    # Enable AWS CLI autocompletion: github.com/aws/aws-cli/issues/1079
    complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'

    # awslocal
    complete -c awslocal -w aws

    # shell_gpt
    export DEFAULT_MODEL=gpt-4o-mini
end
