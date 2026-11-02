# Homebrew の vendor_conf.d/direnv.fish を同名ファイルで置き換える。
# vendor 版は Cellar 配下のバージョン入りパスを呼ぶため、brew upgrade 後に
# 起動済みの fish が旧パスを呼び続けて "Unknown command" になる。
#
# direnv hook は呼び出したパスをそのままフックに埋め込む。
# バージョンを含まない /opt/homebrew/bin/direnv (最新版を指す symlink) で呼べば、
# upgrade をまたいでもパスが無効にならない。
# PATH 上の direnv は mise の shim を指すので、絶対パスで呼ぶ。
if test -x /opt/homebrew/bin/direnv
    /opt/homebrew/bin/direnv hook fish | source
end
