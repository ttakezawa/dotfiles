;; -*- coding:utf-8 -*-

;;;; Configure builtin features
(load-file (locate-user-emacs-file "init-builtin.el"))

;;;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;; font for darwin (Mac OS X)
(when (and (eq system-type 'darwin)
           (window-system))
  ;; swap 'Command' for 'option'
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  ;; set transparency
  (add-to-list 'default-frame-alist '(alpha . 85))
  ;; font Ricty Diminished Discord
  (create-fontset-from-ascii-font "Ricty Diminished Discord:size=15:weight=normal:slant=normal" nil "rictydiminisheddiscord")
  (set-fontset-font "fontset-rictydiminisheddiscord"
                    'unicode
                    (font-spec :family "Ricty Diminished Discord" :size 15)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-rictydiminisheddiscord")))

;;;; {el-get}
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))

;;;; {el-get-lock}
(el-get-bundle tarao/el-get-lock)
(el-get-lock)

;;;; {exec-path-from-shell}
(el-get-bundle exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(let ((envs '("PATH" "MANPATH" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;;;; {save-framegeometry}
(el-get-bundle gist:218fd80d9390845bf9dcad727237d4af:save-framegeometry
  :features save-framegeometry)
(add-to-list 'recentf-exclude "framegeometry")

;;;; {recentf-ext}
(el-get-bundle recentf-ext)

;;;; custom-theme
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; (load-theme 'tkzw t)

;;;; {moe-theme}
(el-get-bundle moe-theme)
(require 'moe-theme)
(moe-dark)
;; Customize colors
(let ((comment "#af5f00")) ;; Assign comment to moe-dark pallete orange-5
  ;; (set-face-background 'default "black") ;; Use true black in background.
  (set-face-background 'default "unspecified-bg") ;; Use terminal background.
  (set-face-attribute font-lock-comment-face           nil :foreground comment :slant 'normal)
  (set-face-attribute font-lock-comment-delimiter-face nil :foreground comment :slant 'normal))

;;;; {highlight-symbol}
(el-get-bundle highlight-symbol)
(setq highlight-symbol-idle-delay 0.1)
(add-to-list 'mode-line-cleaner-alist '(highlight-symbol-mode . "")) ;; Hide from mode-line
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;;;; {which-key}
(el-get-bundle which-key)
(setq which-key-lighter "")
(setq which-key-idle-delay 0.5)
(which-key-setup-side-window-bottom)
(which-key-mode 1)

;;;; {scratch-log}
(el-get-bundle scratch-log)
(setq takezawa/sl-dir (expand-file-name (locate-user-emacs-file (format-time-string "backups/%Y_%m" (current-time)))))
(unless (file-exists-p takezawa/sl-dir) (make-directory takezawa/sl-dir))
(setq sl-scratch-log-file         (concat takezawa/sl-dir "/scratch-log"))
(setq sl-prev-scratch-string-file (concat takezawa/sl-dir "/scratch-log-prev"))
(setq sl-timer-interval 3)
(require 'scratch-log)

;; ;;;; {auto-save-buffers-enhanced}
;; (el-get-bundle auto-save-buffers-enhanced)
;; (setq auto-save-buffers-enhanced-interval 30.0)
;; (auto-save-buffers-enhanced t)

;;;; {backup-each-save}
(el-get-bundle backup-each-save)
(setq backup-each-save-mirror-location
      (expand-file-name (format-time-string "backups/%Y_%m" (current-time)) user-emacs-directory))
(setq backup-each-save-time-format "%Y%m%d_%H%M%S")
(add-hook 'after-save-hook 'backup-each-save)

;;;; {dumb-jump}
(el-get-bundle jacktasia/dumb-jump :depends (f s dash popup))
(global-set-key (kbd "M-.") 'dumb-jump-go)
(global-set-key (kbd "C-c j") 'dumb-jump-go)
(global-set-key (kbd "M-*") 'dumb-jump-back)

(defun takezawa/dump-jump-go-prompt-at-point ()
  (interactive)
  (dumb-jump-go nil nil (read-from-minibuffer "Jump to: " (thing-at-point 'symbol))))
(global-set-key (kbd "M-,") 'takezawa/dump-jump-go-prompt-at-point)

;;;; {etags-table}
(el-get-bundle emacswiki:etags-table)
(require 'etags-table)
(setq etags-table-search-up-depth 10)

;;;; {helm}
(el-get-bundle helm)
(require 'helm-multi-match) ;; ファイルリスト(candidates-file)でskip matchできるようにする
(add-to-list 'mode-line-cleaner-alist '(helm-mode . "")) ;; Hide from mode-line
(helm-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)
;; (global-set-key (kbd "M-.") 'helm-etags-select) ;; etags
;; (global-set-key (kbd "M-*") 'pop-tag-mark) ;; jump back
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "C-q") 'helm-execute-persistent-action) ;; C-qでチラ見

;; macOSのときは locate の代わりに mdfind を使う
(when (eq system-type 'darwin)
  (setq helm-locate-fuzzy-match nil)
  (setq helm-locate-command "mdfind -name %s %s"))

;; ;; TAGS絞込のとき、helmバッファの見た目通りにマッチさせる
;; (setq helm-etags-match-part-only nil)

;; C-M-iでアクション選択
(define-key helm-map (kbd "C-M-i") 'helm-select-action)
;; find-fileのときC-iで選択
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;; set column size of helm buffer list
(setq helm-buffer-max-length 50)

;;;; {helm-ls-git}
(el-get-bundle helm-ls-git)
(global-set-key (kbd "C-x G") 'helm-ls-git-ls)

;;;; {projectile-mode}
(el-get-bundle projectile)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(projectile-global-mode)

;; Taken from http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access
(defun projectile-root-child-of (dir &optional list)
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (--first
      (if (and
           (string-equal (file-remote-p it) (file-remote-p dir))
           (string-match-p (expand-file-name it) (expand-file-name dir)))
          dir)
      (or list project-root-regexps (list))))))
(defvar project-root-regexps ()
  "List of regexps to match against when projectile is searching
  for project root directories.")
;; rbenv以下のgemがprojectルートになるように設定
(add-to-list 'project-root-regexps "~/\.rbenv/versions/[^/]+/lib/ruby/gems/[^/]+/gems/[^/]+/?$")
;; rbenv以下のrubyがprojectルートになるように設定
(add-to-list 'project-root-regexps "~/\.rbenv/versions/[^/]+/?$")

(add-to-list 'projectile-project-root-files-functions 'projectile-root-child-of)

;; flycheckの各checkerでプロジェクトルート/node_modules/.binを参照させるようにする
(add-hook 'flycheck-mode-hook
          (lambda ()
            (when (projectile-project-p)
              (let ((path (concat (projectile-project-root) "node_modules/.bin")))
                (when (file-directory-p path)
                  (let ((cmd (concat path "/eslint")))
                    (when (file-exists-p cmd) (setq flycheck-javascript-eslint-executable cmd)))
                  )))))

;;;; {helm-projectile}
(el-get-bundle helm-projectile)
(global-set-key (kbd "C-c C-p g") 'helm-projectile-rg)
(define-key projectile-mode-map (kbd "C-c C-p g") 'helm-projectile-rg)

;; Configure helm-for-files
(require 'helm-projectile)
(setq helm-for-files-preferred-list
      '(;; helm-source-buffers-list
        helm-source-ls-git-status
        ;; helm-source-projectile-files-list
        helm-source-recentf
        ;; helm-source-bookmarks
        ;; helm-source-file-cache
        ;; helm-source-files-in-current-dir
        helm-source-locate))
(global-set-key (kbd "C-x f") 'helm-for-files)

(global-set-key (kbd "C-x r") 'helm-projectile)

;;;; {helm-swoop}
(el-get-bundle helm-swoop)
;; デフォルトでhelm-swoopで周辺3行も表示する つまり M-3 M-x helm-swoop と同じ
(defun takezawa/helm-swoop ()
  (interactive)
  (if current-prefix-arg
      (helm-swoop)
    (let ((current-prefix-arg '(3)))
      (helm-swoop))))
(global-set-key (kbd "C-c C-s") 'takezawa/helm-swoop)

;;;; {helm-ag}
(el-get-bundle helm-ag)
(setq helm-ag-insert-at-point 'symbol)
;; C-c C-e: Switch to edit mode
;; C-u打つのが面倒なので、常にC-uの挙動(ディレクトリ選択を求める)にする
(defun takezawa/helm-do-ag-dir ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (helm-do-ag)))
(global-set-key (kbd "C-c g") 'takezawa/helm-do-ag-dir)

;; {helm-rg}
(el-get-bundle cosmicexplorer/helm-rg :name helm-rg :depends (dash helm))
(global-set-key (kbd "C-c g") 'helm-rg)

;;;; {helm-ghq}
(el-get-bundle helm-ghq)
(global-set-key (kbd "C-x g") 'helm-ghq)

;; define takezawa/helm-for-files with helm-ls-git
(defvar takezawa/helm-source-home-filelist
  `((name . "Home FileList")
    (candidates-file ,(expand-file-name "home.filelist" user-emacs-directory) t)
    (action . ,(helm-actions-from-type-file))))

(defvar takezawa/helm-source-system-filelist
  `((name . "System FileList")
    (candidates-file ,(expand-file-name "system.filelist" user-emacs-directory) t)
    (action . ,(helm-actions-from-type-file))))

(require 'helm-ls-git)
(defun takezawa/helm-for-files ()
  "Preconfigured `helm' for opening files.
Run all sources defined in `takezawa/helm-for-files-preferred-list'."
  (interactive)
  (require 'helm-projectile)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (unless helm-source-ls-git
    (setq helm-source-ls-git
          (helm-make-source "Git files" 'helm-ls-git-source
            :fuzzy-match helm-ls-git-fuzzy-match)))
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :buffer "*takezawa/helm-for-files*"
          :sources '(helm-source-buffers-list
                     helm-source-recentf
                     ;; helm-source-bookmarks
                     ;; helm-source-file-cache
                     helm-source-files-in-current-dir
                     ;; helm-source-locate
                     ;; helm-source-ls-git
                     helm-source-projectile-files-list
                     takezawa/helm-source-home-filelist
                     takezawa/helm-source-system-filelist))))
;; 最近使っていないのでコメントアウト
;; (global-set-key (kbd "C-x f") 'takezawa/helm-for-files)

;;;; {elscreen}
(el-get-bundle elscreen)
(when (require 'elscreen nil t)
  (setq elscreen-prefix-key (kbd "C-q"))
  (setq elscreen-display-screen-number nil)
  (elscreen-start)

  ;; for ediff-mode
  (defun elscreen-show-display-tab ()
    (interactive)
    (setq elscreen-display-tab nil)
    (elscreen-notify-screen-modification 'force))
  (defun elscreen-hide-display-tab ()
    (interactive)
    (setq elscreen-display-tab t)
    (elscreen-notify-screen-modification 'force))
  (add-hook 'ediff-before-setup-hook 'elscreen-show-display-tab)
  (add-hook 'ediff-quit-hook 'elscreen-hide-display-tab))

;;;; {direnv}
(el-get-bundle wbolster/emacs-direnv :name direnv :depends (dash with-editor))
(require 'direnv)
(direnv-mode)

;;;; {visual-regexp-steroids}
(el-get-bundle visual-regexp-steroids)
(global-set-key (kbd "C-M-%") 'vr/query-replace)
(global-set-key (kbd "C-M-s") 'vr/isearch-forward)
(global-set-key (kbd "C-M-r") 'vr/isearch-backward)

;;;; {flycheck}
;; requires
;;  * gem install rubocop
;;  * npm install -g jsonlint
(el-get-bundle flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; flycheck disable specific modes
(setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc html-tidy))
(setq flycheck-display-errors-delay 0.1)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)) ;; new-lineは頻度が多すぎて重いので除外
(setq flycheck-idle-change-delay 20.0)
(global-set-key (kbd "C-c l") 'flycheck-list-errors)
(global-set-key (kbd "C-c e") 'flycheck-explain-error-at-point)
(custom-set-faces
 '(flycheck-error ((t (:background "red4" :weight bold))))
 '(flycheck-warning ((t (:background "color-58" :weight bold))))
 '(flycheck-info ((t (:foreground "white" :background "darkgreen")))))

;;;; {flycheck-tip}
(el-get-bundle flycheck-tip)
(require 'flycheck-tip)
(global-set-key (kbd "M-g n") 'error-tip-cycle-dwim)
(global-set-key (kbd "M-g p") 'error-tip-cycle-dwim-reverse)

;; Enable eslint in web-mode
;; * npm install -g eslint
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;;; {git-gutter}
(el-get-bundle git-gutter)
(global-git-gutter-mode 1)
(add-to-list 'mode-line-cleaner-alist '(git-gutter-mode . "")) ;; Hide from mode-line
(setq git-gutter:diff-option "-w")

;;;; {git-modes}
;; gitattributes-mode, gitconfig-mode, gitignore-mode
(el-get-bundle git-modes)
(add-to-list 'auto-mode-alist '("/\\.gitignore_global\\'" . gitignore-mode))
(add-hook 'gitconfig-mode-hook 'intent-tabs-mode) ; TODO: これでもなぜかタブでインデントさせてくれない

;;;; 絵文字
;; Test characters
;; 01234567890123456789
;; ※①・▽☆□α■→×
;; Emoji:🍎🚗🔫🚀
;; (el-get-bundle emojify)
;; (emojify-set-emoji-styles '(unicode))
;; (add-hook 'after-init-hook #'global-emojify-mode)

;;;; {auto-complete}
(el-get-bundle auto-complete)
(ac-config-default)
(add-to-list 'ac-dictionary-directories (expand-file-name "ac-dict" user-emacs-directory))
(setq ac-use-menu-map t)
(setq ac-ignore-case t)
(setq ac-auto-start t)

(add-to-list 'ac-modes 'makefile-mode)        ;; Enable auto-complete-mode
(add-to-list 'ac-modes 'makefile-gmake-mode)  ;; Enable auto-complete-mode

;;;; {ac-helm}
(el-get-bundle ac-helm)
(global-set-key (kbd "M-/") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "M-/") 'ac-complete-with-helm)

;;;; {yasnippet}
(defun my/downcase-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (downcase first-char) rest-str))))
(el-get-bundle yasnippet)
(yas-global-mode 1)

;;;; {yasnippet-snippets}
(el-get-bundle yasnippet-snippets)

;;;; {helm-c-yasnippet}
(el-get-bundle helm-c-yasnippet)
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;;;; {crontab-mode}
(el-get-bundle crontab-mode)
(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\(_\\|\\.\\)" . crontab-mode))

;;;; {yaml-mode}
(el-get-bundle yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\." . yaml-mode))
(add-to-list 'auto-mode-alist '("user-data\\(\\.j2\\)?$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc$" . yaml-mode))

;;;; {go-mode}
;; ##### Golang environment
;; ### Install godef for godef-jump, gocode for go-eldoc, godoc for godoc-at-point, gogetdoc for godoc-at-point
;; $ go get -v -u github.com/rogpeppe/godef github.com/nsf/gocode golang.org/x/tools/cmd/godoc github.com/zmb3/gogetdoc golang.org/x/tools/cmd/guru
;; $ gocode set autobuild true; gocode set unimported-packages true; gocode set propose-builtins true
;; ### To use Flycheck default checkers: http://www.flycheck.org/en/latest/languages.html#go
;; $ go get -v -u github.com/mdempsky/unconvert github.com/golang/lint/golint github.com/kisielk/errcheck
;; ### Install gometalinter and linters
;; $ go get -v -u -f github.com/alecthomas/gometalinter
;; $ gometalinter -d -i -u -f
;; ### gorepl-mode
;; $ go get -v -u -f github.com/motemen/gore
;; ### Update .goimportsignore
;; $ go get -v -u go get golang.org/x/tools/cmd/goimports github.com/pwaller/goimports-update-ignore
;; $ goimports-update-ignore -max-depth 20
;; crontab: 0 3 * * * bash -lc '(goimports-update-ignore -max-depth 20) 2>&1 | gawk "{ print strftime(\"\%Y/\%m/\%d \%H:\%M:\%S\"), \$0; fflush() }"' >>$HOME/.crontab.log 2>&1

(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)
(el-get-bundle go-eldoc)
(el-get-bundle go-test)
(el-get-bundle manute/gorepl-mode :depends (f s hydra))
(with-eval-after-load 'go-mode
  (require 'go-autocomplete)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq go-packages-function 'go-packages-go-list)
  (setq godoc-at-point-function 'godoc-gogetdoc)
  ;;(define-key go-mode-map (kbd "M-.") 'godef-jump)
  ;;(define-key go-mode-map (kbd "C-x 4 M-.") 'godef-jump-other-window)
  (define-key go-mode-map (kbd "M-.") 'go-guru-definition)
  (define-key go-mode-map (kbd "C-x 4 M-.") 'go-guru-definition-other-window)
  (define-key go-mode-map (kbd "C-c d") 'godoc-at-point)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  ;(add-hook 'go-mode-hook #'gorepl-mode)
  (add-hook 'go-mode-hook
            '(lambda ()
               (setq ac-sources '(ac-source-go))
               (setq-local tab-width 4)
               (setq-local comment-auto-fill-only-comments t)
               (set-fill-column 140)
               (auto-fill-mode 1))))

(add-hook 'go-mode-hook
          '(lambda ()
             (unless (getenv "GOROOT")
               ;; goenvを使っている場合にGOROOTが認識されずgodefなどが使えなくなってしまうので、GOROOTを再設定することでworkaroundする
               (setenv "GOROOT" (substring (shell-command-to-string "go env GOROOT") 0 -1)))))

;; highlight err[0-9]* and Err([A-Z]\S+)?
(font-lock-add-keywords
 'go-mode
 '(("\\b\\(err[0-9]*\\)\\b" 1 '((:foreground "#ff4b4b") (:weight bold)) t)
   ("\\b\\(Err\\([A-Z]\\w+\\)?\\)\\b" 1 '((:foreground "#ff4b4b") (:weight bold)) t)))

(setq flycheck-go-vet-shadow 'strict) ;; flycheck go-vet use "-shadowstrict" option

;;;; {flycheck-gometalinter}
(el-get-bundle flycheck-gometalinter)
(with-eval-after-load 'flycheck-gometalinter
  ;; flycheckデフォルトのcheckerのうち最後の go-unconvert よりも後に gometalinter を実行させる
  (flycheck-add-next-checker 'go-unconvert '(warning . gometalinter))
  (add-hook 'go-mode-hook
            '(lambda ()
               (flycheck-select-checker 'go-gofmt))))

;; たくさん実行すると重いのでできるだけ絞る。flycheckで用意されているものややたら遅いものはまず除外する
(setq flycheck-gometalinter-disable-all t)
(setq flycheck-gometalinter-deadline "10s")
(setq flycheck-gometalinter-enable-linters
      '(
        ;; "aligncheck" ;; slow
        ;; "deadcode"   ;; slow
        "dupl"
        ;; "errcheck" ;; flycheck slow
        "gosec"
        "goconst"
        "gocyclo"
        ;; "gofmt" ;; flycheck
        ;; "goimports"
        ;; "golint" ;; flycheck
        "gosimple"
        ;; "gotype" ;; vendorが考慮されずにimportエラーが起きてしまうので除外 E.g. could not import github.com/foo/bar/baz (can't find import: github.com/foo/bar/baz)
        ;; "ineffassign"
        "interfacer" ;; slow
        ;; "lll"
        "misspell"
        "staticcheck"
        ;; "structcheck" ;; slow
        ;; "unconvert" ;; flycheck slow
        ;; "unused"
        ;; "varcheck" ;; slow
        ;; "vet" ;; flycheck
        ;; "vetshadow" ;; flycheck
        ))

(flycheck-define-checker go-flycheck-gometalinter
  "A Golang checker using flycheck-gometalinter.sh"
  :command ("flycheck-gometalinter.sh" source-original)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (optional column) ":error: " (message) line-end)
   (warning line-start (file-name) ":" line ":" (optional column) ":warning: " (message) line-end))
  :modes go-mode
  :next-checkers ((warning . go-golint)
                  ;; Fall back, if go-golint doesn't exist
                  (warning . go-vet)
                  ;; Fall back, if go-vet doesn't exist
                  (warning . go-build) (warning . go-test)
                  (warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-megacheck)))

(flycheck-add-next-checker 'go-gofmt 'go-flycheck-gometalinter)

(add-to-list 'flycheck-checkers 'go-flycheck-gometalinter)

;;;; {enh-ruby-mode}
(el-get-bundle enh-ruby-mode)
(add-to-list 'ac-modes 'enh-ruby-mode) ;; Enable auto-complete-mode
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ruby\\|ru\\|jbuilder\\|arb\\)$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Gemfile\\|Rakefile\\|\\.pryrc\\)$" . enh-ruby-mode))
(with-eval-after-load 'enh-ruby-mode
  (setq enh-ruby-deep-arglist nil
        enh-ruby-deep-indent-paren nil
        enh-ruby-deep-indent-paren-style nil)
  (defun enh-ruby-mode-set-encoding () nil)
  (custom-set-faces
   '(erm-syn-errline  ((t (:weight bold :background "red3" :underline nil))))
   '(erm-syn-warnline ((t (:weight bold :background "goldenrod4" :underline nil))))
   '(enh-ruby-op-face ((t (:foreground "#00af5f")))))

  ;; C-ceでカーソル位置のerr or warnを表示
  (define-key enh-ruby-mode-map (kbd "C-c e")
    (lambda () (interactive)
      (unless (enh-ruby-show-errors-at (point) 'erm-syn-errline)
        (enh-ruby-show-errors-at (point) 'erm-syn-warnline)))))

(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'helm-etags-select) ;; etags
            (local-set-key (kbd "M-*") 'pop-tag-mark) ;; jump back
            ))

(defun ruby-mode-set-frozen-string-literal-true ()
  (when (eq major-mode 'enh-ruby-mode)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (unless (looking-at "^# frozen_string_literal: true")
        (insert "# frozen_string_literal: true\n")))))

(add-hook 'enh-ruby-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'ruby-mode-set-frozen-string-literal-true)))

;; ruby align setup (see https://github.com/daveyeu/emacs-dot-d/blob/master/custom/ruby-align.el)
(defconst align-ruby-modes '(enh-ruby-mode)
  "align-perl-modes is a variable defined in `align.el'.")
(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes  . align-ruby-modes))
    (ruby-new-style-hash
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+:\\(\\s-+\\)[a-zA-Z0-9:'\"]") ;; This guy needs more work.
     (modes  . align-ruby-modes)
     (repeat . t)))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")
(add-to-list 'align-perl-modes 'enh-ruby-mode)
(add-to-list 'align-dq-string-modes 'enh-ruby-mode)
(add-to-list 'align-sq-string-modes 'enh-ruby-mode)
(add-to-list 'align-open-comment-modes 'enh-ruby-mode)
(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))

;;;; {ruby-block}
(el-get-bundle ruby-block)
(with-eval-after-load 'enh-ruby-mode
  (require 'ruby-block)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))

;;;; {ruby-end}
(el-get-bundle ruby-end)
(with-eval-after-load 'enh-ruby-mode
  (require 'ruby-end)
  (setq ruby-end-insert-newline nil))

;;;; {helm-bundle-show}
(el-get-bundle helm-bundle-show :type github :pkgname "masutaka/emacs-helm-bundle-show" :features helm-bundle-show)
(global-set-key (kbd "C-x p") 'helm-bundle-show)

;;;; {rinari}
(el-get-bundle eschulte-jump :type github :pkgname "eschulte/jump.el" :depends (findr))
(el-get-bundle rinari
  :type github :pkgname "eschulte/rinari"
  :load-path ("." "util" "util/jump")
  :build (("bundle")) :submodule nil
  :depends (inf-ruby eschulte-jump)
  :features rinari)

(global-rinari-mode)
(define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
(define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
(define-key rinari-minor-mode-map (kbd "C-c M") 'rinari-find-mailer)
(define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)

;;;; Configure flycheck ruby-reek
;;;; Taken by https://github.com/flycheck/flycheck/pull/886
(flycheck-def-config-file-var flycheck-reekrc ruby-reek "config.reek" :safe #'stringp)
(flycheck-define-checker ruby-reek
  "A Ruby smell checker using reek.
See URL `https://github.com/troessner/reek'."
  :command ("reek" "--format=xml"
            (config-file "--config" flycheck-reekrc)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (enh-ruby-mode ruby-mode)
  :next-checkers ((info . ruby-rubocop)))

;; ruby-rubocopのあとにruby-lintが処理されるようにする
;; (ruby-rubocop のエラーが info 以下なら ruby-reek が実行される)
(setq flycheck-checkers (append flycheck-checkers '(ruby-reek)))
(flycheck-add-next-checker 'ruby-rubocop '(info . ruby-reek))

;;;; {json-mode}
(el-get-bundle json-mode)
(add-to-list 'auto-mode-alist '("\\.babelrc$" . json-mode))

;;;; {coffee-mode}
(el-get-bundle coffee-mode)

;;;; {markdown-mode}
(el-get-bundle markdown-mode)
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . gfm-mode))
(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit outline-1 markdown-header-face))))
 '(markdown-header-face-2 ((t (:inherit outline-2 markdown-header-face))))
 '(markdown-header-face-3 ((t (:inherit outline-3 markdown-header-face))))
 '(markdown-header-face-4 ((t (:inherit outline-4 markdown-header-face))))
 '(markdown-header-face-5 ((t (:inherit outline-5 markdown-header-face))))
 '(markdown-header-face-6 ((t (:inherit outline-6 markdown-header-face))))
 )

;;;; {plantuml-mode}
(el-get-bundle plantuml-mode)
(add-to-list 'ac-modes 'plantuml-mode) ;; Enable auto-complete-mode
(add-to-list 'auto-mode-alist '("\\.uml$" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plu$" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml$" . plantuml-mode))
(setq plantuml-jar-path "/usr/local/Cellar/plantuml/8024/plantuml.8024.jar")
(setq plantuml-run-command "java -Djava.awt.headless=true -jar %s")
(add-hook 'plantuml-mode-hook
          (lambda ()
            ;; configure comment-style
            (set (make-local-variable 'comment-start) "'")
            (set (make-local-variable 'comment-end)   "")

            ;; workaround of error: "Wrong type argument: keymapp, nil"
            (when (null plantuml-mode-map)
              (setq plantuml-mode-map (make-sparse-keymap)))))

;; ;;;; {eimp}
;; ;; Emacs Image Manipulation
;; (el-get-bundle eimp)
;; (add-hook 'image-mode-hook 'eimp-mode)

;;;; {web-mode}
(el-get-bundle web-mode)
(require 'web-mode)
(setq web-mode-engines-alist '())
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'web-mode-engines-alist '("jsx" . "\\.jsx\\'"))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.ctmpl$" . web-mode))
(add-to-list 'web-mode-engines-alist '("go" . "\\.html\\.ctmpl$"))
(add-to-list 'auto-mode-alist '("\\.html.ctmpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-script-padding 2
      web-mode-style-padding 2
      web-mode-enable-auto-closing t
      web-mode-enable-auto-pairing t
      web-mode-enable-auto-opening t
      web-mode-enable-current-element-highlight t)
(custom-set-faces
 '(web-mode-current-element-highlight-face ((t (:background "#00005f")))))

;; configure auto-complete for web-mode
(add-to-list 'ac-modes 'web-mode) ;; Enable auto-complete-mode
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ("erb" . (ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ("javascript" . (ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ("jsx" . (ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ))

;;;; {scss-mode}
(el-get-bundle scss-mode)

;;;; {sass-mode}
(el-get-bundle sass-mode)

;;;; {slim-mode}
(el-get-bundle slim-mode)

;;;; {highlight-indentation}
(el-get-bundle highlight-indentation)
(require 'highlight-indentation)
(setq highlight-indentation-offset 4)
(set-face-background 'highlight-indentation-current-column-face "#5f0000")
(add-hook 'slim-mode-hook
          (lambda()
            (local-set-key (kbd "M-.") 'helm-etags-select) ;; etags
            (local-set-key (kbd "M-*") 'pop-tag-mark) ;; jump back
            (highlight-indentation-mode)
            (highlight-indentation-current-column-mode)))

;;;; {dockerfile-mode}
(el-get-bundle dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\." . dockerfile-mode))

;;;; {open-junk-file}
;; open-junk-fileパッケージがautoloadに対応してないので自分で設定
(el-get-bundle open-junk-file)
(autoload 'open-junk-file "open-junk-file" nil t)
(global-set-key (kbd "C-c , ,") 'open-junk-file)
(setq open-junk-file-format "~/Dropbox/journals/junk/%Y/%m/%Y_%m_%d.md")

;;;; {terraform-mode}
(el-get-bundle elpa:hcl-mode :repo ("marmalade" . "http://marmalade-repo.org/packages/"))
(el-get-bundle elpa:terraform-mode :repo ("marmalade" . "http://marmalade-repo.org/packages/"))
(setq terraform-indent-level 4)

;;;; {swift-mode}
(el-get-bundle swift-mode)
(require 'swift-mode)
(add-to-list 'flycheck-checkers 'swift)
(setq flycheck-swift-sdk-path
      (replace-regexp-in-string
       "\n+$" "" (shell-command-to-string
                  "xcrun --show-sdk-path --sdk macosx")))

;;;; {dashboard}
(el-get-bundle rakanalh/emacs-dashboard :depends (page-break-lines) :features dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((projects . 5) (recents  . 20)))
(add-hook 'dashboard-mode-hook
          '(lambda ()
             (local-set-key (kbd "n") 'widget-forward)
             (local-set-key (kbd "C-n") 'widget-forward)
             (local-set-key (kbd "C-p") 'widget-backward)))

;;;; {smartparens}
(el-get-bundle smartparens)
(smartparens-global-mode)

;;;; {protobuf-mode}
(el-get-bundle protobuf-mode)
