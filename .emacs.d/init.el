;; -*- coding:utf-8 -*-

;;;; Configure builtin features
(load-file (locate-user-emacs-file "init-builtin.el"))

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
(setq use-package-always-defer t)

(use-package diminish
  :diminish global-whitespace-mode
  :diminish eldoc-mode)

;;;; use-pacakge-report
(setq use-package-compute-statistics t)
(setq use-package-verbose t)

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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (let ((envs '("PATH" "MANPATH" "GOROOT" "GOPATH")))
    (exec-path-from-shell-copy-envs envs)))

(use-package save-framegeometry
  :straight (:type git :repo "https://gist.github.com/218fd80d9390845bf9dcad727237d4af.git")
  :config
  (add-to-list 'recentf-exclude "framegeometry"))

(use-package recentf-ext :demand t)

;;;; custom-theme
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; (load-theme 'tkzw t)

(use-package moe-theme :demand t
  :config
  (moe-dark)
  ;; Customize colors
  (let ((comment "#af5f00")) ;; Assign comment to moe-dark pallete orange-5
    ;; (set-face-background 'default "black") ;; Use true black in background.
  (set-face-background 'default "unspecified-bg") ;; Use terminal background.
  (set-face-attribute font-lock-comment-face           nil :foreground comment :slant 'normal)
  (set-face-attribute font-lock-comment-delimiter-face nil :foreground comment :slant 'normal)))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 0.1)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package which-key :demand t
  :config
  (setq which-key-lighter "")
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package scratch-log
  :config
  (setq takezawa/sl-dir (expand-file-name (locate-user-emacs-file (format-time-string "backups/%Y_%m" (current-time)))))
  (unless (file-exists-p takezawa/sl-dir) (make-directory takezawa/sl-dir t))
  (setq sl-scratch-log-file         (concat takezawa/sl-dir "/scratch-log"))
  (setq sl-prev-scratch-string-file (concat takezawa/sl-dir "/scratch-log-prev"))
  (setq sl-timer-interval 3))

;; ;;;; {auto-save-buffers-enhanced}
;; (el-get-bundle auto-save-buffers-enhanced)
;; (setq auto-save-buffers-enhanced-interval 30.0)
;; (auto-save-buffers-enhanced t)

(use-package backup-each-save
  :init
  (add-hook 'after-save-hook 'backup-each-save)
  (setq backup-each-save-mirror-location
        (expand-file-name (format-time-string "backups/%Y_%m" (current-time)) user-emacs-directory))
  (setq backup-each-save-time-format "%Y%m%d_%H%M%S"))

(use-package dumb-jump)

(use-package smart-jump
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references))
  :config
  (smart-jump-setup-default-registers))

(use-package etags-table
  :config
  (setq etags-table-search-up-depth 10))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  (setq projectile-git-submodule-command nil) ;; workaround for https://github.com/bbatsov/projectile/issues/1323
  :config
  (projectile-global-mode)

  ;; Taken from http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access
  (use-package dash) ;; --first is defined in dash.el
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
                      (when (file-exists-p cmd) (setq flycheck-javascript-eslint-executable cmd)))))))))

(use-package helm
  :diminish helm-mode
  :bind (("C-x C-f" . helm-find-files)
         ("C-x f" . helm-for-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         ("C-c i" . helm-semantic-or-imenu))
  :init
  ;; (global-set-key (kbd "M-.") 'helm-etags-select) ;; etags
  ;; (global-set-key (kbd "M-*") 'pop-tag-mark) ;; jump back

  ;; macOSのときは locate の代わりに mdfind を使う
  (when (eq system-type 'darwin)
    (setq helm-locate-fuzzy-match nil)
    (setq helm-locate-command "mdfind -name %s %s"))

  ;; ;; TAGS絞込のとき、helmバッファの見た目通りにマッチさせる
  ;; (setq helm-etags-match-part-only nil)

  ;; set column size of helm buffer list
  (setq helm-buffer-max-length 50)

  :config
  (helm-mode 1)
  ;; ファイルリスト(candidates-file)でskip matchできるようにする
  (require 'helm-multi-match)
  (bind-keys :map helm-map
             ("C-h" . delete-backward-char)
             ("C-q" . helm-execute-persistent-action) ;; C-qでチラ見
             ("C-M-i" . helm-select-action) ;; C-M-iでアクション選択
             ;; find-fileのときC-iで選択
             :map helm-read-file-map
             ("TAB" . helm-execute-persistent-action)
             :map helm-find-files-map
             ("TAB" . helm-execute-persistent-action))

  (use-package helm-ls-git :demand t
    :bind (("C-x G" . helm-ls-git-ls))
    :config
    (unless helm-source-ls-git-status
      (setq helm-source-ls-git-status (helm-ls-git-build-git-status-source))))

  (use-package helm-projectile :demand t
    :bind (:map projectile-mode-map
           ("C-c C-p g" . helm-projectile-rg)
           ("C-x r"     . helm-projectile)))

  ;; Configure helm-for-files with helm-ls-git and helm-projectile
  (custom-set-variables
   '(helm-for-files-preferred-list
     '(;; helm-source-buffers-list
       helm-source-ls-git-status
       ;; helm-source-projectile-files-list
       helm-source-recentf
       ;; helm-source-bookmarks
       ;; helm-source-file-cache
       ;; helm-source-files-in-current-dir
       helm-source-projectile-files-list
       helm-source-locate))))

;; (defun takezawa/helm-for-files ()
;;   "Preconfigured `helm' for opening files.
;; Run all sources defined in `takezawa/helm-for-files-preferred-list'."
;;   (interactive)
;;   (require 'helm-projectile)
;;   (unless helm-source-buffers-list
;;     (setq helm-source-buffers-list
;;           (helm-make-source "Buffers" 'helm-source-buffers)))
;;   (unless helm-source-ls-git
;;     (setq helm-source-ls-git
;;           (helm-make-source "Git files" 'helm-ls-git-source
;;             :fuzzy-match helm-ls-git-fuzzy-match)))
;;   (let ((helm-ff-transformer-show-only-basename nil))
;;     (helm :buffer "*takezawa/helm-for-files*"
;;           :sources '(helm-source-buffers-list
;;                      helm-source-recentf
;;                      ;; helm-source-bookmarks
;;                      ;; helm-source-file-cache
;;                      helm-source-files-in-current-dir
;;                      ;; helm-source-locate
;;                      ;; helm-source-ls-git
;;                      helm-source-projectile-files-list
;;                      takezawa/helm-source-home-filelist
;;                      takezawa/helm-source-system-filelist))))
;; ;; 最近使っていないのでコメントアウト
;; ;; (global-set-key (kbd "C-x f") 'takezawa/helm-for-files)

(use-package helm-swoop
  :bind (("C-c C-s" . takezawa/helm-swoop))
  :config
  ;; デフォルトでhelm-swoopで周辺3行も表示する つまり M-3 M-x helm-swoop と同じ
  (defun takezawa/helm-swoop ()
    (interactive)
    (if current-prefix-arg
        (helm-swoop)
      (let ((current-prefix-arg '(3)))
        (helm-swoop)))))

(use-package helm-ag
  :bind (("C-c g" . takezawa/helm-do-ag-dir))
  :config
  (setq helm-ag-insert-at-point 'symbol)
  ;; C-c C-e: Switch to edit mode
  ;; C-u打つのが面倒なので、常にC-uの挙動(ディレクトリ選択を求める)にする
  (defun takezawa/helm-do-ag-dir ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (helm-do-ag))))

(use-package helm-rg
  :bind (("C-c g" . helm-rg)))

(use-package helm-ghq
  :bind (("C-x g" . helm-ghq)))

(with-eval-after-load 'helm
  ;; define takezawa/helm-for-files with helm-ls-git
  (defvar takezawa/helm-source-home-filelist
    `((name . "Home FileList")
      (candidates-file ,(expand-file-name "home.filelist" user-emacs-directory) t)
      (action . ,(helm-actions-from-type-file))))

  (defvar takezawa/helm-source-system-filelist
    `((name . "System FileList")
      (candidates-file ,(expand-file-name "system.filelist" user-emacs-directory) t)
      (action . ,(helm-actions-from-type-file)))))

(use-package elscreen
  :bind (("C-q" . elscreen-start))
  :init
  (setq elscreen-prefix-key (kbd "C-q"))
  (setq elscreen-display-screen-number nil)
  :config
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

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :diminish company-mode
  :hook (after-init . global-company-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package visual-regexp-steroids
  :bind (("C-M-%" . vr/query-replace)
         ("C-M-s" . vr/isearch-forward)
         ("C-M-r" . vr/isearch-backward)))

;;;; {flycheck}
;; requires
;;  * gem install rubocop
;;  * npm install -g jsonlint
(use-package flycheck
  :bind (("C-c l" . flycheck-list-errors)
         ("C-c e" . flycheck-explain-error-at-point))
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; flycheck disable specific modes
  (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc html-tidy))
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)) ;; new-lineは頻度が多すぎて重いので除外
  (setq flycheck-idle-change-delay 20.0)
  (custom-set-faces
   '(flycheck-error ((t (:background "red4" :weight bold))))
   '(flycheck-warning ((t (:background "color-58" :weight bold))))
   '(flycheck-info ((t (:foreground "white" :background "darkgreen")))))
  :config
  ;; Enable eslint in web-mode
  ;; * npm install -g eslint
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flycheck-tip
  :config
  (bind-keys (("M-g n" . error-tip-cycle-dwim)
              ("M-g p" . error-tip-cycle-dwim-reverse))))

(use-package git-gutter :demand t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1)
  (setq git-gutter:diff-option "-w"))

;; gitattributes-mode, gitconfig-mode, gitignore-mode
(use-package git-modes
  :mode (("/\\.gitignore_global\\'" . gitignore-mode))
  :init
  ;; TODO: これでもなぜかタブでインデントさせてくれない
  (add-hook 'gitconfig-mode-hook 'intent-tabs-mode))

(use-package magit
  :mode ("COMMIT_EDITMSG\\'" . magit-file-mode)
  :config
  (unbind-key "C-x g" magit-file-mode-map))

;;;; 絵文字
;; Test characters
;; 01234567890123456789
;; ※①・▽☆□α■→×
;; Emoji:🍎🚗🔫🚀
;; (el-get-bundle emojify)
;; (emojify-set-emoji-styles '(unicode))
;; (add-hook 'after-init-hook #'global-emojify-mode)

(use-package yasnippet :defer 5
  :config
  (yas-global-mode 1)
  (defun my/downcase-first-char (&optional string)
    "Capitalize only the first character of the input STRING."
    (when (and string (> (length string) 0))
      (let ((first-char (substring string nil 1))
            (rest-str   (substring string 1)))
        (concat (downcase first-char) rest-str)))))

(use-package yasnippet-snippets)

(use-package helm-c-yasnippet
  :bind (("C-c y" . helm-yas-complete)))

(use-package crontab-mode
  :mode (("\\.?cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\(_\\|\\.\\)" . crontab-mode)))

(use-package yaml-mode
  :mode (("\\.yml\\." . yaml-mode)
         ("user-data\\(\\.j2\\)?$" . yaml-mode)
         ("\\.eslintrc$" . yaml-mode)))

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

(use-package go-mode)
(use-package go-guru)
(use-package go-eldoc)
(use-package gotest)
(use-package gorepl-mode)
(with-eval-after-load 'go-mode
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq go-packages-function 'go-packages-go-list)
  (setq godoc-at-point-function 'godoc-gogetdoc)
  ;;(define-key go-mode-map (kbd "M-.") 'godef-jump)
  ;;(define-key go-mode-map (kbd "C-x 4 M-.") 'godef-jump-other-window)
  ;;(define-key go-mode-map (kbd "M-.") 'go-guru-definition)
  (define-key go-mode-map (kbd "C-x 4 M-.") 'go-guru-definition-other-window)
  (define-key go-mode-map (kbd "C-c d") 'godoc-at-point)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  ;(add-hook 'go-mode-hook #'gorepl-mode)
  (add-hook 'go-mode-hook
            '(lambda ()
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

(use-package flycheck-gometalinter
  :disabled
  :config
  ;; flycheckデフォルトのcheckerのうち最後の go-unconvert よりも後に gometalinter を実行させる
  (flycheck-add-next-checker 'go-unconvert '(warning . gometalinter))
  (add-hook 'go-mode-hook
            '(lambda ()
               (flycheck-select-checker 'go-gofmt)))
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
          )))


(use-package enh-ruby-mode
  :mode (("\\.\\(rb\\|ruby\\|ru\\|jbuilder\\|arb\\)\\'" . enh-ruby-mode)
         ("\\(Gemfile\\|Rakefile\\|\\.pryrc\\)\\'" . enh-ruby-mode)
         ("\\(IAMfile\\|\\.iam\\)\\'" . enh-ruby-mode))
  :config
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
        (enh-ruby-show-errors-at (point) 'erm-syn-warnline))))

  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              ;;(local-set-key (kbd "M-.") 'helm-etags-select) ;; etags
              ;;(local-set-key (kbd "M-*") 'pop-tag-mark) ;; jump back
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
              (add-hook 'before-save-hook 'ruby-mode-set-frozen-string-literal-true))))

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

(use-package ruby-block
  :init
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (setq ruby-block-highlight-toggle t)
              (ruby-block-mode t))))

(use-package ruby-end
  :init
  (setq ruby-end-insert-newline nil))

(use-package helm-bundle-show
  :bind (("C-x p" . helm-bundle-show)))

(use-package rinari
  :disabled
  :config
  (global-rinari-mode)
  (define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
  (define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
  (define-key rinari-minor-mode-map (kbd "C-c M") 'rinari-find-mailer)
  (define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view))

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

(use-package json-mode
  :mode (("\\.babelrc$" . json-mode)))

(use-package coffee-mode)

(use-package markdown-mode
  :mode (("\\.\\(md\\|markdown\\)$" . gfm-mode))
  :config
  (custom-set-faces
   '(markdown-header-face-1 ((t (:inherit outline-1 markdown-header-face))))
   '(markdown-header-face-2 ((t (:inherit outline-2 markdown-header-face))))
   '(markdown-header-face-3 ((t (:inherit outline-3 markdown-header-face))))
   '(markdown-header-face-4 ((t (:inherit outline-4 markdown-header-face))))
   '(markdown-header-face-5 ((t (:inherit outline-5 markdown-header-face))))
   '(markdown-header-face-6 ((t (:inherit outline-6 markdown-header-face))))))

(use-package plantuml-mode
  :mode (("\\.uml$" . plantuml-mode)
         ("\\.plu$" . plantuml-mode)
         ("\\.plantuml$" . plantuml-mode))
  :config
  (setq plantuml-run-command "java -Djava.awt.headless=true -jar %s")
  (add-hook 'plantuml-mode-hook
            (lambda ()
              ;; configure comment-style
              (set (make-local-variable 'comment-start) "'")
              (set (make-local-variable 'comment-end)   "")

              ;; set plantuml-jar-path originated by https://ivanmalison.github.io/dotfiles/#plantumlmode
              (cond ((equal system-type 'darwin)
                     (let* ((plantuml-dir
                             (s-trim (shell-command-to-string "brew --prefix plantuml")))
                            (filename
                   (when (;FIXME: le-exists-p plantuml-dir)
                          (--first (s-ends-with? ".jar" it) (directory-files plantuml-dir))))
                   (filepath (when filename
                               (imalison:join-paths plantuml-dir filename))))
                            (setq plantuml-jar-path filepath
                                  org-plantuml-jar-path filepath)))
                     ((equal system-type 'gnu/linux)
                      (let ((filepath "/opt/plantuml/plantuml.jar"))
                        (setq plantuml-jar-path filepath
                              org-plantuml-jar-path filepath))))

              ;; workaround of error: "Wrong type argument: keymapp, nil"
              (when (null plantuml-mode-map)
                (setq plantuml-mode-map (make-sparse-keymap))))))

;; ;;;; {eimp}
;; ;; Emacs Image Manipulation
;; (el-get-bundle eimp)
;; (add-hook 'image-mode-hook 'eimp-mode)

(use-package web-mode
  :mode (("\\.jsx\\'" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.j2$" . web-mode)
         ("\\.html\\.ctmpl$" . web-mode)
         ("\\.html.ctmpl$" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '())
  (add-to-list 'web-mode-engines-alist '("jsx" . "\\.jsx\\'"))
  (add-to-list 'web-mode-engines-alist '("go" . "\\.html\\.ctmpl$"))
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
   '(web-mode-current-element-highlight-face ((t (:background "#00005f"))))))

(use-package scss-mode)

(use-package sass-mode)

(use-package slim-mode
  :config
  (add-hook 'slim-mode-hook
            (lambda()
              ;;(local-set-key (kbd "M-.") 'helm-etags-select) ;; etags
              ;;(local-set-key (kbd "M-*") 'pop-tag-mark) ;; jump back
              (when (require 'highlight-indentation nil t)
                (highlight-indentation-mode)
                (highlight-indentation-current-column-mode)))))

(use-package highlight-indentation
  :config
  (setq highlight-indentation-offset 4)
  (set-face-background 'highlight-indentation-current-column-face "#5f0000"))

(use-package dockerfile-mode
  :mode (("Dockerfile\\." . dockerfile-mode)))

(use-package open-junk-file
  :bind (("C-c , ," . open-junk-file))
  :config
  (setq open-junk-file-format "~/Dropbox/journals/junk/%Y/%m/%Y_%m_%d.md"))

(use-package terraform-mode
  :config
  (setq terraform-indent-level 4))

(use-package swift-mode
  :config
  (add-to-list 'flycheck-checkers 'swift)
  (setq flycheck-swift-sdk-path
        (replace-regexp-in-string
         "\n+$" "" (shell-command-to-string
                    "xcrun --show-sdk-path --sdk macosx"))))

(use-package dashboard :demand t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((projects . 5) (recents  . 20)))
  (bind-keys :map dashboard-mode-map
             ("n" . widget-forward)
             ("C-n" . widget-forward)
             ("C-p" . widget-backward)))

(use-package smartparens :demand t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(use-package protobuf-mode)
