;; -*- coding:utf-8 -*-

;;;; {cask}
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

;;;; [Configure builtin features]
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [backspace] 'delete-backward-char)
(global-set-key (kbd "C-z") nil)
(setq inhibit-startup-message t)
(setq-default frame-background-mode 'dark)
(setq ring-bell-function 'ignore) ; ignore bell
(cond
 (window-system (tool-bar-mode -1))
 (t             (menu-bar-mode -1)))
(windmove-default-keybindings)
(add-hook 'makefile-mode 'intent-tabs-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq vc-follow-symlinks t)
(show-paren-mode 1)
(column-number-mode t)
(global-auto-revert-mode 1)
(auto-image-file-mode +1)
(setq dired-dwim-target t)
(require 'generic-x)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ; auto chmod +x
(global-set-key (kbd "C-c C-c") 'compile)

;; tweak indentatino
(setq-default indent-tabs-mode nil) ; [Tab] key insert spaces.
(setq sh-basic-offset 2
      sh-indentation 2)

;; utf-8 coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq locale-coding-system 'utf-8) ; for ansi term-mode

;; cua
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "C-c SPC") 'cua-set-rectangle-mark)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-use-other-window t)

;; indent
(global-set-key (kbd "C-c i") 'indent-region)

;; recentf
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
      recentf-max-saved-items 2000
      recentf-exclude '(".recentf"))

;; save place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".emacs-places" user-emacs-directory))

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; which-function-mode (builtin)
(which-function-mode 1)
(setq which-func-modes t) ;; If this is equal to t, then it is enabled in any major mode that supports it.
(global-set-key (kbd "C-c w") (lambda () (interactive) (message (which-function))))

;; highlight current-line (builtin)
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :background "gray9" :underline t)

;; whitespace-mode (builtin)
(setq whitespace-space-regexp "\\(\u3000+\\)") ; 全角スペース
(setq whitespace-style '(face      ; faceで可視化
                         trailing  ; 行末
                         tabs      ; タブ
                         spaces    ; スペース
                         empty     ; 先頭/末尾の空行
                         tab-mark))
(setq whitespace-action '(auto-cleanup)) ; ファイル保存時に余分な空白を削除
(custom-set-faces
 '(whitespace-trailing ((t (:background nil :foreground "DeepPink"     :underline t))))
 '(whitespace-tab      ((t (:background nil :foreground "LightSkyBlue" :underline t))))
 '(whitespace-space    ((t (:background nil :foreground "GreenYellow"  :underline t)))))
(global-whitespace-mode 1)

;; align (builtin)
(require 'align)
(global-set-key (kbd "C-c a") 'align)

;; table align with org-mode
;; e.g.
;; |---+---|
;; | X | Y |
;; |---+---|
;; | a | 1 |
;; | b | 2 |
;; |---+---|
(autoload 'org-table-align "org-table" nil)
(global-set-key (kbd "C-c t") 'org-table-align)

;; define window resizer (see: http://d.hatena.ne.jp/khiker/20100119/window_resize)
(global-set-key (kbd "C-c r") 'takezawa/window-resizer)
(defun takezawa/window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width) (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

;; for darwin (Mac OS X)
(when (eq system-type 'darwin)
  ;; swap 'Command' for 'option'
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  ;; set transparency
  (add-to-list 'default-frame-alist '(alpha . 80))
  ;; font Ricty Discord
  (create-fontset-from-ascii-font "Ricty Discord:size=14:weight=normal:slant=normal" nil "rictydiscord")
  (set-fontset-font "fontset-rictydiscord"
                    'unicode
                    (font-spec :family "Ricty Discord" :size 14)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-rictydiscord")))

;;;; End [Configure builtin features]

;;;; custom-theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'tkzw t)

;;;; {exec-path-from-shell}
(let ((envs '("PATH" "MANPATH" "GEM_PATH" "GEM_HOME" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;;;; {helm}
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)
;; via: http://d.hatena.ne.jp/syohex/20131016/1381935863
(global-set-key (kbd "M-.") 'takezawa/helm-etags-select)
(defun takezawa/helm-etags-select (arg)
  (interactive "P")
  (let ((tag  (helm-etags-get-tag-file))
        (helm-execute-action-at-once-if-one t))
    (when (or (equal arg '(4))
              (and helm-etags-mtime-alist
                   (helm-etags-file-modified-p tag)))
      (remhash tag helm-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-source-etags-select :keymap helm-etags-map
              :input (concat (thing-at-point 'symbol) " ")
              :buffer "*helm etags*"
              :default (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
      (message "Error: No tag file found, please create one with etags shell command."))))

;;;; {helm-ag}
(global-set-key (kbd "C-c g") 'helm-ag)

;;;; {helm-ghq}
(global-set-key (kbd "C-x g") 'helm-ghq)

;;;; {helm-ls-git}
;; define takezawa/helm-for-files with helm-ls-git
(defvar takezawa/helm-source-home-filelist
  `((name . "Home FileList")
    (candidates-file ,(expand-file-name "home.filelist" user-emacs-directory) t)
    (action . ,(cdr (helm-get-actions-from-type
                     helm-source-locate)))))

(defvar takezawa/helm-source-system-filelist
  `((name . "System FileList")
    (candidates-file ,(expand-file-name "system.filelist" user-emacs-directory) t)
    (action . ,(cdr (helm-get-actions-from-type
                     helm-source-locate)))))

(require 'helm-ls-git)
(defun takezawa/helm-for-files ()
  "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer
     '(helm-source-buffers-list
       helm-source-recentf
       ;; helm-source-bookmarks
       ;; elm-source-file-cache
       helm-source-files-in-current-dir
       helm-source-ls-git
       takezawa/helm-source-home-filelist
       takezawa/helm-source-system-filelist
       ;; helm-source-locate
       )
     "*takezawa/helm-for-files*")))
(global-set-key (kbd "C-x f") 'takezawa/helm-for-files)

;;;; {elscreen}
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

;;;; {flycheck}
;; requires
;;  - ruby: gem install rubocop
;;  - js:   npm install -g jshint
;;  - json: npm install -g jsonlint
(add-hook 'after-init-hook #'global-flycheck-mode)
;; flycheck disable specific modes
(setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
(setq flycheck-display-errors-delay 0.1)
(custom-set-faces
 '(flycheck-error ((t (:background "red4" :weight bold))))
 '(flycheck-warning ((t (:background "yellow4" :weight bold)))))

;;;; {etags-table}
(require 'etags-table)
(setq etags-table-search-up-depth 10)

;;;; {git-gutter}
(global-git-gutter-mode 1)

;;;; {auto-complete}
(ac-config-default)
(add-to-list 'ac-dictionary-directories (expand-file-name "ac-dict" user-emacs-directory))
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(setq ac-ignore-case nil)

;;;; {ac-helm}
(global-set-key (kbd "M-/") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "M-/") 'ac-complete-with-helm)

;;;; {crontab-mode}
(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\(_\\|\\.\\)" . crontab-mode))

;;;; {yaml-mode}
(add-to-list 'auto-mode-alist '("\\.yml\\." . yaml-mode))

;;;; {go-mode}
;; golang requirements
; go get -v -u code.google.com/p/rog-go/exp/cmd/godef
; go get -v -u code.google.com/p/go.tools/cmd/godoc
; go get -v -u code.google.com/p/go.tools/cmd/goimports
; go get -v -u github.com/golang/lint/golint      # flycheck
; go get -v -u code.google.com/p/go.tools/cmd/vet # flycheck
; go get -v -u github.com/kisielk/errcheck        # flycheck
; go get -v -u github.com/nsf/gocode # go-eldoc
(eval-after-load 'go-mode
  '(progn
     (add-hook 'before-save-hook 'gofmt-before-save)
     (setq gofmt-command "goimports")
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "C-c d") 'godoc)
     (add-hook 'go-mode-hook 'go-eldoc-setup)))

;;;; {enh-ruby-mode}
(add-to-list 'ac-modes 'enh-ruby-mode) ;; Enable auto-complete-mode
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\)$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(eval-after-load 'enh-ruby-mode
  '(progn
     (setq enh-ruby-deep-arglist nil
           enh-ruby-deep-indent-paren nil
           enh-ruby-deep-indent-paren-style nil)
     (custom-set-faces
      '(erm-syn-errline  ((t (:weight bold :background "red3" :underline t))))
      '(erm-syn-warnline ((t (:weight bold :background "yellow3" :underline t))))
      '(enh-ruby-op-face ((t (:foreground "#00af5f")))))

     ;; C-ceでカーソル位置のerr or warnを表示
     (define-key enh-ruby-mode-map (kbd "C-c e")
       (lambda () (interactive)
         (unless (enh-ruby-show-errors-at (point) 'erm-syn-errline)
           (enh-ruby-show-errors-at (point) 'erm-syn-warnline))))))

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
     (modes  . align-ruby-modes)))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")
(add-to-list 'align-perl-modes 'enh-ruby-mode)
(add-to-list 'align-dq-string-modes 'enh-ruby-mode)
(add-to-list 'align-sq-string-modes 'enh-ruby-mode)
(add-to-list 'align-open-comment-modes 'enh-ruby-mode)
(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))

;;;; {ruby-block}
(eval-after-load 'enh-ruby-mode
  '(progn
     (require 'ruby-block)
     (ruby-block-mode t)
     (setq ruby-block-highlight-toggle t)))

;;;; {ruby-end}
(eval-after-load 'enh-ruby-mode
  '(progn
     (require 'ruby-end)
     (setq ruby-end-insert-newline nil)))

;;;; {rinari}
(global-rinari-mode)
(define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
(define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
(define-key rinari-minor-mode-map (kbd "C-c M") 'rinari-find-mailer)
(define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)
(define-key rinari-minor-mode-map (kbd "C-c p") 'rinari-goto-partial)

;;; {plantuml-mode}
(add-to-list 'ac-modes 'plantuml-mode) ;; Enable auto-complete-mode
(add-to-list 'auto-mode-alist '("\\.plu$" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml$" . plantuml-mode))
(setq plantuml-jar-path "/usr/local/Cellar/plantuml/8002/plantuml.8002.jar")
(add-hook 'plantuml-mode-hook
          (lambda ()
            ;; workaround of error: "Wrong type argument: keymapp, nil"
            (when (null plantuml-mode-map)
              (setq plantuml-mode-map (make-sparse-keymap)))))

;;;; {web-mode}
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(setq web-mode-script-padding 2
      web-mode-style-padding 2
      web-mode-tag-auto-close-style 2
      web-mode-enable-auto-pairing t
      web-mode-enable-auto-opening t
      web-mode-enable-current-element-highlight t)
(custom-set-faces
 '(web-mode-current-element-highlight-face ((t (:background "#00005f")))))
