;; -*- coding:utf-8 -*-

;;;; {cask}
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;;; Configure builtin features
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [backspace] 'delete-backward-char)
(setq inhibit-startup-message t)
(setq-default frame-background-mode 'dark)
(setq ring-bell-function 'ignore) ; ignore bell
(cond
 (window-system (tool-bar-mode -1))
 (t             (menu-bar-mode -1)))
(windmove-default-keybindings)
(setq-default indent-tabs-mode nil) ; [Tab] key insert spaces.
(add-hook 'makefile-mode 'intent-tabs-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq vc-follow-symlinks t)
(show-paren-mode 1)
(column-number-mode t)
(global-auto-revert-mode 1)
(setq dired-dwim-target t)
(require 'generic-x)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ; auto chmod +x

;; utf-8 coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq locale-coding-system 'utf-8) ; for ansi term-mode

;; ibuffer
(global-set-key "\C-x\C-b" 'ibuffer)
(setq ibuffer-use-other-window t)

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

;; which-func-mode
(which-func-mode 1)
(setq which-func-modes t)
(global-set-key "\C-cw" (lambda () (interactive) (message (which-function))))

;; highlight current-line
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :background "gray9" :underline t)

;; for darwin (Mac OS X)
(when (eq system-type 'darwin)
  ;; disable C-z
  (global-set-key "\C-z" nil)
  ;; swap 'Command' for 'option'
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  ;; set transparency
  (set-frame-parameter nil 'alpha 80)
  ;; font Ricty Discord
  (create-fontset-from-ascii-font "Ricty Discord:size=14:weight=normal:slant=normal" nil "rictydiscord")
  (set-fontset-font "fontset-rictydiscord"
                    'unicode
                    (font-spec :family "Ricty Discord" :size 14)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-rictydiscord")))

;;;; custom-theme
(add-to-list 'custom-theme-load-path  "~/.emacs.d/themes/")
(load-theme 'tkzw t)

;;;; {whitespace}
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

;;;; {exec-path-from-shell}
(let ((envs '("PATH" "MANPATH" "GEM_PATH" "GEM_HOME" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;;;; {helm}
(helm-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)

;;;; {anything}
(require 'anything-startup)
(recentf-mode 1) ; for anything source-buffers+
(setq anything-c-source-buffers+-custom
      (append '((candidates-number-limit . 15)) anything-c-source-buffers+))
(defvar anything-c-source-home-filelist
  `((name . "Home FileList")
    (candidates-file "~/.home.filelist" updating)
    (requires-pattern . 2)
    (candidate-number-limit . 30)
    (type . file)))

(defun takezawa/anything-find-file ()
  (interactive)
  (let* ((filelist-sources
          '(anything-c-source-home-filelist))
         (anything-find-file-additional-sources
          `(anything-c-source-buffers+-custom
            ,@filelist-sources)))
    (anything-find-file)))

(global-set-key (kbd "C-x C-f") 'takezawa/anything-find-file)

;;;; {elscreen}
(when (require 'elscreen nil t)
  (setq elscreen-prefix-key [?\C-q])
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
(eval-after-load 'flycheck
  '(progn
     (setq flycheck-display-errors-delay 0.1)
     (set-face-background 'flycheck-error "red")
     (set-face-background 'flycheck-warning "orange")))

;;;; {etags-table}
(require 'etags-table)
(setq etags-table-search-up-depth 10)

;;;; {git-gutter}
(global-git-gutter-mode 1)

;;;; {enh-ruby-mode}
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\)$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(eval-after-load 'enh-ruby-mode
  '(progn
     (setq enh-ruby-deep-arglist nil
           enh-ruby-deep-indent-paren nil
           enh-ruby-deep-indent-paren-style nil)
     (custom-set-faces
      '(erm-syn-errline  ((t (:background "red" :underline t))))
      '(erm-syn-warnline ((t (:background "magenta" :underline t))))
      '(enh-ruby-op-face ((t (:foreground "#00af5f")))))

     ;; C-ceでカーソル位置のerr or warnを表示
     (define-key enh-ruby-mode-map (kbd "C-c e")
       (lambda () (interactive)
         (unless (enh-ruby-show-errors-at (point) 'erm-syn-errline)
           (enh-ruby-show-errors-at (point) 'erm-syn-warnline))))))

;;;; {ruby-block}
(eval-after-load 'enh-ruby-mode
  '(progn
     (require 'ruby-block)
     (ruby-block-mode t)
     (setq ruby-block-highlight-toggle t)))
