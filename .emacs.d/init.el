;; -*- coding:utf-8 -*-

;;;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;;; darwin
(when (eq system-type 'darwin)
  ;; disable C-z
  (global-set-key "\C-z" nil)
  ;; swap 'Command' for 'option'
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  ;; set transparency
  (set-frame-parameter nil 'alpha 80)
)

;;;; basic
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore) ;; ignore bell
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [backspace] 'delete-backward-char)
(cond
 (window-system (tool-bar-mode -1))
 (t             (menu-bar-mode -1)))

;;;; generic-x
(require 'generic-x)

;;;; custom-theme
(add-to-list 'custom-theme-load-path  "~/.emacs.d/themes/")
(load-theme 'tkzw t)

;;;; font Ricty Discord
(create-fontset-from-ascii-font "Ricty Discord:size=14:weight=normal:slant=normal" nil "rictydiscord")
(set-fontset-font "fontset-rictydiscord"
                  'unicode
                  (font-spec :family "Ricty Discord" :size 14)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-rictydiscord"))

;;;; {exec-path-from-shell}
(let ((envs '("PATH" "MANPATH" "GEM_PATH" "GEM_HOME" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;;;; {dired}
(setq dired-dwim-target t)

;;;; {helm}
(helm-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)
;(setq helm-locate-command "locate-with-mdfind %.0s %s")

;;;; {anything}
(require 'anything-startup)
(recentf-mode 1) ; for anyting source-buffers+
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
          `(
            anything-c-source-buffers+-custom
            ,@filelist-sources)))
    (anything-find-file)))

(global-set-key (kbd "C-x C-f") 'takezawa/anything-find-file)

;;;; {elscreen}
(when (require 'elscreen nil t)
  (setq elscreen-prefix-key [?\C-q])
  (setq elscreen-display-screen-number nil)
  (elscreen-start)

  (defun elscreen-show-display-tab ()
    (interactive)
    (setq elscreen-display-tab nil)
    (elscreen-notify-screen-modification 'force))
  (defun elscreen-hide-display-tab ()
    (interactive)
    (setq elscreen-display-tab t)
    (elscreen-notify-screen-modification 'force))

  ;; ediff-mode
  (add-hook 'ediff-before-setup-hook 'elscreen-show-display-tab)
  ;; (add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
  (add-hook 'ediff-quit-hook 'elscreen-hide-display-tab)
  )
