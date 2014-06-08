;; -*- coding:utf-8 -*-

;; change C-h to Backspace
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [backspace] 'delete-backward-char)

;; swap 'Command' for 'option'
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; set transparency
(set-frame-parameter nil 'alpha 80)

(setq dired-dwim-target t)

;; ignore bell
(setq ring-bell-function 'ignore)

(cond
 (window-system (tool-bar-mode -1))
 (t             (menu-bar-mode -1)))

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

