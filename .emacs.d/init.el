;; -*- coding:utf-8 -*-

;;;;;;;;;;;;;;;; Begin [Configure builtin features] ;;;;;;;;;;;;;;;;
;; Emacs標準機能による設定

(package-initialize)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key [backspace] 'delete-backward-char)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-\\") nil)  ; disable toggle-input-method
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
(require 'generic-x)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ; auto chmod +x
(global-set-key (kbd "C-c C-c") 'compile)

;; desktop-save-mode (builtin)
(when (window-system)
  (setq desktop-files-not-to-save "") ;; バッファは復元しないようにする
  (desktop-save-mode 1))

;; server server for emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; dired
;; C-x C-qでwdired
(setq dired-dwim-target t)

;; tweak indentation
(setq-default indent-tabs-mode nil) ; [Tab] key insert spaces.
(setq sh-basic-offset 2
      sh-indentation 2)

;; css indentation
(setq css-indent-offset 2)

;; utf-8 coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq locale-coding-system 'utf-8) ; for ansi term-mode

;; cua (rectangle selection)
;; cua-rectangle-mark-mode is from Emacs 24.4
(if (functionp 'cua-rectangle-mark-mode)
    (progn
      (global-set-key (kbd "C-x SPC") 'cua-rectangle-mark-mode))
  (progn
    (cua-mode 1)
    (setq cua-enable-cua-keys nil)
    (global-set-key (kbd "C-x SPC") 'cua-set-rectangle-mark)))

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-use-other-window t)

;; indent
(global-set-key (kbd "C-c i") 'indent-region)

;; recentf
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
      recentf-max-saved-items 2000
      recentf-auto-cleanup 10
      recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)
      recentf-exclude '(".recentf"))
; 起動直後に履歴表示
(recentf-mode 1)
(add-hook 'after-init-hook
          (lambda()
            (recentf-open-files)))

;; save place
(require 'saveplace)
(setq save-place-file (locate-user-emacs-file ".emacs-places"))
(save-place-mode 1)

;; バックアップファイル(*~)の保存先を変更
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file (format-time-string "backups/%Y_%m" (current-time))))))

;; 自動保存ファイル(#*#)の保存先を変更 (デフォルトだと/tmpでマシンが再起動したときに消えてしまう)
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file (format-time-string "backups/%Y_%m" (current-time))) t)))

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ダブルクリックでURLを開く
(global-set-key [double-mouse-1] 'ffap-at-mouse)

;; read-onlyファイルをtrampでsudoして開く
;; via: http://tsdh.wordpress.com/2008/08/20/re-open-read-only-files-as-root-automagically/
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; which-function-mode (builtin)
(which-function-mode 1)
(setq which-func-modes t) ;; If this is equal to t, then it is enabled in any major mode that supports it.
(global-set-key (kbd "C-c w") (lambda () (interactive) (message (which-function))))

;; highlight current-line (builtin)
;; see: http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(require 'hl-line)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.05 t 'global-hl-line-timer-function))

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
 '(whitespace-tab      ((t (:background nil :foreground "DarkSlateGray" :underline t))))
 '(whitespace-space    ((t (:background nil :foreground "GreenYellow"  :underline t)))))
(global-whitespace-mode 1)

;; align (builtin)
(require 'align)
(global-set-key (kbd "C-c a") 'align)

;; eldoc-mode (builtin)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;; flyspell-mode
; configure for camelCase
(setq ispell-extra-args '("--run-together" "--run-together-limit=6" "--run-together-min=2"))
(add-to-list 'ispell-extra-args "--sug-mode=ultra")
(dolist (hook '(web-mode-hook js-mode-hook ruby-mode-hook enh-ruby-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; conf-unix-mode (builtin)
(add-to-list 'auto-mode-alist '("\\.service$" . conf-unix-mode))

;; Open .env and .env.* files with shell-script-mode
(add-to-list 'auto-mode-alist '("\\.env" . shell-script-mode))

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

;; Atom Edtior
(defun open-atom ()
  (interactive)
  (call-process "atom" nil nil nil buffer-file-name))

;; define window resizer  via: http://d.hatena.ne.jp/khiker/20100119/window_resize
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

;; see: http://www.gfd-dennou.org/member/uwabami/cc-env/emacs/language_config.html
(defun set-east-asian-ambiguous-width (width)
  (dolist (range
           '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
                    (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
                    #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1)
                    #x00E6 (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
                    (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
                    #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
                    (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
                    (#x0148 . #x014B) #x014D (#x0152 . #x0153)
                    (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
                    #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4
                    #x02C7 (#x02C9 . #x02CB) #x02CD #x02D0
                    (#x02D8 . #x02DB) #x02DD #x02DF (#x0300 . #x036F)
                    (#x0391 . #x03A9) (#x03B1 . #x03C1) (#x03C3 . #x03C9)
                    #x0401 (#x0410 . #x044F) #x0451 #x2010
                    (#x2013 . #x2016) (#x2018 . #x2019) (#x201C . #x201D)
                    (#x2020 . #x2022) (#x2024 . #x2027) #x2030
                    (#x2032 . #x2033) #x2035 #x203B #x203E #x2074 #x207F
                    (#x2081 . #x2084) #x20AC #x2103 #x2105 #x2109 #x2113
                    #x2116 (#x2121 . #x2122) #x2126 #x212B
                    (#x2153 . #x2154) (#x215B . #x215E) (#x2160 . #x216B)
                    (#x2170 . #x2179) (#x2190 . #x2199) (#x21B8 . #x21B9)
                    #x21D2 #x21D4 #x21E7 #x2200 (#x2202 . #x2203)
                    (#x2207 . #x2208) #x220B #x220F #x2211 #x2215 #x221A
                    (#x221D . #x2220) #x2223 #x2225 (#x2227 . #x222C)
                    #x222E (#x2234 . #x2237) (#x223C . #x223D) #x2248
                    #x224C #x2252 (#x2260 . #x2261) (#x2264 . #x2267)
                    (#x226A . #x226B) (#x226E . #x226F) (#x2282 . #x2283)
                    (#x2286 . #x2287) #x2295 #x2299 #x22A5 #x22BF #x2312
                    (#x2460 . #x24E9) (#x24EB . #x254B) (#x2550 . #x2573)
                    (#x2580 . #x258F) (#x2592 . #x2595) (#x25A0 . #x25A1)
                    (#x25A3 . #x25A9) (#x25B2 . #x25B3) (#x25B6 . #x25B7)
                    (#x25BC . #x25BD) (#x25C0 . #x25C1) (#x25C6 . #x25C8)
                    #x25CB (#x25CE . #x25D1) (#x25E2 . #x25E5) #x25EF
                    (#x2605 . #x2606) #x2609 (#x260E . #x260F)
                    (#x2614 . #x2615) #x261C #x261E #x2640 #x2642
                    (#x2660 . #x2661) (#x2663 . #x2665) (#x2667 . #x266A)
                    (#x266C . #x266D) #x266F #x273D (#x2776 . #x277F)
                    (#xE000 . #xF8FF) (#xFE00 . #xFE0F) #xFFFD ))
    (set-char-table-range char-width-table range width))
  (set-char-table-range char-width-table #x00AC 1))
(set-east-asian-ambiguous-width 2)

;;;;;;;;;;;;;;;; End [Configure builtin features] ;;;;;;;;;;;;;;;;

;;;; font for darwin (Mac OS X)
(when (and (eq system-type 'darwin)
           (window-system))
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

;;;; custom-theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'tkzw t)

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
(let ((envs '("PATH" "MANPATH" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;;;; {recentf-ext}
(el-get-bundle recentf-ext)

;;;; {highlight-symbol}
(el-get-bundle highlight-symbol)
(setq highlight-symbol-idle-delay 0.1)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;;;; {which-key}
(el-get-bundle which-key)
(which-key-setup-side-window-bottom)
(which-key-mode 1)

;;;; {scratch-log}
(el-get-bundle scratch-log)
(setq sl-scratch-log-file         (expand-file-name (locate-user-emacs-file (format-time-string "backups/%Y_%m/scratch-log"      (current-time)))))
(setq sl-prev-scratch-string-file (expand-file-name (locate-user-emacs-file (format-time-string "backups/%Y_%m/scratch-log-prev" (current-time)))))
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
(global-set-key (kbd "M-*") 'dumb-jump-back)

;;;; {etags-table}
(el-get-bundle emacswiki:etags-table)
(require 'etags-table)
(setq etags-table-search-up-depth 10)

;;;; {helm}
(el-get-bundle helm)
(require 'helm-multi-match) ;; ファイルリスト(candidates-file)でskip matchできるようにする
(helm-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
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

;; find-fileのときC-iで選択するようにする
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)

;; Configure helm-for-files
(setq helm-for-files-preferred-list
      '(;; helm-source-buffers-list
        helm-source-recentf
        ;; helm-source-bookmarks
        ;; helm-source-file-cache
        ;; helm-source-files-in-current-dir
        helm-source-locate))
(global-set-key (kbd "C-x C-r") 'helm-for-files)

;;;; {helm-swoop}
(el-get-bundle helm-swoop)
;; M-3 M-iで3行ずつ表示
(global-set-key (kbd "M-i") 'helm-swoop)

;;;; {helm-ag}
(el-get-bundle helm-ag)
;; C-u打つのが面倒なので、常にC-uの挙動(ディレクトリ選択を求める)にする
(defun takezawa/helm-do-ag-dir ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (helm-do-ag)))
(global-set-key (kbd "C-c g") 'takezawa/helm-do-ag-dir)

;;;; {ag}
(el-get-bundle ag)
;; C-c C-p で{wgrep}による編集ができる
(global-set-key (kbd "C-c G") 'ag)

;;;; {helm-ghq}
(el-get-bundle helm-ghq)
(global-set-key (kbd "C-x g") 'helm-ghq)

;;;; {helm-ls-git}
(el-get-bundle helm-ls-git)
(global-set-key (kbd "C-x G") 'helm-ls-git-ls)

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

;;;; {foreign-regexp}
(el-get-bundle foreign-regexp)
(require 'foreign-regexp)
(setq foreign-regexp/regexp-type 'perl)

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
(custom-set-faces
 '(flycheck-error ((t (:background "red4" :weight bold))))
 '(flycheck-warning ((t (:background "yellow4" :weight bold)))))

;;;; {flycheck-tip}
(el-get-bundle flycheck-tip)
(require 'flycheck-tip)
(define-key global-map (kbd "C-0") 'error-tip-cycle-dwim)
(define-key global-map (kbd "C-9") 'error-tip-cycle-dwim-reverse)

;; Enable eslint in web-mode
;; * npm install -g eslint
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;;; {git-gutter}
(el-get-bundle git-gutter)
(global-git-gutter-mode 1)

;;;; {git-modes}
;; gitattributes-mode, gitconfig-mode, gitignore-mode
(el-get-bundle git-modes)
(add-to-list 'auto-mode-alist '("/\\.gitignore_global\\'" . gitignore-mode))
(add-hook 'gitconfig-mode-hook 'intent-tabs-mode) ; TODO: これでもなぜかタブでインデントさせてくれない

;;;; 絵文字
(el-get-bundle emojify)
(emojify-set-emoji-styles '(unicode))
(add-hook 'after-init-hook #'global-emojify-mode)

;;;; {auto-complete}
(el-get-bundle auto-complete)
(ac-config-default)
(add-to-list 'ac-dictionary-directories (expand-file-name "ac-dict" user-emacs-directory))
(setq ac-use-menu-map t)
(setq ac-ignore-case nil)

;;;; {ac-helm}
(el-get-bundle ac-helm)
(global-set-key (kbd "M-/") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "M-/") 'ac-complete-with-helm)

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
;; ### Install godef for godef-jump, gocode for go-eldoc, godoc for godoc-at-point
;; $ go get -v -u -f github.com/rogpeppe/godef github.com/nsf/gocode golang.org/x/tools/cmd/godoc
;; ### Install gometalinter and tools
;; $ go get -v -u -f github.com/alecthomas/gometalinter
;; $ gometalinter --no-vendored-linters -d -i -u -f
;; ### Update .goimportsignore
;; $ go get -v -u -f github.com/pwaller/goimports-update-ignore
;; $ goimports-update-ignore -max-depth 20
;; crontab: 0 3 * * * bash -lc '(goimports-update-ignore -max-depth 20) 2>&1 | gawk "{ print strftime(\"\%Y/\%m/\%d \%H:\%M:\%S\"), \$0; fflush() }"' >>$HOME/.crontab.log 2>&1

(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)
(el-get-bundle go-eldoc)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-c d") 'godoc-at-point)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook
            '(lambda ()
               (set (make-local-variable 'tab-width) 4)))
  )

(add-hook 'go-mode-hook
          '(lambda ()
             (add-to-list 'flycheck-disabled-checkers 'go-golint)
             (add-to-list 'flycheck-disabled-checkers 'go-vet)
             (add-to-list 'flycheck-disabled-checkers 'go-errcheck)
             (add-to-list 'flycheck-disabled-checkers 'go-uncovert)
             ))

;;;; {flycheck-gometalinter}
(el-get-bundle flycheck-gometalinter)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

(setq flycheck-gometalinter-deadline "25s")
(setq flycheck-gometalinter-disable-all t)
(setq flycheck-gometalinter-enable-linters
      '(
        ;; "aligncheck" ;; slow
        ;; "deadcode"   ;; slow
        "dupl"
        "errcheck" ;; slow
        "gas"
        "goconst"
        "gocyclo"
        ;; "gofmt"
        ;; "goimports"
        "golint"
        "gosimple"
        ;; "gotype" ;; vendorが考慮されずにimportエラーが起きてしまうのでコメントアウト could not import github.com/foo/bar/baz (can't find import: github.com/foo/bar/baz)
        ;; "ineffassign"
        "interfacer" ;; slow
        ;; "lll"
        ;; "misspell"
        "staticcheck"
        ;; "structcheck" ;; slow
        "unconvert" ;; slow
        ;; "unused"
        ;; "varcheck" ;; slow
        "vet"
        "vetshadow"
        ))

;;;; {enh-ruby-mode}
(el-get-bundle enh-ruby-mode)
(add-to-list 'ac-modes 'enh-ruby-mode) ;; Enable auto-complete-mode
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ruby\\|ru\\|arb\\)$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("unicorn.*\\.rb" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(with-eval-after-load 'enh-ruby-mode
  (setq enh-ruby-deep-arglist nil
        enh-ruby-deep-indent-paren nil
        enh-ruby-deep-indent-paren-style nil)
  (defun enh-ruby-mode-set-encoding () nil)
  (custom-set-faces
   '(erm-syn-errline  ((t (:weight bold :background "red3" :underline t))))
   '(erm-syn-warnline ((t (:weight bold :background "yellow3" :underline t))))
   '(enh-ruby-op-face ((t (:foreground "#00af5f")))))

  ;; C-ceでカーソル位置のerr or warnを表示
  (define-key enh-ruby-mode-map (kbd "C-c e")
    (lambda () (interactive)
      (unless (enh-ruby-show-errors-at (point) 'erm-syn-errline)
        (enh-ruby-show-errors-at (point) 'erm-syn-warnline)))))

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

;;;; {helm-rubygems-local}
(el-get-bundle f-kubotar/helm-rubygems-local
  (global-set-key (kbd "C-x p") 'helm-rubygems-local))

;;;; {rinari}
(el-get-bundle elpa:findr)
(el-get-bundle elpa:jump :repo ("marmalade" . "http://marmalade-repo.org/packages/") :depends (findr inflections))
(el-get-bundle elpa:rinari :depends (inf-ruby ruby-compilation jump))

(global-rinari-mode)
(define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
(define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
(define-key rinari-minor-mode-map (kbd "C-c M") 'rinari-find-mailer)
(define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)
(define-key rinari-minor-mode-map (kbd "C-c p") 'rinari-goto-partial)

;;;; javascript
(add-to-list 'auto-mode-alist '("\\.json5$" . js-mode))
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") 'helm-etags-select) ;; etags
  (setq js-indent-level 2
        js-expr-indent-offset 2)
  (add-to-list 'align-rules-list
               '(javascript-assignment-literal
                 (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(js-mode))))
  (add-to-list 'align-rules-list
               '(javascript-hash-literal
                 (regexp . "^\\s-*[a-zA-Z0-9.:?_\"]+:\\(\\s-+\\)[a-zA-Z0-9:'\"]")
                 (modes  . '(js-mode))))
  )

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

;;;; {emip}
(el-get-bundle eimp)
(add-hook 'image-mode-hook 'eimp-mode)

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

;;;; {slim-mode}
(el-get-bundle slim-mode)

;;;; {highlight-indentation}
(el-get-bundle highlight-indentation)
(require 'highlight-indentation)
(setq highlight-indentation-offset 4)
(set-face-background 'highlight-indentation-current-column-face "#5f0000")
(add-hook 'slim-mode-hook 'highlight-indentation-mode)
(add-hook 'slim-mode-hook 'highlight-indentation-current-column-mode)

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

;;;; {projectile-mode}
(el-get-bundle projectile)
(projectile-global-mode)
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

;;;; {auto-install}
(el-get-bundle auto-install)
(setq auto-install-directory (locate-user-emacs-file "auto-install"))
(add-to-list 'load-path auto-install-directory)
(require 'auto-install)

;;;; {solidity-mode)
;; (el-get-bundle solidity-mode) だとうまくいかないので、auto-installで入れることにする
(auto-install-from-url "https://raw.githubusercontent.com/ttakezawa/emacs-solidity/master/solidity-mode.el")
(require 'flycheck)
(require 'solidity-mode)
(add-to-list 'flycheck-checkers 'solidity-checker)
(setq solidity-solc-path (executable-find "solc"))
(add-hook 'solidity-mode-hook
          '(lambda ()
             (set (make-local-variable 'c-basic-offset) 4)))

;;;; {swift-mode}
(el-get-bundle swift-mode)
(require 'swift-mode)
(add-to-list 'flycheck-checkers 'swift)
(setq flycheck-swift-sdk-path
      (replace-regexp-in-string
       "\n+$" "" (shell-command-to-string
                  "xcrun --show-sdk-path --sdk macosx")))
