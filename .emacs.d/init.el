;; -*- coding:utf-8 -*-

;;;;;;;;;;;;;;;; Begin [Configure builtin features] ;;;;;;;;;;;;;;;;
;; Emacs標準機能による設定

(package-initialize)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key [backspace] 'delete-backward-char)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-\\"))  ; disable toggle-input-method
(setq inhibit-startup-message t)
(setq-default frame-background-mode 'dark)
(load (setq custom-file (locate-user-emacs-file "custom.el")) t)
(setq ring-bell-function 'ignore) ; ignore bell
(windmove-default-keybindings)
(add-hook 'makefile-mode 'intent-tabs-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold (* 500 1024)) ;; 500KB
(show-paren-mode 1)
(column-number-mode t)
(global-auto-revert-mode 1)
(auto-image-file-mode +1)
(require 'generic-x)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ; auto chmod +x
(global-set-key (kbd "C-c C-c") 'compile)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; MacでGUIのときだけは特に邪魔にならないのでメニューバーを表示させる
(defun takezawa/setup-menu-bar-mode (&optional frame)
  "setup menu-bar-mode."
  (unless frame
    (setq frame (selected-frame)))
  (select-frame frame)
  (if (and (eq system-type 'darwin) window-system)
      (menu-bar-mode 1)
    (menu-bar-mode -1)))
(takezawa/setup-menu-bar-mode)
(add-hook 'after-make-frame-functions
          'takezawa/setup-menu-bar-mode)

;; server server for emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; desktop-save-mode (builtin)
(setq desktop-files-not-to-save "") ;; バッファを復元させない
(setq desktop-restore-frames nil)   ;; フレームを復元させない
(setq desktop-globals-to-save '(search-ring regexp-search-ring kill-ring))
(desktop-save-mode 1)

;; Automatically overriding stale locks
;; Taken from https://www.emacswiki.org/emacs/Desktop#toc4 and https://github.com/hjz/emacs/blob/master/config/desktop.el
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil."
  (when pid
    (= 0 (call-process "kill" nil nil nil "-0" (number-to-string pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

;; mouse
(xterm-mouse-mode)
(require 'mwheel)
(mouse-wheel-mode 1)
(global-unset-key [M-down-mouse-1]) ;; ターミナルで M-左クリック のとき文字が入力されてしまうのを回避

;; dired (C-x C-qでwdired)
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
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
(global-set-key (kbd "C-c C-i") 'indent-region)

;; recentf
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
      recentf-max-saved-items 2000
      recentf-auto-cleanup 10
      recentf-auto-save-timer (run-with-idle-timer 30 t '(lambda () (with-suppressed-message (recentf-save-list))))
      recentf-exclude '(".recentf" "custom.el"))
(recentf-mode 1)

;; suppress recentf-cleanup  ( Cleaning up the recentf list...done (0 removed) )
(defadvice recentf-cleanup (around recentf-cleanup-quiet activate)
  "suppress the output from `message' to minibuffer"
  (with-suppressed-message ad-do-it))

;; dashboard-modeを使ってない場合、起動直後にrecentf-open-filesを実行
(add-hook 'after-init-hook
          '(lambda()
             (when (and
                    (< (length command-line-args) 2)
                    (not (fboundp 'dashboard-mode)))
               (recentf-open-files))))

;; save place
(require 'saveplace)
(setq save-place-file (locate-user-emacs-file ".emacs-places"))
(save-place-mode 1)

;; バックアップファイル(*~)の保存先を変更
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file (format-time-string "backups/%Y_%m" (current-time))))))

;; 自動保存ファイル(#*#)の保存先を変更 (デフォルトだと/tmpでマシンが再起動したときに消えてしまう)
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file (format-time-string "backups/%Y_%m" (current-time))) t)))

;; Compact mode-line
;; Taken from https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(defvar mode-line-cleaner-alist
  `((emacs-lisp-mode . "El"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; macOSのクリップボードを活用させる ( Taken from https://gist.github.com/the-kenny/267162 )
(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

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
      (run-with-idle-timer 0.2 t 'global-hl-line-timer-function))

(set-face-attribute hl-line-face nil :background "gray9" :underline t)

;;;; whitespace-mode (builtin)

;; ファイル保存時に余分な空白を削除
(setq whitespace-action '(auto-cleanup))

;; non-printable character を別の文字に置き換えて表示する
;; 参考:
;;   - http://www.ecma-international.org/ecma-262/6.0/#sec-white-space
;;   - https://github.com/rails/rails/blob/v5.0.1/activesupport/lib/active_support/multibyte/unicode.rb#L38
(setq whitespace-display-mappings
      '((tab-mark ?\t [?» ?\t] [?\\ ?\t]) ;; タブを»を使って表示する

        ;; 以下のUnicode Whitespaceを半角スペースや□などに置き換えて表示する。
        ;; また、後のwhitespace-space face設定にて下線を付けて強調表示される。
        (space-mark ?\u000B [?\ ]) ; Vertical tab
        (space-mark ?\u000C [?\ ]) ; Form Feed
        (space-mark ?\u00A0 [?\ ]) ; NO-BREAK SPACE
        (space-mark ?\u2000 [?\ ]) ; EN QUAD
        (space-mark ?\u2001 [?\ ]) ; EM QUAD
        (space-mark ?\u2002 [?\ ]) ; EN SPACE
        (space-mark ?\u2003 [?\ ]) ; EM SPACE
        (space-mark ?\u2004 [?\ ]) ; THREE-PER-EM SPACE
        (space-mark ?\u2005 [?\ ]) ; FOUR-PER-EM SPACE
        (space-mark ?\u2006 [?\ ]) ; SIX-PER-EM SPACE
        (space-mark ?\u2007 [?\ ]) ; FIGURE SPACE
        (space-mark ?\u2008 [?\ ]) ; PUNCTUATION SPACE
        (space-mark ?\u2009 [?\ ]) ; THIN SPACE
        (space-mark ?\u200A [?\ ]) ; HAIR SPACE
        (space-mark ?\u200B [?\ ]) ; ZERO WIDTH SPACE
        (space-mark ?\u202F [?\ ]) ; NARROW NO-BREAK SPACE
        (space-mark ?\u205F [?\ ]) ; MEDIUM MATHEMATICAL SPACE
        (space-mark ?\u3000 [?□]) ; IDEOGRAPHIC SPACE - 全角スペース
        (space-mark ?\uFEFF [?B?O?M]) ; ZERO WIDTH NO-BREAK SPACE - BOM
        ))

;; 全角スペースや、Unicode Whitespaceをspaceのfaceで強調させる
(setq whitespace-space-regexp "\\([\u000B\u000C\u00A0\u2000-\u200B\u202F\u205F\u3000\uFEFF]+\\)")

(setq whitespace-style '(face       ; faceで可視化
                         trailing   ; 行末
                         tabs       ; タブ
                         spaces     ; スペース
                         empty      ; 先頭/末尾の空行
                         space-mark ; white-display-mappingsのspace-mark置換を適用させる
                         tab-mark   ; white-display-mappingsのtab-mark置換を適用させる
                         ))
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
(add-to-list 'mode-line-cleaner-alist '(eldoc-mode . "")) ;; Hide from mode-line

;; javascript-mode (builtin)
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
                 (modes  . '(js-mode)))))

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

;; $HOME/.[a-z]+env/ 以下は read-only-mode とする。伴ってflycheckも無効化される
(add-hook 'find-file-hook
          '(lambda ()
             (when (or
                    (string-match (concat "^" (file-name-as-directory (getenv "HOME")) "\.[a-z]+env") (buffer-file-name))
                    (string-match "^/usr" (buffer-file-name)))
               (read-only-mode 1))))

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
(global-set-key (kbd "C-c s") 'takezawa/window-resizer)
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

;;;;;;;;;;;;;;;; End [Configure builtin features] ;;;;;;;;;;;;;;;;

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
  (set-face-background 'default "black") ;; Use true black in background.
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
(global-set-key (kbd "C-c j") 'dumb-jump-go)
(global-set-key (kbd "M-*") 'dumb-jump-back)

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

;; find-fileのときC-iで選択するようにする
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)

;;;; {helm-ls-git}
(el-get-bundle helm-ls-git)
(global-set-key (kbd "C-x G") 'helm-ls-git-ls)

;;;; {projectile-mode}
(el-get-bundle projectile)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
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
(global-set-key (kbd "C-c p g") 'helm-projectile-ag)

;; Configure helm-for-files
(require 'helm-projectile)
(setq helm-for-files-preferred-list
      '(;; helm-source-buffers-list
        helm-source-ls-git-status
        helm-source-projectile-files-list
        helm-source-recentf
        ;; helm-source-bookmarks
        ;; helm-source-file-cache
        ;; helm-source-files-in-current-dir
        helm-source-locate))
(global-set-key (kbd "C-x C-r") 'helm-for-files)

;;;; {helm-swoop}
(el-get-bundle helm-swoop)
;; デフォルトでhelm-swoopで周辺3行も表示する つまり M-3 M-x helm-swoop と同じ
(defun takezawa/helm-swoop ()
  (interactive)
  (if current-prefix-arg
      (helm-swoop)
    (let ((current-prefix-arg '(3)))
      (helm-swoop))))
(global-set-key (kbd "M-i") 'takezawa/helm-swoop)

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
(setq ac-auto-start nil)

;;;; {ac-helm}
(el-get-bundle ac-helm)
(global-set-key (kbd "M-/") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "M-/") 'ac-complete-with-helm)

;;;; {yasnippet}
(el-get-bundle yasnippet)
(yas-global-mode 1)

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
;; ### Install godef for godef-jump, gocode for go-eldoc, godoc for godoc-at-point
;; $ go get -v -u -f github.com/rogpeppe/godef github.com/nsf/gocode golang.org/x/tools/cmd/godoc
;; ### To use Flycheck default checkers: http://www.flycheck.org/en/latest/languages.html#go
;; $ go get -v -u -v github.com/mdempsky/unconvert github.com/golang/lint/golint github.com/kisielk/errcheck
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
               (setq-local tab-width 4)
               (setq-local comment-auto-fill-only-comments t)
               (set-fill-column 90)
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
(setq flycheck-gometalinter-enable-linters
      '(
        ;; "aligncheck" ;; slow
        ;; "deadcode"   ;; slow
        "dupl"
        ;; "errcheck" ;; flycheck slow
        "gas"
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
(define-key rinari-minor-mode-map (kbd "C-c p") 'rinari-goto-partial)

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
