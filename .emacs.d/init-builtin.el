;; Emacs標準機能による設定
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
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ; auto chmod +x
(global-set-key (kbd "C-c C-c") 'compile)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; replacing all symbolic links with their target names
(setq-default find-file-visit-truename t)

;; generic-x
(require 'generic-x)
(add-to-list 'auto-mode-alist '("/\\.ssh/config" . default-generic-mode))

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

;; extend kill-ring history
(setq kill-ring-max 500)

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
(xterm-mouse-mode 0)
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

;; Disable symbolic links that looks like .#foobar.go
(setq create-lockfiles nil)

;; 自動保存ファイル(#*#)の保存先を変更 (デフォルトだと/tmpでマシンが再起動したときに消えてしまう)
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file (format-time-string "backups/%Y_%m" (current-time))) t)))

;; Compact mode-line
;; Taken from https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(require 'cl)
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

;; See: https://github.com/vincentbernat/dot.emacs/blob/master/files.conf.el#L13
;; Let emacs open files with line and column number in it
(defadvice find-file (around find-file-line-number
                             (path &optional wildcards)
                             activate)
  "Turn files like file.js:14:10 into file.js and going to line 14, col 10."
  (save-match-data
    (let* ((match (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\):?$" path))
           (line-no (and match
                         (match-string 2 path)
                         (string-to-number (match-string 2 path))))
           (col-no (and match
                        (match-string 3 path)
                        (string-to-number (match-string 3 path))))
           (path (if match (match-string 1 path) path)))
      ad-do-it
      (when line-no
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-no))
        (when (> col-no 0)
          (forward-char (1- col-no)))))))

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

;; conf-unix-mode (builtin)
(add-to-list 'auto-mode-alist '("\\.service$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.aws/credentials" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.aws/config" . conf-unix-mode))

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
