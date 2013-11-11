;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; ----------------------------------------------------------------------
;;; gnuserv
;;; ----------------------------------------------------------------------
(unless (require 'gnuserv-compat nil t)
  (require 'gnuserv nil t))
(when (fboundp 'server-start)
  (server-start)
  (setq gnuserv-frame (selected-frame)))

;;; ----------------------------------------------------------------------
;;; package.el
;;; ----------------------------------------------------------------------
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar installing-package-list
  '(
    auto-complete
    bm
    coffee-mode
    color-moccur
    color-theme
    csharp-mode
    cygwin-mount
    diminish
    dsvn
    elscreen
    gist
    git-commit-mode
    git-gutter
    git-gutter-fringe
    gtags
    js2-mode
    less-css-mode
    lua-mode
    magit
    mmm-mode
    php-mode
    popwin
    recentf-ext
    ruby-block
    ruby-electric
    scss-mode
    session
    shell-pop
    snippet
    undo-tree
    yaml-mode
    )
  "A list of packages to install by package.el at launch.")

(require 'cl)
(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;;; ----------------------------------------------------------------------
;;; el-get
;;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(defvar el-get-installing-package-list
  '(
    anything
    anything-c-moccur
    anything-howm
    anything-project
    dabbrev-highlight
    howm
    jaspace
    moccur-edit
    po-mode+
    visual-basic-mode
    emacs-rails
    open-junk-file
    lispxmp
    paredit
    auto-async-byte-compile
    )
  "A list of packages to install by el-get at launch.")

(el-get 'sync el-get-installing-package-list)

;;; ----------------------------------------------------------------------
;;; 基本設定
;;; ----------------------------------------------------------------------
(setq inhibit-startup-message t)
(setq scroll-step 2)
(setq next-line-add-newlines nil)
(setq kill-whole-line t)
(setq case-replace nil)
;;(setq major-mode 'text-mode)
(setq-default transient-mark-mode t)
(setq indent-line-function 'indent-relative-maybe)
(setq truncate-partial-width-windows nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(temp-buffer-resize-mode t)
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(tool-bar-mode -1)
(blink-cursor-mode 0)
(delete-selection-mode t)
(setq line-move-visual nil)
(setq mode-require-final-newline t)
(setq-default tab-width 4 indent-tabs-mode nil)
(show-paren-mode t)
(setq blink-matching-paren nil)
(setq confirm-kill-emacs nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq-default indicate-empty-lines t)

;; mode-line
(setq mode-line-frame-identification " ")
(setq line-number-mode t)
(setq column-number-mode t)

;; Automatically reload files after they've been modified (typically in Visual C++)
(global-auto-revert-mode 1)

;; 日本語環境設定
(set-language-environment "Japanese")

;; 規定の文字コードを UTF-8 に
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(if (not window-system)
    (set-terminal-coding-system 'utf-8-unix))
(set-buffer-file-coding-system 'utf-8-unix)
(if (eq window-system 'w32)
    (setq default-file-name-coding-system 'japanese-cp932-dos))

;; フォントの設定
;; (cond ((eq window-system 'x)
;;        (create-fontset-from-fontset-spec
;;         "-mplus-fixed-*-*-*--10-*-*-*-*-*-fontset-10,
;;          ascii:-mplus-gothic-*--10-*-iso8859-1,
;;          japanese-jisx0208:-mplus-gothic-*--10-*-jisx0208.1983-0,
;;          katakana-jisx0201:-mplus-gothic-*--10-*-jisx0201.1976-0")
;;        (create-fontset-from-fontset-spec
;;         "-mplus-fixed-*-*-*--12-*-*-*-*-*-fontset-12,
;;          ascii:-mplus-gothic-*--12-*-iso8859-1,
;;          japanese-jisx0208:-mplus-gothic-*--12-*-jisx0208.1983-0,
;;          katakana-jisx0201:-mplus-gothic-*--12-*-jisx0201.1976-0")
;;        (create-fontset-from-fontset-spec
;;         "-misc-fixed-*-*-*--14-*-*-*-*-*-fontset-14,
;;          ascii:-misc-fixed-*--14-*-iso8859-1,
;;          japanese-jisx0208:-misc-fixed-*--14-*-jisx0208.1983-0,
;;          katakana-jisx0201:-misc-fixed-*--14-*-jisx0201.1976-0")
;;        (create-fontset-from-fontset-spec
;;         "-misc-fixed-*-*-*--16-*-*-*-*-*-fontset-16,
;;          ascii:-*-fixed-*--16-*-iso8859-1,
;;          japanese-jisx0208:-*-fixed-*--16-*-jisx0208.1983-0,
;;          katakana-jisx0201:-*-fixed-*--16-*-jisx0201.1976-0")
;;        (create-fontset-from-fontset-spec
;;         "-misc-fixed-*-*-*--24-*-*-*-*-*-fontset-24,
;;          ascii:-*-fixed-*--24-*-iso8859-1,
;;          japanese-jisx0208:-*-fixed-*--24-*-jisx0208.1983-0,
;;          katakana-jisx0201:-*-fixed-*--24-*-jisx0201.1976-0")
;;        (set-default-font "fontset-12"))
;;       ((eq window-system 'w32)
;;        (create-fontset-from-ascii-font "Consolas-11:weight=normal:slant=normal" nil "consolasmeiryo")
;;        (set-fontset-font "fontset-consolasmeiryo" 'japanese-jisx0208 '("MeiryoKe_Console" . "jisx0208-sjis"))
;;        (set-fontset-font "fontset-consolasmeiryo" 'katakana-jisx0201 '("MeiryoKe_Console" . "jisx0201-katakana"))
;;        (add-to-list 'initial-frame-alist '(font . "fontset-consolasmeiryo"))
;;        ))
(add-to-list 'initial-frame-alist '(font . "Ricty-13"))
;(add-to-list 'initial-frame-alist '(alpha 100 90))
(setq default-frame-alist initial-frame-alist)

;; Cygwin, IME など環境固有の設定
(when (eq window-system 'w32)
  (setenv "CYGWIN" "nodosfilewarning")
  (setq default-input-method "W32-IME")
  (w32-ime-initialize)
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]")))

;;; ----------------------------------------------------------------------
;;; ibus.el
;;; ----------------------------------------------------------------------
;; (when (require 'ibus nil t)
;;   (add-hook 'after-init-hook 'ibus-mode-on)
;;   (global-set-key "\C-\\" 'ibus-toggle)
;;   (setq ibus-cursor-color '("red" "green" "cyan"))
;;   (ibus-define-common-key [?\C-\  ?\C-/]  nil)
;;   (add-hook 'minibuffer-setup-hook 'ibus-disable)
;;   (ibus-disable-isearch))

;;; ----------------------------------------------------------------------
;;; uim.el
;;; ----------------------------------------------------------------------
;; (when (not (fboundp 'ibus-mode-on))
;;   (when (require 'uim nil t)
;;     (setq uim-candidate-display-inline t)
;;     (global-set-key "\C-\\" 'uim-mode)))

;;; ----------------------------------------------------------------------
;;; mozc.el
;;; ----------------------------------------------------------------------
(when (require 'mozc nil t)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay))

(add-hook 'input-method-activate-hook
          (lambda() (set-cursor-color "red")))
(add-hook 'input-method-inactivate-hook
          (lambda() (set-cursor-color "green")))

;;; ----------------------------------------------------------------------
;;; ibuffer
;;; ----------------------------------------------------------------------
(when (require 'ibuffer nil t)
  (setq ibuffer-formats
        '((mark modified read-only " " (name 30 30)
                " " (size 6 -1) " " (mode 16 16) " " (coding 15 15) " " filename)
          (mark " " (name 30 -1) " " (coding 15 15) " " filename)))
  (define-ibuffer-column
   ;; ibuffer-formats に追加した文字
   coding
   ;; 一行目の文字
   (:name " coding ")
   ;; 以下に文字コードを返す関数を書く
   (if (coding-system-get buffer-file-coding-system 'mime-charset)
       (format " %s" (coding-system-get buffer-file-coding-system 'mime-charset))
     " undefined"
     ))
  (global-set-key "\C-x\C-b" 'ibuffer))

;;; ----------------------------------------------------------------------
;;; imenu
;;; ----------------------------------------------------------------------
(require 'imenu)

;;; ----------------------------------------------------------------------
;;; abbrev/dabbrev
;;; ----------------------------------------------------------------------
(setq save-abbrevs t)
(setq abbrev-file-name (expand-file-name "~/.emacs.d/.abbrev_defs"))
(quietly-read-abbrev-file)
(add-hook 'pre-command-hook
          (lambda ()
            (setq abbrev-mode nil)))
(setq dabbrev-abbrev-skip-leading-regexp "[:@$]")

;;; ----------------------------------------------------------------------
;;; dabbrev-highlight
;;; ----------------------------------------------------------------------
(require 'dabbrev-highlight)

;;; ----------------------------------------------------------------------
;;; hippie-expand
;;; ----------------------------------------------------------------------
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
(define-key esc-map  "/" 'hippie-expand) ;; M-/

;; 光る hippie-expand
(defvar he-dabbrev-highlight-function "")
(let (current-load-list)
  (defadvice try-expand-dabbrev
    (after dabbrev-expand-highlight activate)
    "Advised by he-dabbrev-highlight.
Highlight last expanded string."
    (setq he-dabbrev-highlight-function "dabbrev")
    (he-dabbrev-highlight))

  (defadvice try-expand-dabbrev-all-buffers
    (after dabbrev-expand-highlight activate)
      "Advised by he-dabbrev-highlight.
Highlight last expanded string."
      (setq he-dabbrev-highlight-function "dabbrev-all-buffers")
      (he-dabbrev-highlight))

  (defadvice try-expand-migemo
    (after dabbrev-expand-highlight activate)
    "Advised by he-dabbrev-highlight.
Highlight last expanded string."
    (setq he-dabbrev-highlight-function "migemo")
    (he-dabbrev-highlight)))

(defun he-dabbrev-highlight ()
  (when ad-return-value
    (let ((start (marker-position he-search-loc))
          (len (length (car he-tried-table)))
          (buf (marker-buffer he-search-loc))
          (cbuf (current-buffer))
          end wait)
      (save-selected-window
        (save-excursion
          (if (eq buf cbuf)
              (if (> start (point))
                  (setq end start
                        start (- end len))
                (setq end (+ start len)))
            (set-buffer buf)
            (setq end start
                  start (- end len)))
          (if (and (get-buffer-window buf)
                   (select-window (get-buffer-window buf))
                   (pos-visible-in-window-p start)
                   (pos-visible-in-window-p end))
              (progn
                ;; Highlight the string used for the last expansion.
                (if dabbrev-highlight-overlay
                    (move-overlay dabbrev-highlight-overlay start end)
                  (setq dabbrev-highlight-overlay (make-overlay start end)))
                  (overlay-put dabbrev-highlight-overlay
                               'face dabbrev-highlight-face)
                  (add-hook 'pre-command-hook 'dabbrev-highlight-done))
            (unless (minibufferp cbuf)
              ;; Display one-line summary in minibuffer.
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char start)
                  (let ((str (buffer-substring-no-properties start end))
                          (bol (progn (forward-line 0) (point)))
                          (eol (progn (end-of-line) (point))))
                    (if (or (featurep 'xemacs)
                            (<= emacs-major-version 20))
                        (setq str (concat " *" str "* "))
                        (put-text-property 0 (length str)
                                           'face dabbrev-highlight-face str)
                        (put-text-property 0 (length he-dabbrev-highlight-function)
                                           'face 'bold he-dabbrev-highlight-function))
                    (message "%s: %s(%d): %s%s%s"
                             (format "Using %s" he-dabbrev-highlight-function)
                             (buffer-name buf)
                             (count-lines (point-min) start)
                             (buffer-substring-no-properties bol start)
                             str
                             (buffer-substring-no-properties end eol))
                    (setq wait t))))))))
      (when wait
        (let ((inhibit-quit t))
          (sit-for 10)
          (when quit-flag
            (setq quit-flag nil)
            (setq unread-command-events '(7))))))))

;;; ----------------------------------------------------------------------
;;; auto-complete
;;; ----------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'html-mode)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-auto-start 2)
(setq ac-dwim t)
(setq-default ac-sources '(ac-source-dictionary
                           ac-source-words-in-same-mode-buffers
                           ac-source-files-in-current-dir))
(set-face-attribute 'ac-completion-face nil
                    :foreground "yellow" :underline t)
(set-face-attribute 'ac-candidate-face nil
                    :background "darkgray" :underline nil)
(set-face-attribute 'ac-selection-face nil
                    :background "steelblue")
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;;; ----------------------------------------------------------------------
;;; font-lock
;;; ----------------------------------------------------------------------
(setq font-lock-support-mode
      (if (fboundp 'jit-lock-mode) 'jit-lock-mode 'lazy-lock-mode))
(global-font-lock-mode t)

;;; ----------------------------------------------------------------------
;;; color-theme
;;; ----------------------------------------------------------------------
(cond (window-system
       (require 'color-theme)
       (load "my-color-theme")
       (my-color-theme)))

;;; ----------------------------------------------------------------------
;;; diff-mode で文字単位での強調表示を行う
;;; ----------------------------------------------------------------------
(add-hook 'diff-mode-hook
          '(lambda ()
             (diff-auto-refine-mode t)))

;;; ----------------------------------------------------------------------
;;; windmove
;;; ----------------------------------------------------------------------
(windmove-default-keybindings)

;;; ----------------------------------------------------------------------
;;; pc-bufsw
;;; ----------------------------------------------------------------------
(require 'pc-bufsw)
(pc-bufsw::bind-keys (quote [C-tab]) (quote [C-S-tab]))

;;; ----------------------------------------------------------------------
;;; emacs-w3m と browse-url の設定
;;; ----------------------------------------------------------------------
(setq w3m-type 'w3m-ja)
(setq w3m-use-cookies t)
(setq w3m-accept-japanese-characters t)
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(autoload 'w3m-browse-url "w3m" "Ask emacs-w3m to show a URL." t)
(require 'browse-url)
(cond ((eq window-system 'x)
       (setq browse-url-browser-function 'browse-url-xdg-open)
       (global-set-key [mouse-3] 'browse-url-at-mouse))
      ((eq window-system 'w32)
       (setq browse-url-browser-function 'browse-url-default-windows-browser)
       (global-set-key [mouse-3] 'browse-url-at-mouse))
      (t
       (setq browse-url-browser-function 'w3m-browse-url)))
(global-set-key "\C-xm" 'browse-url-at-point)

;;; ----------------------------------------------------------------------
;; browse-kill-ring
;;; ----------------------------------------------------------------------
;; (require 'browse-kill-ring)
;;  (browse-kill-ring-default-keybindings)
;;  (setq browse-kill-ring-no-duplicates t)
;;  (setq browse-kill-ring-separator "--ヽ(´ー｀)ノ--------------------")
;;  (setq browse-kill-ring-separator-face 'browse-kill-ring-separator-face)
;;  (make-face 'browse-kill-ring-separator-face)
;;  (set-face-attribute 'browse-kill-ring-separator-face nil
;;              :foreground "light steel blue" :bold t)

;;; ----------------------------------------------------------------------
;;; undo-tree.el
;;; ----------------------------------------------------------------------
(global-undo-tree-mode)
(define-key global-map [?\C-.] 'undo-tree-redo)

;;; ----------------------------------------------------------------------
;;; migemo
;;; ----------------------------------------------------------------------
(when (require 'migemo nil t)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (cond ((eq window-system 'w32)
         (setq migemo-dictionary "../etc/migemo/migemo-dict")
         (setq migemo-coding-system 'japanese-shift-jis-unix))
        (t
         (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
         (setq migemo-coding-system 'utf-8-unix)))
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  (migemo-init))

;;; ----------------------------------------------------------------------
;;; dired
;;; ----------------------------------------------------------------------
(require 'dired-x)
(require 'jka-compr)
(when (require 'wdired nil t)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;;; ----------------------------------------------------------------------
;;; howm
;;; ----------------------------------------------------------------------
(setq howm-compatible-to-ver1dot3 t)
(setq howm-directory "~/Dropbox/Documents/howm/")
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(mapc
 (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-grep howm-create
             howm-keyword-to-kill-ring))
;; メモは UTF-8
(add-to-list 'auto-coding-alist '("\\.howm\\'" . utf-8-unix))
(setq howm-process-coding-system 'utf-8)
(add-hook 'howm-create-file-hook
          (lambda ()
            (set-buffer-file-coding-system 'utf-8)))
;; 「最近のメモ」一覧時にタイトル表示
(setq howm-list-recent-title t)
;; 全メモ一覧時にタイトル表示
(setq howm-list-all-title t)
;; メニューを 2 時間キャッシュ
(setq howm-menu-expiry-hours 2)
;; howm の時は auto-fill で
;; (add-hook 'howm-mode-on-hook 'auto-fill-mode)
;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)

;; 検索しないファイルの正規表現
(setq
 howm-excluded-file-regexp
 "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

;; いちいち消すのも面倒なので
;; 内容が 0 ならファイルごと削除する
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
;; C-cC-c で保存してバッファをキルする
(defun my-save-and-kill-buffer-howm ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match (expand-file-name howm-directory)
                       (expand-file-name buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer-howm)))

;; 日付けの入力が面倒
(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map
       "\C-m" 'my-insert-day)
     (defun my-insert-day ()
       (interactive)
       (let ((day nil)
             (calendar-date-display-form
         '("[" year "-" (format "%02d" (string-to-int month))
           "-" (format "%02d" (string-to-int day)) "]")))
         (setq day (calendar-date-string
                    (calendar-cursor-to-date t)))
         (exit-calendar)
         (insert day)))))

;;; ----------------------------------------------------------------------
;;; cc-mode
;;; ----------------------------------------------------------------------
(require 'cc-mode)
(defconst my-cc-style
  '(
    ;; インデント幅を空白4コ分にする
    (c-basic-offset . 4)
    ;; tab キーでインデントを実行
    (c-tab-always-indent        . t)
    ;; コメントだけの行のインデント幅
    (c-comment-only-line-offset . 0)

    ;; カッコ前後の自動改行処理の設定
    (c-hanging-braces-alist
     . (
        (class-open before after)       ; クラス宣言の'{'の前後
        (class-close before)            ; クラス宣言の'}'の前
        (defun-open before after)       ; 関数宣言の'{'の前後
        (defun-close before after)      ; 関数宣言の'}'の前後
        ;;(inline-open after)           ; クラス内のインライン
                                        ; 関数宣言の'{'の後
        (inline-close after)            ; クラス内のインライン
                                        ; 関数宣言の'}'の後
        (brace-list-open after)         ; 列挙型、配列宣言の'{'の後
        (brace-list-close before)       ; 列挙型、配列宣言の'}'の前
        (block-open after)              ; ステートメントの'{'の後
        (block-close . c-snug-do-while) ; ステートメントの'}'前
        (substatement-open after)       ; サブステートメント
                                        ; (if 文等)の'{'の後
        (statement-case-open after)     ; case 文の'{'の後
        (extern-lang-open before after) ; 他言語へのリンケージ宣言の
                                        ; '{'の前後
        (extern-lang-close before)      ; 他言語へのリンケージ宣言の
                                        ; '}'の前
        (namespace-open before after)   ; 名前空間宣言の'{'の前後
        (namespace-close before)        ; 名前空間宣言の'}'の前
                                        ;(arglist-cont-noempty after)
                                        ;(statement-cont after)
        ))

    ;; コロン前後の自動改行処理の設定
    (c-hanging-colons-alist
     . (
        (case-label after)              ; case ラベルの':'の後
        (label after)                   ; ラベルの':'の後
        (access-label after)            ; アクセスラベル(public等)の':'の後
        (member-init-intro after)       ; コンストラクタでのメンバー初期化
                                        ; リストの先頭の':'の後
        ;;(inher-intro before)          ; クラス宣言での継承リストの先頭の
                                        ; ':'では改行しない
        ))

    ;; 挿入された余計な空白文字のキャンセル条件の設定
    ;; 下記の*を削除する
    (c-cleanup-list . (
                       brace-else-brace ; else の直前
                                        ; "} * else {"  ->  "} else {"
                       brace-elseif-brace ; else if の直前
                                        ; "} * else if {"  ->  "} else {"
                       brace-catch-brace ; catch の直前
                                        ; "} * catch ("  ->  "} catch ("
                       empty-defun-braces ; else if の直前
                                        ; "} * else if (.*) {"
                                        ; ->  } "else if (.*) {"
                       defun-close-semi ; クラス・関数定義後の';' の直前
                                        ; "} * ;"  ->  "};"
                       list-close-comma ; 配列初期化時の'},'の直前
                                        ; "} * ,"  ->  "},"
                       scope-operator   ; スコープ演算子'::' の間
                                        ; ": * :"  ->  "::"
                       ))

    ;; オフセット量の設定
    ;; 必要部分のみ抜粋(他の設定に付いては info 参照)
    ;; オフセット量は下記で指定
    ;; +  c-basic-offsetの 1倍, ++ c-basic-offsetの 2倍
    ;; -  c-basic-offsetの-1倍, -- c-basic-offsetの-2倍
    (c-offsets-alist
     . (
        (arglist-intro          . ++)   ; 引数リストの開始行
        (arglist-close          . c-lineup-arglist) ; 引数リストの終了行
        (substatement-open      . 0)    ; サブステートメントの開始行
        (statement-case-open    . +)    ; case 文の後の '{'
        (statement-cont         . ++)   ; ステートメントの継続行
        (case-label             . 0)    ; case 文のラベル行
        (label                  . 0)    ; ラベル行
        (block-open             . 0)    ; ブロックの開始行
        (inline-open            . 0)    ; クラス内のインラインメソッドを開始する中括弧
        (member-init-intro      . ++)   ; （構造体の）メンバ初期化リストの最初の行
        ))

    ;; インデント時に構文解析情報を表示する
    (c-echo-syntactic-information-p . t)
    )
  "My C/C++ Programming Style")

(add-hook 'c-mode-common-hook
      (lambda ()
        ;; my-c-stye を登録して有効にする
        (c-add-style "PERSONAL" my-cc-style t)
        ;; タブ長の設定
        (setq tab-width 4)
        ;; タブの代わりにスペースを使う
        ;(setq indent-tabs-mode nil)
        ;; 自動改行(auto-newline)を有効にする
        (when (fboundp 'c-toggle-auto-newline)
          (c-toggle-auto-newline t))
        ;; 連続する空白の一括削除(hungry-delete)を有効にする
        ;(c-toggle-hungry-state t)
        ;; セミコロンで自動改行しない
        (setq c-hanging-semi&comma-criteria nil)
        ;; カーソルに追従して水平スクロール
        ;; (setq truncate-lines t)
        ;; コンパイルコマンドの設定
        (setq compile-command "make -k" )     ; Cygwin の make
        ;; (setq compile-command "nmake /NOLOGO /S") ; VC++ の nmake
        (setq compilation-window-height 16)))

(define-key c-mode-base-map "\r" 'newline-and-indent)
(define-key c-mode-base-map "\C-cc" 'compile)
(define-key c-mode-base-map "\C-h" 'c-electric-backspace)
(define-key c-mode-base-map "\C-xt" 'ff-find-other-file)
(define-key c-mode-base-map [mouse-2] 'ff-mouse-find-other-file)
(setq auto-mode-alist
      (append '(("\\.C$" . c-mode)
                ("\\.[Hh]$" . c++-mode)
                ("\\.[Hh][Pp][Pp]$" . c++-mode))
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; hideshow
;;; ----------------------------------------------------------------------
(when (locate-library "hideshow")
  (require 'hideshow)
  (add-hook 'c-mode-common-hook   'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook       'hs-minor-mode)
  (add-hook 'lisp-mode-hook       'hs-minor-mode)
  (add-hook 'perl-mode-hook       'hs-minor-mode)
  (add-hook 'sh-mode-hook         'hs-minor-mode)
  (defun ruby-custom-setup ()
    (add-to-list 'hs-special-modes-alist
                 '(ruby-mode
                   "\\(def\\|do\\)"
                   "end"
                   "#"
                   (lambda (arg) (ruby-end-of-block)) nil ))
    (hs-minor-mode t))
  (add-hook 'ruby-mode-hook 'ruby-custom-setup))

;;; ----------------------------------------------------------------------
;;; gtags
;;; ----------------------------------------------------------------------
;; (add-hook 'gtags-mode-hook
;;           '(lambda ()
;;              (local-set-key "\M-t" 'gtags-find-tag)
;;              (local-set-key "\M-r" 'gtags-find-rtag)
;;              (local-set-key "\M-s" 'gtags-find-symbol)
;;              (local-set-key "\C-t" 'gtags-pop-stack)
;;              (define-key gtags-mode-map [mouse-3] nil)
;;              (define-key gtags-select-mode-map [mouse-3] nil)))
;; (add-hook 'c-mode-common-hook
;;           '(lambda()
;;              (gtags-mode 1)
;;              ;;(gtags-make-complete-list)
;;              ))

;;; ----------------------------------------------------------------------
;;; color-moccur
;;; ----------------------------------------------------------------------
(when (require 'color-moccur nil t)
  (require 'moccur-edit)
  (setq moccur-split-word t) ;スペース区切りでAND検索
  (when (require 'migemo nil t)
    (setq moccur-use-migemo t))
  (setq *moccur-buffer-name-exclusion-list*
        '(".+TAGS.+" "\.svn" "*Completions*" "*Messages*" " *migemo*"))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map "O" 'dired-do-moccur)))
  (define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)
  (global-set-key "\M-o" 'occur-by-moccur)
  (global-set-key "\C-c\C-x\C-o" 'moccur))

;;; ----------------------------------------------------------------------
;;; dsvn
;;; ----------------------------------------------------------------------
(when (locate-library "dsvn")
  (autoload 'svn-status "dsvn" "Run `svn status'." t)
  (autoload 'svn-update "dsvn" "Run `svn update'." t)
  (setq svn-status-hide-unmodified t)
  (setq process-coding-system-alist
        (cons '("svn" . utf-8) process-coding-system-alist)))


;;; ----------------------------------------------------------------------
;;; magit
;;; ----------------------------------------------------------------------
(global-set-key "\C-xg" 'magit-status)

;;; ----------------------------------------------------------------------
;;; ruby-mode
;;; ----------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("config\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Rake\\|Cap\\|Gem\\|Guard\\)file$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; (autoload 'rubydb "rubydb3x" "Run rubydb on program FILE in buffer *gud-FILE*.
;; The directory containing FILE becomes the initial working directory
;; and source-file directory for your debugger.")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (ruby-electric-mode t)
             ;; (define-key ruby-mode-map "\C-cd" 'rubydb)
             (define-key ruby-mode-map (kbd "RET") nil) ; unset ruby-electric-return
             (define-key ruby-mode-map (kbd "C-c C-c") nil) ; emacs-rails prefix key
             ))

;;; ruby-mode のインデントをいい感じにする
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

(defun ruby-insert-magic-comment-if-needed ()
  "バッファのcoding-systemをもとにmagic commentをつける。"
  (when (and
         (and (eq major-mode 'ruby-mode)
              (or (not mmm-primary-mode)
                  (eq mmm-primary-mode 'ruby-mode)))
         (find-multibyte-characters (point-min) (point-max) 1))
    (save-excursion
      (goto-char 1)
      (when (looking-at "^#!")
        (forward-line 1))
      (if (re-search-forward "^#.+coding" (point-at-eol) t)
          (delete-region (point-at-bol) (point-at-eol))
        (open-line 1))
      (let* ((coding-system (symbol-name buffer-file-coding-system))
             (encoding (cond ((string-match "japanese-iso-8bit\\|euc-j" coding-system)
                              "euc-jp")
                             ((string-match "shift.jis\\|sjis\\|cp932" coding-system)
                              "shift_jis")
                             ((string-match "utf-8" coding-system)
                              "utf-8"))))
        (insert (format "# -*- coding: %s -*-" encoding))))))
(add-hook 'before-save-hook 'ruby-insert-magic-comment-if-needed)

;;; rd-mode
(when (locate-library "rd-mode")
  (autoload 'rd-mode "rd-mode" "major mode for ruby document formatter RD" t)
  (add-to-list 'auto-mode-alist '("\\.rd$" . rd-mode)))

;;; ----------------------------------------------------------------------
;;; emacs-rails
;;; ----------------------------------------------------------------------
(custom-set-variables '(rails-minor-mode-local-prefix-key "C-c"))
(custom-set-variables '(rails-minor-mode-global-prefix-key "C-c C-c"))
(require 'rails)
(setq rails-indent-and-complete nil)
(define-keys rails-minor-mode-map
  ("\C-c\C-p"            'rails-lib:run-primary-switch)
  ("\C-c\C-n"            'rails-lib:run-secondary-switch)
  ([?\C-.]               'redo))
(define-keys rails-view-minor-mode-map
  ("\C-c\C-cp"           'rails-view-minor-mode:create-partial-from-selection))

;;; ----------------------------------------------------------------------
;;; snippet.el
;;; ----------------------------------------------------------------------
(require 'snippet)

;;; ----------------------------------------------------------------------
;;; python-mode
;;; ----------------------------------------------------------------------
(setq py-indent-offset 4)
(add-to-list 'auto-mode-alist '("\\.pyw$" . python-mode))

;;; ----------------------------------------------------------------------
;;; cperl-mode
;;; ----------------------------------------------------------------------
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            ;;(setq indent-tabs-mode nil)
            (setq cperl-indent-level 4)
            (setq cperl-indent-tabs-mode nil)
            (setq cperl-continued-statement-offset 4)
            (setq cperl-comment-column 40)
            (setq cperl-close-paren-offset -4)
            (setq cperl-indent-parens-as-block t)
            (setq cperl-invalid-face nil)
            ;(setq cperl-electric-parens t)
            (setq cperl-auto-newline t)
            ;; face の設定
            (set-face-bold-p 'cperl-array-face nil)
            (set-face-underline-p 'cperl-array-face t)
            (set-face-background 'cperl-array-face nil)
            (set-face-bold-p 'cperl-hash-face nil)
            (set-face-italic-p 'cperl-hash-face nil)
            (set-face-underline-p 'cperl-hash-face t)
            (set-face-background 'cperl-hash-face nil)
            ))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

;;; ----------------------------------------------------------------------
;;; yaml-mode
;;; ----------------------------------------------------------------------
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; ----------------------------------------------------------------------
;;; visual-basic-mode
;;; ----------------------------------------------------------------------
(autoload 'visual-basic-mode "visual-basic-mode" "Basic Editing Mode" t)
(autoload 'vbp-mode "vbp-mode" "VBP mode." t)
(setq visual-basic-mode-indent 4)
(setq auto-mode-alist
      (append '(("\\.[Ff][Rr][Mm]$" . visual-basic-mode)  ;;Form Module
        ("\\.[Bb][Aa][Ss]$" . visual-basic-mode)  ;;Bas Module
        ("\\.[Cc][Ll][Ss]$" . visual-basic-mode)  ;;Class Module
        ("\\.[Vv][Bb][Ss]$" . visual-basic-mode)  ;;VBScript file
        ("\\.[Vv][Bb][Pp]$" . vbp-mode)
        ("\\.[Vv][Bb][Gg]$" . vbp-mode))
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; php-mode
;;; ----------------------------------------------------------------------
(setq php-mode-force-pear t)
(add-hook 'php-mode-hook
          '(lambda ()
             (define-key php-mode-map '[(control .)] nil)
             (define-key php-mode-map '[(control c)(control .)] 'php-show-arglist)))

;;; ----------------------------------------------------------------------
;;; js2-mode (javascript)
;;; ----------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

;;; ----------------------------------------------------------------------
;;; less-css-mode / scss-mode
;;; ----------------------------------------------------------------------
(setq less-css-compile-at-save nil)
(setq scss-compile-at-save nil)

;;; ----------------------------------------------------------------------
;;; csharp-mode
;;; ----------------------------------------------------------------------
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;;; ----------------------------------------------------------------------
;;; po-mode+
;;; ----------------------------------------------------------------------
(autoload 'po-mode "po-mode+"
  "Major mode for translators to edit PO files" t)
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)

;;; ----------------------------------------------------------------------
;;; mmm-mode
;;; ----------------------------------------------------------------------
(when (require 'mmm-mode nil t)
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 2)
  (setq mmm-parse-when-idle t)
  ;; 非 GUI 端末の場合
  (if (not window-system)
      (progn
    (set-face-background 'mmm-default-submode-face nil)
    (set-face-bold-p 'mmm-default-submode-face t)
    (set-face-background 'mmm-comment-submode-face nil)
    (set-face-bold-p 'mmm-comment-submode-face t)
    ))
  (mmm-add-classes
   '((mmm-html-css-mode
      :submode css-mode
      :front "<style[^>]*>\\([^<]*<!--\\)?\n"
      :back "\\(\\s-*-->\\)?\n[ \t]*</style>"
      )
     (mmm-html-javascript-mode
      :submode js2-mode
      :front "<script[^>]*>"
      :back "</script>")
     (mmm-jsp-mode
      :submode java-mode
      :front "<%"
      :back "%>"
      :insert ((?c mmm-jsp-mode nil @ "<%" @ " " _ " " @ "%>" @)
               (?e mmm-jsp-mode nil @ "<%=" @ " " _ " " @ "%>" @)))
     (mmm-eruby-mode
      :submode ruby-mode
      :front "<%"
      :back "%>"
      :insert ((?c mmm-eruby-mode nil @ "<%" @ " " _ " " @ "%>" @)
               (?e mmm-eruby-mode nil @ "<%=" @ " " _ " " @ "%>" @)))))
  (mmm-add-mode-ext-class 'html-mode nil 'mmm-html-css-mode)
  (mmm-add-mode-ext-class 'html-mode nil 'mmm-html-javascript-mode)
  (mmm-add-mode-ext-class nil "\\.html\\.erb?\\'" 'mmm-eruby-mode)
  (mmm-add-mode-ext-class nil "\\.rhtml?\\'" 'mmm-eruby-mode))

;;; ----------------------------------------------------------------------
;;; latex-mode
;;; ----------------------------------------------------------------------
(add-hook 'latex-mode-hook
          '(lambda ()
             (setq tex-verbatim-face nil)
             (defun tex-font-lock-suscript () nil)))

;;; ----------------------------------------------------------------------
;;; その他の拡張子に対応する編集モードを設定
;;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ("\\.doc$"               . text-mode)
                ("\\.[Hh][Tt][Mm][Ll]?$" . html-mode)     ;; HTML Document
                ("\\.[Aa][Ss][PpAa]$"    . html-mode)     ;; Active Server Page
                ("\\.[Tt][Pp][Ll]?$"     . html-mode)     ;; Smarty Template
                ("\\.[Jj][Ss][PpAa]$"    . html-mode)     ;; Java Server Pages
                ("\\.[ch]java$"          . java-mode)     ;; i-appli
                ("\\.html\\.erb$"        . html-mode)     ;; HTML(erb)
                ("\\.rhtml$"             . html-mode)     ;; HTML(erb)
                ("\\.text\\.erb$"        . text-mode)     ;; Text(erb)
                ("\\.rtext$"             . text-mode)     ;; Text(erb)
                )
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; #!shebang に対応する編集モードを設定
;;; ----------------------------------------------------------------------
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;;; ----------------------------------------------------------------------
;;; ChangeLog 用の設定
;;; ----------------------------------------------------------------------
(setq user-full-name "YAMAGUCHI, Seiji")
(setq user-mail-address "valda@underscore.jp")

;;; ----------------------------------------------------------------------
;;; テンプレートの自動挿入
;;; ----------------------------------------------------------------------
(setq auto-insert-directory (expand-file-name "~/.emacs.d/insert"))
;;(add-hook 'find-file-hooks 'auto-insert)

;;; ----------------------------------------------------------------------
;;; ~のつくバックアップファイルの保存場所の指定
;;; ----------------------------------------------------------------------
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
             (cons "\\.*$" (expand-file-name "~/bak")))

;;; ----------------------------------------------------------------------
;;; filecache
;;; ----------------------------------------------------------------------
(require 'filecache)
(file-cache-add-directory-list
   (list "~"
         "~/.ssh"
         "~/.emacs.d"
         "~/.emacs.d/elisp"
         "~/bin"
         "~/Dropbox"
         "~/chrome/SubScript"))
(when (eq window-system 'w32)
  (file-cache-add-directory-list
   (list "c:/Libraries/WTL80/include"
         "c:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/atlmfc/include")))

;;; ----------------------------------------------------------------------
;;; recentf-ext
;;; ----------------------------------------------------------------------
(require 'recentf-ext)
(setq recentf-max-saved-items 10000)

;;; ----------------------------------------------------------------------
;;; session.el
;;; ----------------------------------------------------------------------
(setq history-length t)
(setq session-initialize '(de-saveplace session keys menus places)
      session-globals-include '((kill-ring 100)
                                (session-file-alist 100 t)
                                (file-name-history 1000)))
(setq session-globals-max-string 10000000)
(setq session-save-print-spec '(t nil nil)) ; anything/helmと一緒に使うために必要
(add-hook 'after-init-hook 'session-initialize)

;;; ----------------------------------------------------------------------
;;; elscreen
;;; ----------------------------------------------------------------------
(when (require 'elscreen nil t)
  (setq elscreen-display-tab nil)
  (cond (window-system
         (elscreen-set-prefix-key "\C-z")
         (define-key elscreen-map "\C-z" 'elscreen-toggle)
         (define-key elscreen-map "z" 'iconify-frame))
        (t
         (elscreen-set-prefix-key "\C-t")))
  (elscreen-start))

;;; ----------------------------------------------------------------------
;;; flymake
;;; ----------------------------------------------------------------------
(when (require 'flymake nil t)
  (defun flymake-ruby-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "ruby" (list "-c" local-file))))
  (push '(".+\\.r[bu]$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("\\(Rake\\|Cap\\|Gem\\|Guard\\)file$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               ;; Don't want flymake mode for ruby regions in rhtml files
               (if (not (null buffer-file-name)) (flymake-mode))))
  (defun flymake-perl-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "perl" (list "-wc" local-file))))
  (push '(".+\\.p[lm]$\\|.+\\.t$" flymake-perl-init) flymake-allowed-file-name-masks)
  (push '("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1) flymake-err-line-patterns)
  (add-hook 'cperl-mode-hook
            '(lambda ()
               (if (not (null buffer-file-name)) (flymake-mode))))
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  (add-hook 'python-mode-hook
             '(lambda ()
                (if (not (null buffer-file-name)) (flymake-mode)))))

;;; ----------------------------------------------------------------------
;;; scratch バッファを消さないようにする
;;; ----------------------------------------------------------------------
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

;;; ----------------------------------------------------------------------
;; kill-ring に同じ内容の文字列を複数入れない
;;; ----------------------------------------------------------------------
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))


;;; ----------------------------------------------------------------------
;;; bm
;;; ----------------------------------------------------------------------
(when (require 'bm nil t)
  (setq-default bm-buffer-persistence t)
  (setq bm-repository-file "~/.emacs.d/.bm-repository")
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'auto-save-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  ;;   M$ Visual Studio key setup.
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<f2>")   'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous))

;;; ----------------------------------------------------------------------
;;; uniquify
;;; ----------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; ----------------------------------------------------------------------
;;; sdic
;;; ----------------------------------------------------------------------
(when (require 'sdic nil t)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (global-set-key "\C-cW" 'sdic-describe-word-at-point)
  (setq sdic-eiwa-dictionary-list
        '((sdicf-client "~/dict/eijiro52u.sdic")))
  (setq sdic-waei-dictionary-list
        '((sdicf-client "~/dict/waeiji52u.sdic"
                        (add-keys-to-headword t))))
  (setq sdic-default-coding-system 'utf-8-unix)

  ;; sdic-display-buffer 書き換え
  (defadvice sdic-display-buffer (around sdic-display-buffer-normalize activate)
    "sdic のバッファ表示を普通にする。"
    (setq ad-return-value (buffer-size))
    (let ((p (or (ad-get-arg 0)
                 (point))))
      (and sdic-warning-hidden-entry
              (> p (point-min))
              (message "この前にもエントリがあります。"))
      (goto-char p)
      (display-buffer (get-buffer sdic-buffer-name))
      (set-window-start (get-buffer-window sdic-buffer-name) p)))

  (defadvice sdic-other-window (around sdic-other-normalize activate)
    "sdic のバッファ移動を普通にする。"
    (other-window 1))

  (defadvice sdic-close-window (around sdic-close-normalize activate)
    "sdic のバッファクローズを普通にする。"
    (bury-buffer sdic-buffer-name))
  )

;;; ----------------------------------------------------------------------
;;; anything.el
;;; ----------------------------------------------------------------------
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

(setq anything-idle-delay 0.3)
(setq anything-input-idle-delay 0.1)
(setq anything-candidate-number-limit 100)
(when window-system
  (set-face-attribute 'anything-file-name nil
                      :foreground "white" :background nil)
  (set-face-attribute 'anything-dir-priv nil
                      :foreground "LightSkyBlue" :background nil))

(defun my-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers+
     anything-c-source-elscreen
     anything-c-source-files-in-current-dir+
     anything-c-source-bm
     anything-c-source-recentf
     anything-c-source-file-cache)
   " *my-anything*"))

(global-set-key (if window-system (kbd "C-;") "\C-x;") 'my-anything)
(global-set-key "\M-x" 'anything-M-x)
(global-set-key "\C-xb" 'anything-buffers+)
(global-set-key "\M-y" 'anything-show-kill-ring)

;;; anything-gtags
;; (require 'anything-gtags)
;; (add-hook 'gtags-mode-hook
;;           '(lambda ()
;;                (local-set-key "\M-\C-t" 'anything-gtags-select)))

;;; anything-c-moccur
(require 'anything-c-moccur)
;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.3 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;;; anything-project
(require 'anything-project)
(global-set-key (kbd "\C-xf") 'anything-project)

;;; ----------------------------------------------------------------------
;;; gist
;;; ----------------------------------------------------------------------
(require 'gist)

;;; ----------------------------------------------------------------------
;;; popwin.el
;;; ----------------------------------------------------------------------
(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq anything-samewindow nil)
  (setq popwin:special-display-config
        (append '(("*Backtrace*" :height 20)
                  ("*Kill Ring*" :height 20 :noselect t)
                  ("*Apropos*" :height 30)
                  ("*Help*" :height 30)
                  ("*sdic*" :height 20)
                  ;; ("*anything*" :height 20)
                  ;; ("*anything moccur*" :height 20)
                  ;; ("*Anything Completions*" :height 20)
                  ("*magit-edit-log*" :height 0.5)
                  (dired-mode :height 20 :position top))
                popwin:special-display-config))
  (define-key global-map (kbd "C-x p") 'popwin:edisplay-last-buffer))
(length popwin:special-display-config)
;;; ----------------------------------------------------------------------
;;; git-gutter.el
;;; ----------------------------------------------------------------------
(require 'git-gutter-fringe)
(global-git-gutter-mode t)


;;; ----------------------------------------------------------------------
;;; ansi-term / shell-pop
;;; ----------------------------------------------------------------------
(when (not (eq system-type 'windows-nt))
  (defadvice ansi-term (after ansi-term-after-advice (arg))
    "run hook as after advice"
    (run-hooks 'ansi-term-after-hook))
  (ad-activate 'ansi-term)

  (defvar ansi-term-after-hook nil)
  (add-hook 'term-mode-hook
            (lambda()
              (define-key term-raw-map (kbd "C-\\") nil)
              (define-key term-raw-map (kbd "M-x") nil)
              (define-key term-raw-map (kbd "C-z") nil)
              (define-key term-raw-map (kbd "C-z z") 'term-stop-subjob)
              (define-key term-raw-map (kbd "C-k")
                (lambda (&optional arg) (interactive "P") (funcall 'kill-line arg) (term-send-raw)))
              (define-key term-raw-map (kbd "C-y") 'term-paste)
              (define-key term-raw-map (kbd "M-y") 'anything-show-kill-ring)
              (define-key term-raw-map (kbd "ESC <C-return>") 'my-term-switch-line-char)
              (define-key term-mode-map (kbd "ESC <C-return>") 'my-term-switch-line-char)))

  (require 'shell-pop)
  (shell-pop-set-universal-key (kbd "<f12>"))
  (shell-pop-set-internal-mode "ansi-term")
  (shell-pop-set-internal-mode-shell "/bin/zsh")
  (shell-pop-set-window-height 40)

  (defun my-term-switch-line-char ()
    "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
    (interactive)
    (cond
     ((term-in-line-mode)
      (term-char-mode))
     ((term-in-char-mode)
      (term-line-mode))))

  (defadvice anything-c-kill-ring-action (around my-anything-kill-ring-term-advice activate)
    "In term-mode, use `term-send-raw-string' instead of `insert-for-yank'"
    (if (eq major-mode 'term-mode)
        (letf (((symbol-function 'insert-for-yank) (symbol-function 'term-send-raw-string)))
          ad-do-it)
      ad-do-it)))

;;; ----------------------------------------------------------------------
;;; jaspace.el
;;; ----------------------------------------------------------------------
(cond (window-system
       (require 'jaspace)
       (setq jaspace-alternate-eol-string "\xab\n")
       (setq jaspace-highlight-tabs t)
       (setq jaspace-modes
             (append '(python-mode php-mode coffee-mode js2-mode) jaspace-modes))))

;;; ----------------------------------------------------------------------
;;; diminish
;;; ----------------------------------------------------------------------
(when (require 'diminish nil t)
  (diminish 'flymake-mode)
  (diminish 'auto-complete-mode)
  (add-hook 'jaspace-mode-hook (lambda () (diminish 'jaspace-mode)))
  (add-hook 'ruby-electric-mode-hook (lambda () (diminish 'ruby-electric-mode)))
  (add-hook 'gtags-mode-hook (lambda () (diminish 'gtags-mode)))
  (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
  (diminish 'git-gutter-mode)
  (diminish 'undo-tree-mode)
  (if (fboundp 'ibus-mode) (diminish 'ibus-mode)))

;;; ----------------------------------------------------------------------
;;; 行末に存在するスペースを強調表示
;;; ----------------------------------------------------------------------
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t)
  (dolist (m '(calendar-mode-hook ansi-term-after-hook))
    (add-hook m
              '(lambda ()
                 (setq show-trailing-whitespace nil)))))

;;; ----------------------------------------------------------------------
;;; japanese-(hankaku|zenkaku)-region の俺俺変換テーブル
;;; ----------------------------------------------------------------------
(eval-after-load "japan-util"
  '(progn
     (put-char-code-property ?ー 'jisx0201 ?ｰ)
     (put-char-code-property ?ー 'ascii nil)
     (put-char-code-property ?ｰ 'jisx0208 ?ー)
     (put-char-code-property ?ｰ 'ascii nil)
     (put-char-code-property ?〜 'ascii nil)
     (put-char-code-property ?、 'ascii nil)
     (put-char-code-property ?。 'ascii nil)
     (put-char-code-property ?.  'jisx0208 ?．)
     (put-char-code-property ?,  'jisx0208 ?，)
     (put-char-code-property ?． 'jisx0201 ?.)
     (put-char-code-property ?， 'jisx0201 ?,)))
;; 全角ひらがなを半角カナに変換しない (携帯開発向け)
(dolist (c '(?あ ?い ?う ?え ?お ?か ?き ?く ?け ?こ ?さ ?し ?す ?せ ?そ
                 ?た ?ち ?つ ?て ?と ?な ?に ?ぬ ?ね ?の ?は ?ひ ?ふ ?へ ?ほ
                 ?ま ?み ?む ?め ?も ?や ?ゆ ?よ ?ら ?り ?る ?れ ?ろ ?わ ?ゐ
                 ?ゑ ?を ?ん ?が ?ぎ ?ぐ ?げ ?ご ?ざ ?じ ?ず ?ぜ ?ぞ ?だ ?ぢ
                 ?づ ?で ?ど ?ば ?び ?ぶ ?べ ?ぼ ?ぱ ?ぴ ?ぷ ?ぺ ?ぽ ?ぁ ?ぃ
                 ?ぅ ?ぇ ?ぉ ?っ ?ゃ ?ゅ ?ょ ?ゎ ?ヮ ?ヶ ?ヵ))
  (put-char-code-property c 'jisx0201 nil))

;;; ----------------------------------------------------------------------
;;; ファイルをシステムの関連付けで開く
;;; ----------------------------------------------------------------------
(defun my-file-open-by-windows (file)
  "ファイルをウィンドウズの関連付けで開く"
  (interactive "fOpen File: ")
  (message "Opening %s..." file)
  (cond ((not window-system)
          ; window-system⇒w32と表示される
        )
        ((eq system-type 'windows-nt)
          ; XPではwindows-ntと表示される
          ; infile:      標準入力
          ; destination: プロセスの出力先
          ; display:     ?
          (call-process "cmd.exe" nil 0 nil "/c" "start" "" (convert-standard-filename file)))
        ((eq system-type 'darwin)
          (call-process "open" nil 0 nil file))
        (t
          (call-process "xdg-open" nil 0 nil file)))
  (recentf-add-file file)
  (message "Opening %s...done" file))

;;; ----------------------------------------------------------------------
;;; open-junk-file
;;; ----------------------------------------------------------------------
(global-set-key "\C-x\C-z" 'open-junk-file)

;;; ----------------------------------------------------------------------
;;; lispxmp
;;; ----------------------------------------------------------------------
(define-key emacs-lisp-mode-map "\C-c\C-d" 'lispxmp)

;;; ----------------------------------------------------------------------
;;; paredit
;;; ----------------------------------------------------------------------
(define-key paredit-mode-map [C-right] nil)
(define-key paredit-mode-map [C-left] nil)
(define-key paredit-mode-map (kbd "C-c <right>") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-c <left>") 'paredit-forward-barf-sexp)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;;; ----------------------------------------------------------------------
;;; auto-async-byte-compile
;;; ----------------------------------------------------------------------
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; ----------------------------------------------------------------------
;;; eldoc-mode
;;; ----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;; ----------------------------------------------------------------------
;;; その他のキーバインド
;;; ----------------------------------------------------------------------
(find-function-setup-keys)
(global-set-key [home] 'beginning-of-buffer )
(global-set-key [end] 'end-of-buffer )
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\C-x\C-h" 'help-for-help)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xw" 'widen)
(global-set-key [?\C-=] 'call-last-kbd-macro)
(global-set-key [(shift tab)] 'indent-region)
(global-set-key [backtab] 'indent-region)
(global-set-key "\C-\M-g" 'keyboard-escape-quit)
(cond ((eq window-system 'x)
       (define-key function-key-map [backspace] [8])
       (put 'backspace 'ascii-character 8)
       (global-set-key [delete] 'delete-char)
       (global-set-key [backspace] 'delete-backward-char)
       (global-set-key "\177" 'delete-char)
       (global-set-key "\C-h" 'backward-delete-char)
       (autoload 'mwheel-install "mwheel" "Enable mouse wheel support.")
       (mwheel-install)
       (global-set-key [mouse-2] 'mouse-yank-at-click))
      ((eq window-system 'w32)
       (global-set-key [mouse-2] 'mouse-yank-at-click))
      (t
       (global-set-key "\C-h" (quote delete-backward-char))))
(define-key isearch-mode-map [(control h)] 'isearch-delete-char)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)

;;; ----------------------------------------------------------------------
;;; narrowing などの操作を有効化
;;; ----------------------------------------------------------------------
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; ----------------------------------------------------------------------
(cd "~")
;;; end of file ;;;
