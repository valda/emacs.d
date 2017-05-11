;;; -*- mode: lisp-interaction; coding: utf-8-unix -*-

;;; ----------------------------------------------------------------------
;;; gnuserv
;;; ----------------------------------------------------------------------
(unless (require 'gnuserv-compat nil t)
  (require 'gnuserv nil t))
(when (fboundp 'server-start)
  (server-start)
  (setq gnuserv-frame (selected-frame)))

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
(setq mode-line-frame-identification " ")
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-require-final-newline nil)
(setq ring-bell-function 'ignore)
(setq search-default-regexp-mode nil)

;; Automatically reload files after they've been modified (typically in Visual C++)
(global-auto-revert-mode 1)

;; 日本語環境設定
(set-language-environment "Japanese")

;; 規定の文字コードを UTF-8 に
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(if (not window-system)
    (set-terminal-coding-system 'utf-8-unix))
;; Windows 固有の設定
(when (eq window-system 'w32)
  (setq default-file-name-coding-system 'japanese-cp932-dos)
  (setenv "CYGWIN" "nodosfilewarning"))

;;; ----------------------------------------------------------------------
;;; フォント設定
;;; ----------------------------------------------------------------------
(add-to-list 'initial-frame-alist '(font . "Ricty-13.5"))
(setq default-frame-alist initial-frame-alist)

;;; ----------------------------------------------------------------------
;;; package.el
;;; ----------------------------------------------------------------------
(setq package-user-dir "~/.emacs.d/elpa")
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; install packages by package.el
(defvar package-el-installing-package-list
  '(
    ag
    anzu
    auto-async-byte-compile
    auto-complete
    bm
    coffee-mode
    csharp-mode
    cygwin-mount
    diminish
    dockerfile-mode
    dsvn
    editorconfig
    elscreen
    exec-path-from-shell
    flycheck
    flycheck-pyflakes
    gist
    git-gutter
    git-gutter-fringe
    google-translate
    helm
    helm-bm
    helm-c-yasnippet
    helm-descbinds
    helm-flycheck
    helm-gtags
    helm-ls-git
    helm-swoop
    highlight-symbol
    howm
    ido-vertical-mode
    inf-ruby
    js2-mode
    json-mode
    less-css-mode
    lispxmp
    lua-mode
    magit
    migemo
    mmm-mode
    monokai-theme
    mozc
    mozc-popup
    open-junk-file
    php-mode
    popwin
    rainbow-delimiters
    rainbow-mode
    recentf-ext
    rinari
    ruby-block
    ruby-end
    swbuff
    scss-mode
    session
    shell-pop
    smartrep
    snippet
    undo-tree
    vcl-mode
    web-mode
    wgrep-ag
    yaml-mode
    yasnippet
    )
  "A list of packages to install by package.el at launch.")

(require 'cl)
(let ((not-installed (loop for x in package-el-installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;;; ----------------------------------------------------------------------
;;; el-get
;;; ----------------------------------------------------------------------
(setq el-get-generate-autoloads nil)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; install packages by el-get
(defvar el-get-installing-package-list
  '(
    dabbrev-highlight
    emacs-php-align
    moccur-edit
    mozc-el-extensions
    po-mode
    po-mode+
    visual-basic-mode
    )
  "A list of packages to install by el-get at launch.")
(el-get 'sync el-get-installing-package-list)

;;; ----------------------------------------------------------------------
;;; diminish
;;; ----------------------------------------------------------------------
;; (require 'diminish)
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
;; (eval-after-load "ibus" '(diminish 'ibus-mode))
(eval-after-load "ruby-end" '(diminish 'ruby-end-mode))
(eval-after-load "whitespace" '(diminish 'global-whitespace-mode))
;; (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode))
(eval-after-load "helm" '(diminish 'helm-migemo-mode))

;;; ----------------------------------------------------------------------
;;; W32-IME / mozc / ibus / uim
;;; ----------------------------------------------------------------------
(cond ((eq window-system 'w32)
       (setq default-input-method "W32-IME")
       (w32-ime-initialize)
       (setq-default w32-ime-mode-line-state-indicator "[--]")
       (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]")))
      ((require 'mozc nil t)
       (require 'mozc-popup)
       (require 'mozc-cursor-color)
       (require 'mozc-mode-line-indicator)
       (setq default-input-method "japanese-mozc")
       (setq mozc-candidate-style 'popup)
       (add-to-list 'mozc-cursor-color-alist '(direct . "green")))
      ((require 'ibus nil t)
       (add-hook 'after-init-hook 'ibus-mode-on)
       (global-set-key "\C-\\" 'ibus-toggle)
       (setq ibus-cursor-color '("red" "green" "cyan"))
       (ibus-define-common-key [?\C-\  ?\C-/]  nil)
       (add-hook 'minibuffer-setup-hook 'ibus-disable)
       (ibus-disable-isearch))
      ((require 'uim nil t)
       (setq uim-candidate-display-inline t)
       (global-set-key "\C-\\" 'uim-mode)))

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
;;; ido-mode
;;; ----------------------------------------------------------------------
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(setq ido-use-filename-at-point 'guess)

;;; ----------------------------------------------------------------------
;;; imenu
;;; ----------------------------------------------------------------------
(require 'imenu)

;;; ----------------------------------------------------------------------
;;; smartrep.el
;;; ----------------------------------------------------------------------
(require 'smartrep)
(setq smartrep-mode-line-active-bg "DeepSkyBlue4")

;;; ----------------------------------------------------------------------
;;; snippet.el
;;; ----------------------------------------------------------------------
(require 'snippet)

;;; ----------------------------------------------------------------------
;;; yasnippet.el
;;; ----------------------------------------------------------------------
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-global-mode 1)

;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;;; ----------------------------------------------------------------------
;;; abbrev/dabbrev
;;; ----------------------------------------------------------------------
(setq save-abbrevs t)
(setq abbrev-file-name (expand-file-name "~/.emacs.d/.abbrev_defs"))
(quietly-read-abbrev-file)
(add-hook 'pre-command-hook
          (lambda ()
            (setq abbrev-mode nil)))

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
      '(yas/hippie-try-expand
        try-complete-abbrev
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))
(define-key esc-map  "/" 'hippie-expand) ;; M-/
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace t)

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
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-auto-start 2)
(setq ac-dwim t)
(setq ac-ignore-case t)
(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers))
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;;; ----------------------------------------------------------------------
;;; font-lock
;;; ----------------------------------------------------------------------
(setq font-lock-support-mode
      (if (fboundp 'jit-lock-mode) 'jit-lock-mode 'lazy-lock-mode))
(global-font-lock-mode t)

;;; ----------------------------------------------------------------------
;;; hl-line
;;; ----------------------------------------------------------------------
(when (window-system)
  (global-hl-line-mode t))

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
;;; swbuff
;;; ----------------------------------------------------------------------
(require 'swbuff)
(global-set-key [C-tab] 'swbuff-switch-to-next-buffer)
(global-set-key [C-iso-lefttab] 'swbuff-switch-to-previous-buffer)
(setq swbuff-exclude-buffer-regexps '("^ .*" "^\\*.*\\*" "^\\*magit.*"))

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
;;  (browse setq-kill-ring-separator "--ヽ(´ー｀)ノ--------------------")
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
  (setq migemo-options '("-q" "--emacs"))
  (cond ((eq window-system 'w32)
         (setq migemo-dictionary "./dict/utf-8/migemo-dict"))
        (t
         (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-use-pattern-alist nil)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  (setq migemo-isearch-min-length 2)
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
(setq howm-directory
      (cond ((string-equal system-name "SILVER")
             "D:/Dropbox/Documents/howm/")
            (t
             "~/Dropbox/Documents/howm/")))
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
(add-to-list 'auto-mode-alist '("\\.howm\\'" . howm-mode))

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
        ;; 自動改行(auto-newline)を有効にする
        (when (fboundp 'c-toggle-auto-newline)
          (c-toggle-auto-newline t))
        ;; セミコロンで自動改行しない
        (setq c-hanging-semi&comma-criteria nil)
        ;; コンパイルコマンドの設定
        (setq compile-command "make -k" )     ; Cygwin の make
        ;; (setq compile-command "nmake /NOLOGO /S") ; VC++ の nmake
        (setq compilation-window-height 16)
        (electric-pair-mode t)))

(define-key c-mode-base-map "\C-cc" 'compile)
(define-key c-mode-base-map "\C-h" 'c-electric-backspace)
(define-key c-mode-base-map "\C-xt" 'ff-find-other-file)
(define-key c-mode-base-map [mouse-2] 'ff-mouse-find-other-file)
(setq auto-mode-alist
      (append '(("\\.C\\'"            . c-mode)
                ("\\.[Hh]\\'"         . c++-mode)
                ("\\.[Hh][Pp][Pp]\\'" . c++-mode))
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; hideshow
;;; ----------------------------------------------------------------------
(when (require 'hideshow nil t)
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
;;; color-moccur
;;; ----------------------------------------------------------------------
(require 'color-moccur)
(setq moccur-split-word t) ; スペース区切りでAND検索
(setq moccur-use-migemo t)
(setq *moccur-buffer-name-exclusion-list*
      '(".+TAGS.+" "\.svn" "*Completions*" "*Messages*" " *migemo*"))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "O" 'dired-do-moccur)))
(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)
(global-set-key "\M-o" 'occur-by-moccur)
(global-set-key "\C-c\C-x\C-o" 'moccur)

;;; ----------------------------------------------------------------------
;;; moccur-edit
;;; ----------------------------------------------------------------------
(require 'moccur-edit)
(defadvice moccur-edit-change-file
    (after save-after-moccur-edit-buffer activate)
  (save-buffer))

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
(setq magit-push-always-verify nil)
(add-to-list 'auto-coding-alist '("COMMIT_EDITMSG" . utf-8-unix))
(eval-after-load "magit"
  '(progn
     (define-key magit-status-mode-map [C-tab] nil)
     (define-key magit-status-mode-map [C-iso-lefttab] nil)
     (define-key magit-diff-mode-map [C-tab] nil)
     (define-key magit-diff-mode-map [C-iso-lefttab] nil)))

;;; ----------------------------------------------------------------------
;;; ruby-mode
;;; ----------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(
                ("\\.rb\\'" . ruby-mode)
                ("config\\.ru\\'" . ruby-mode)
                ("\\(Rake\\|Cap\\|Gem\\|Guard\\)file\\'" . ruby-mode)
                ("\\.xremap\\'" . ruby-mode)
                )
              auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; (autoload 'rubydb "rubydb3x" "Run rubydb on program FILE in buffer *gud-FILE*.
;; The directory containing FILE becomes the initial working directory
;; and source-file directory for your debugger.")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (electric-pair-mode t)
             (electric-indent-mode t)
             (electric-layout-mode t)
             (ruby-end-mode t)
             ;; 保存時にマジックコメントを付けない
             (defadvice ruby-mode-set-encoding
                 (around ruby-mode-set-encoding-disable activate) nil)))

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

;; ruby の symbol をいい感じに hippie-expand する
(defun hippie-expand-ruby-symbols (orig-fun &rest args)
  (if (eq major-mode 'ruby-mode)
      (let ((table (make-syntax-table ruby-mode-syntax-table)))
        (modify-syntax-entry ?: "." table)
        (with-syntax-table table (apply orig-fun args)))
    (apply orig-fun args)))
(advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)

;;; rd-mode
(when (locate-library "rd-mode")
  (autoload 'rd-mode "rd-mode" "major mode for ruby document formatter RD" t)
  (add-to-list 'auto-mode-alist '("\\.rd\\'" . rd-mode)))

;;; ----------------------------------------------------------------------
;;; python-mode
;;; ----------------------------------------------------------------------
(setq py-indent-offset 4)
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-mode))

;;; ----------------------------------------------------------------------
;;; cperl-mode
;;; ----------------------------------------------------------------------
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq cperl-indent-level 4)
            (setq cperl-indent-tabs-mode nil)
            (setq cperl-continued-statement-offset 4)
            (setq cperl-comment-column 40)
            (setq cperl-close-paren-offset -4)
            (setq cperl-indent-parens-as-block t)
            (setq cperl-invalid-face nil)
            (setq cperl-electric-parens nil)
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
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

;;; ----------------------------------------------------------------------
;;; visual-basic-mode
;;; ----------------------------------------------------------------------
(autoload 'visual-basic-mode "visual-basic-mode" "Basic Editing Mode" t)
(autoload 'vbp-mode "vbp-mode" "VBP mode." t)
(setq visual-basic-mode-indent 4)
(setq auto-mode-alist
      (append '(("\\.[Ff][Rr][Mm]\\'" . visual-basic-mode)  ;;Form Module
        ("\\.[Bb][Aa][Ss]\\'" . visual-basic-mode)  ;;Bas Module
        ("\\.[Cc][Ll][Ss]\\'" . visual-basic-mode)  ;;Class Module
        ("\\.[Vv][Bb][Ss]?\\'" . visual-basic-mode) ;;VBScript file
        ("\\.[Vv][Bb][Pp]\\'" . vbp-mode)
        ("\\.[Vv][Bb][Gg]\\'" . vbp-mode))
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; php-mode
;;; ----------------------------------------------------------------------
(add-hook 'php-mode-hook
          '(lambda ()
             (require 'php-align)
             (php-align-setup)
             (php-enable-psr2-coding-style)
             (setq flycheck-phpcs-standard "PSR2")
             (electric-pair-mode t)
             (electric-indent-mode nil)
             (electric-layout-mode nil)
             (define-key php-mode-map '[(control .)] nil)
             (define-key php-mode-map '[(control c)(control .)] 'php-show-arglist)
             ;;(c-set-offset 'arglist-intro' +)
             (c-set-offset 'arglist-cont-nonempty' +)
             ;;(c-set-offset 'arglist-close' 0)
             (c-set-offset 'case-label +)
             ;;(require 'ac-php)
             ;;(add-to-list 'ac-sources 'ac-source-php)
             ;;(setq ac-sources (remove 'ac-source-dictionary ac-sources))
             ))

;;; ----------------------------------------------------------------------
;;; web-mode
;;; ----------------------------------------------------------------------
(add-hook 'web-mode-hook
          '(lambda()
             (electric-pair-mode t)
             (add-to-list 'electric-pair-pairs '(?' . ?'))
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-style-padding 0)
             (setq web-mode-script-padding 0)
             (setq web-mode-block-padding 0)
             (setq web-mode-enable-auto-indentation nil)
             (modify-syntax-entry ?% "w" web-mode-syntax-table)
             (modify-syntax-entry ?? "w" web-mode-syntax-table)
             (when (equal web-mode-content-type "jsx")
               (flycheck-add-mode 'javascript-eslint 'web-mode)
               (flycheck-mode))))

;; views という directory 配下に有る php ファイルは web-mode で開く
(add-to-list 'auto-mode-alist '("/views/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/Smarty/templates/.*\\.\\(php\\|tpl\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;;; ----------------------------------------------------------------------
;;; js2-mode (javascript)
;;; ----------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq-local electric-layout-rules
                        '((?\; . after)))))
(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)

;;; ----------------------------------------------------------------------
;;; json-mode
;;; ----------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))

;;; ----------------------------------------------------------------------
;;; coffee-mode
;;; ----------------------------------------------------------------------
(add-hook 'coffee-mode-hook
          '(lambda()
             (set (make-local-variable 'tab-width) 2)
             (setq coffee-tab-width 2)))

;;; ----------------------------------------------------------------------
;;; less-css-mode
;;; ----------------------------------------------------------------------
(setq less-css-compile-at-save nil)
(add-to-list 'ac-modes 'less-css-mode)
(add-hook 'less-css-mode-hook 'ac-css-mode-setup)

;;; ----------------------------------------------------------------------
;;; scss-mode
;;; ----------------------------------------------------------------------
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'ac-modes 'scss-mode)
(add-hook 'scss-mode-hook 'ac-css-mode-setup)

;;; ----------------------------------------------------------------------
;;; csharp-mode
;;; ----------------------------------------------------------------------
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;; ----------------------------------------------------------------------
;;; po-mode+
;;; ----------------------------------------------------------------------
;; (autoload 'po-mode "po-mode+"
;;   "Major mode for translators to edit PO files" t)
;; (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
;; (autoload 'po-find-file-coding-system "po-compat")
;; (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
;;                             'po-find-file-coding-system)

;;; ----------------------------------------------------------------------
;;; mmm-mode
;;; ----------------------------------------------------------------------
(require 'mmm-mode)
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
 '(
   (mmm-html-css-mode
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
    :front "<%[!=]?"
    :back "%>"
    :insert ((?% jsp-code nil        @ "<%"  @ " " _ " " @ "%>" @)
             (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
             (?= jsp-expression nil  @ "<%=" @ " " _ " " @ "%>" @)))
   (mmm-eruby-mode
    :submode ruby-mode
    :front "<%"
    :back "-?%>"
    :insert ((?c eruby nil @ "<%"  @ " " _ " " @ "%>" @)
             (?e eruby nil @ "<%=" @ " " _ " " @ "%>" @)))
   (mmm-php-mode
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\(\\?>\\|\\'\\)")
   ))
(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-css-mode)
(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-javascript-mode)
(mmm-add-mode-ext-class nil "\\.erb\\'" 'mmm-eruby-mode)
(mmm-add-mode-ext-class nil "\\.rhtml\\'" 'mmm-eruby-mode)
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'mmm-php-mode)

;;; ----------------------------------------------------------------------
;;; latex-mode
;;; ----------------------------------------------------------------------
(add-hook 'latex-mode-hook
          '(lambda ()
             (setq tex-verbatim-face nil)
             (defun tex-font-lock-suscript () nil)))

;;; ----------------------------------------------------------------------
;;; vcl-mode
;;; ----------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.vcl\\'" . vcl-mode))

;;; ----------------------------------------------------------------------
;;; rinari
;;; ----------------------------------------------------------------------
(require 'rinari)
(global-rinari-mode t)

;; ;; ido を helm に置き換える advice
;; (defun advice:ido-completing-read--helm (prompt choices &optional _predicate require-match
;;                                          initial-input hist def _inherit-input-method)
;;   (helm-comp-read prompt choices :must-match t))

;; (defun helm-rinari-find-model()
;;   (interactive)
;;   (advice-add 'ido-completing-read :override 'advice:ido-completing-read--helm)
;;   (rinari-find-model)
;;   (advice-remove 'ido-completing-read 'advice:ido-completing-read--helm))

;; (defun helm-rinari-find-controller()
;;   (interactive)
;;   (advice-add 'ido-completing-read :override 'advice:ido-completing-read--helm)
;;   (rinari-find-controller)
;;   (advice-remove 'ido-completing-read 'advice:ido-completing-read--helm))

;; (define-key rinari-minor-mode-map (kbd "C-c ; f m") 'helm-rinari-find-model)
;; (define-key rinari-minor-mode-map (kbd "C-c ; f c") 'helm-rinari-find-controller)

;;; ----------------------------------------------------------------------
;;; editorconfig
;;; ----------------------------------------------------------------------
(editorconfig-mode 1)

;;; ----------------------------------------------------------------------
;;; その他の拡張子に対応する編集モードを設定
;;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ("\\.doc\\'"               . text-mode)
                ("\\.[Hh][Tt][Mm][Ll]?\\'" . html-mode)     ;; HTML Document
                ("\\.[Aa][Ss][PpAa]\\'"    . html-mode)     ;; Active Server Page
                ("\\.[Jj][Ss][PpAa]\\'"    . html-mode)     ;; Java Server Pages
                ("\\.[ch]java\\'"          . java-mode)     ;; i-appli
                ("\\.html\\.erb\\'"        . html-mode)     ;; HTML(erb)
                ("\\.rhtml?\\'"            . html-mode)     ;; HTML(erb)
                ("\\.text\\.erb\\'"        . text-mode)     ;; Text(erb)
                ("\\.rtext\\'"             . text-mode)     ;; Text(erb)
                ("\\.coffee\\.erb\\'"      . coffee-mode)   ;; CoffeeScript(erb)
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
;;; recentf / recentf-ext
;;; ----------------------------------------------------------------------
;; http://qiita.com/itiut@github/items/d917eafd6ab255629346
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 10000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 'never)
(setq recentf-auto-save-timer
      (run-with-idle-timer 30 t '(lambda ()
                                   (with-suppressed-message (recentf-save-list)))))
(require 'recentf-ext)

;;; ----------------------------------------------------------------------
;;; session.el
;;; ----------------------------------------------------------------------
(setq history-length t)
(setq session-initialize '(de-saveplace session keys menus places)
      session-globals-include '((kill-ring 1000)
                                (session-file-alist 1000 t)
                                (file-name-history 10000)))
(setq session-globals-max-string 10000000)
(setq session-save-print-spec '(t nil 40000)) ; anything/helmと一緒に使うために必要
(add-hook 'after-init-hook 'session-initialize)

;;; ----------------------------------------------------------------------
;;; elscreen
;;; ----------------------------------------------------------------------
(setq elscreen-display-tab nil)
(elscreen-start)
(global-unset-key "\C-z")
(global-unset-key "\C-t")
(cond (window-system
       (elscreen-set-prefix-key "\C-z")
       (define-key elscreen-map "\C-z" 'elscreen-toggle)
       (define-key elscreen-map "z" 'iconify-frame))
      (t
       (elscreen-set-prefix-key "\C-t")
       (define-key elscreen-map "\C-t" 'elscreen-toggle)))
(smartrep-define-key
    global-map elscreen-prefix-key '(("p" . 'elscreen-previous)
                                     ("n" . 'elscreen-next)))

;;; ----------------------------------------------------------------------
;;; flycheck
;;; ----------------------------------------------------------------------
(require 'flycheck)
;;(require 'flycheck-pyflakes)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq flycheck-gcc-language-standard "c++11")
            (setq flycheck-clang-language-standard "c++11")))
(setq flycheck-disabled-checkers
      (append '(python-flake8
                python-pylint
                ruby-rubylint
                javascript-jshint
                javascript-jscs)
              flycheck-disabled-checkers))

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
;;; kill-ring に同じ内容の文字列を複数入れない
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
  ;; M$ Visual Studio key setup.
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<f2>")   'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous))

;;; ----------------------------------------------------------------------
;;; uniquify
;;; ----------------------------------------------------------------------
;(require 'uniquify)
;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

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
;;; helm
;;; ----------------------------------------------------------------------
(require 'helm)
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode)
(require 'helm-multi-match)
(helm-migemo-mode +1)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-ls-git)
;;(require 'helm-elscreen)

(setq helm-idle-delay 0.3)
(setq helm-input-idle-delay 0.2)
(setq helm-candidate-number-limit 100)
(setq helm-buffer-max-length 50)
(setq helm-truncate-lines t)
;;(setq helm-full-frame nil)
;;(setq helm-split-window-default-side 'same)

(global-set-key (if window-system (kbd "C-;") "\C-c;") 'helm-mini)
(global-set-key "\M-x" 'helm-M-x)
(global-set-key "\C-xb" 'helm-buffers-list)
(global-set-key "\M-y" 'helm-show-kill-ring)
;;(define-key elscreen-map "w" 'helm-elscreen)
(global-set-key "\C-cb" 'helm-bm)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)

;;(require 'helm-gtags)
(setq helm-gtags-auto-update t)
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (smartrep-define-key
      helm-gtags-mode-map "C-c" '(("<" . 'helm-gtags-previous-history)
                                  (">" . 'helm-gtags-next-history)))
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'ruby-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'html-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'web-mode-hook 'helm-gtags-mode)

(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(setq helm-multi-swoop-edit-save t)

;;; ----------------------------------------------------------------------
;;; anzu
;;; ----------------------------------------------------------------------
(setq anzu-mode-lighter "")
(setq anzu-deactivate-region t)
(setq anzu-search-threshold 1000)
(setq anzu-use-migemo t)
(global-anzu-mode t)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;;; ----------------------------------------------------------------------
;;; gist
;;; ----------------------------------------------------------------------
(require 'gist)

;;; ----------------------------------------------------------------------
;;; popwin.el
;;; ----------------------------------------------------------------------
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:adjust-other-windows t)
(setq popwin:special-display-config
      (append '(("*Backtrace*" :height 0.3)
                ("*Kill Ring*" :height 0.4 :noselect t)
                ("*Apropos*" :height 0.4)
                ("*Help*" :height 0.4)
                ("*sdic*" :height 0.3)
                ("*Google Translate*" :height 0.3)
                ;;("^\\*helm" :regexp t :height 20)
                ("\\*ag search.*\\*" :dedicated t :regexp t :height 0.4)
                ("*git-gutter:diff*" :height 0.4 :stick t)
                (" *auto-async-byte-compile*" :dedicated t :noselect t :height 0.2)
                (dired-mode :height 0.3 :position top))
              popwin:special-display-config))
(define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)

;;; ----------------------------------------------------------------------
;;; git-gutter.el
;;; ----------------------------------------------------------------------
(require 'git-gutter-fringe)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
(run-with-idle-timer 1 t 'git-gutter)
(global-git-gutter-mode t)

(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(smartrep-define-key
    global-map "C-x v" '(("p" . 'git-gutter:previous-hunk)
                         ("n" . 'git-gutter:next-hunk)))

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

  (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-universal-key "<f12>")
  (setq shell-pop-window-height 40)
  (setq shell-pop-window-position "bottom")
  (require 'shell-pop)

  (defun my-term-switch-line-char ()
    "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
    (interactive)
    (cond
     ((term-in-line-mode)
      (term-char-mode)
      (hl-line-mode -1))
     ((term-in-char-mode)
      (term-line-mode)
      (hl-line-mode 1))))

  (defadvice anything-c-kill-ring-action (around my-anything-kill-ring-term-advice activate)
    "In term-mode, use `term-send-raw-string' instead of `insert-for-yank'"
    (if (eq major-mode 'term-mode)
        (letf (((symbol-function 'insert-for-yank) (symbol-function 'term-send-raw-string)))
          ad-do-it)
      ad-do-it)))

;;; ----------------------------------------------------------------------
;;; whitespace-mode like jaspace.el
;;; ----------------------------------------------------------------------
(when (and (>= emacs-major-version 23)
       (require 'whitespace nil t))
  (setq whitespace-style
    '(face
      tabs spaces newline trailing space-before-tab space-after-tab
      space-mark tab-mark newline-mark))
  ;; (let ((dark (eq 'dark (frame-parameter nil 'background-mode))))
  ;;   (set-face-attribute 'whitespace-space nil
  ;;           :foreground (if dark "pink4" "azure3")
  ;;           :background 'unspecified)
  ;;   (set-face-attribute 'whitespace-tab nil
  ;;           :foreground (if dark "gray20" "gray80")
  ;;           :background 'unspecified
  ;;           :strike-through t)
  ;;   (set-face-attribute 'whitespace-newline nil
  ;;           :foreground (if dark "darkcyan" "darkseagreen")))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
    '((space-mark   ?\xA0  [?\xA4]  [?_]) ; hard space - currency
      (space-mark   ?\x8A0 [?\x8A4] [?_]) ; hard space - currency
      (space-mark   ?\x920 [?\x924] [?_]) ; hard space - currency
      (space-mark   ?\xE20 [?\xE24] [?_]) ; hard space - currency
      (space-mark   ?\xF20 [?\xF24] [?_]) ; hard space - currency
      (space-mark   ?　    [?□]    [?＿]) ; full-width space - square
      (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]) ; hard tab
      (newline-mark ?\n    [?\xAB ?\n])   ; eol - right quote mark
      ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))

;;; ----------------------------------------------------------------------
;;; google-translate.el
;;; ----------------------------------------------------------------------
(global-set-key "\C-xt" 'google-translate-at-point)
(global-set-key "\C-xT" 'google-translate-query-translate)
(global-set-key "\C-ct" 'google-translate-smooth-translate)

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
;; (when (require 'paredit nil t)
;;   (add-hook 'paredit-mode-hook
;;             (lambda ()
;;               (define-key paredit-mode-map [C-right] nil)
;;               (define-key paredit-mode-map [C-left] nil)
;;               (define-key paredit-mode-map (kbd "C-c <right>") 'paredit-forward-slurp-sexp)
;;               (define-key paredit-mode-map (kbd "C-c <left>") 'paredit-forward-barf-sexp)))
;;   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook 'enable-paredit-mode))

;;; ----------------------------------------------------------------------
;;; auto-async-byte-compile
;;; ----------------------------------------------------------------------
(when (require 'auto-async-byte-compile nil t)
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ----------------------------------------------------------------------
;;; eldoc-mode
;;; ----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;; ----------------------------------------------------------------------
;;; highlight-symbol
;;; ----------------------------------------------------------------------
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;;; ----------------------------------------------------------------------
;;; rainbow-delimiters
;;; ----------------------------------------------------------------------
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; ----------------------------------------------------------------------
;;; ag
;;; ----------------------------------------------------------------------
(setq ag-highlight-search t)
(setq ag-reuse-window t)
(setq ag-reuse-buffers t)
(global-set-key "\C-ca" 'ag)
(global-set-key "\C-cf" 'ag-project)

;;; ----------------------------------------------------------------------
;;; wgrep / wgrep-ag
;;; ----------------------------------------------------------------------
(setq wgrep-auto-save-buffer t)
(eval-after-load "ag"
    '(progn
       (add-hook 'ag-mode-hook 'wgrep-ag-setup)
       (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)))

;;; ----------------------------------------------------------------------
;;; monokai-theme
;;; ----------------------------------------------------------------------
(load-theme 'monokai t)
(set-face-attribute 'region nil
                    :foreground 'unspecified
                    :background "DeepSkyBlue4"
                    :inherit t)
(set-face-attribute 'whitespace-trailing nil
                    :background "orange"
                    :foreground 'unspecified
                    :inverse-video 'unspecified)
(set-face-attribute 'whitespace-tab nil
                    :foreground 'unspecified
                    :foreground "brown4"
                    :background 'unspecified
                    :inverse-video 'unspecified
                    :weight 'unspecified)
(set-face-attribute 'mmm-default-submode-face nil
                    :background 'unspecified
                    :inherit t)
(eval-after-load "howm"
  '(set-face-attribute 'howm-mode-title-face nil
                         :foreground "RoyalBlue"
                         :weight 'bold))

;;; ----------------------------------------------------------------------
;;; その他のキーバインド
;;; ----------------------------------------------------------------------
(find-function-setup-keys)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key [home] 'beginning-of-buffer )
(global-set-key [end] 'end-of-buffer )
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\C-x\C-h" 'help-for-help)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xw" 'widen)
(global-set-key [(shift tab)] 'indent-region)
(global-set-key [backtab] 'indent-region)
(global-set-key "\C-\M-g" 'keyboard-escape-quit)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
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
(smartrep-define-key
    global-map "C-x" '(("^" . 'enlarge-window)
                       ("_" . 'shrink-window)
                       ("{" . 'shrink-window-horizontally)
                       ("}" . 'enlarge-window-horizontally)))
(global-unset-key "\M-t")

;;; ----------------------------------------------------------------------
;;; narrowing などの操作を有効化
;;; ----------------------------------------------------------------------
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; ----------------------------------------------------------------------
(if (not (eq window-system 'w32))
    (exec-path-from-shell-initialize))
(cd "~")
;;; end of file ;;;
