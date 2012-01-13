;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; ----------------------------------------------------------------------
;;; gnuserv
;;; ----------------------------------------------------------------------
(when (not (require 'gnuserv-compat nil t))
  (require 'gnuserv nil t))
(when (fboundp 'server-start)
  (server-start)
  (setq gnuserv-frame (selected-frame)))

;;; ----------------------------------------------------------------------
;;; 基本設定
;;; ----------------------------------------------------------------------
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 2)
(setq next-line-add-newlines nil)
(setq kill-whole-line t)
(setq case-replace nil)
(setq default-major-mode 'text-mode)
(setq-default transient-mark-mode t)
(setq indent-line-function 'indent-relative-maybe)
(setq truncate-partial-width-windows nil)
(setq completion-ignore-case t)
(temp-buffer-resize-mode t)
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(tool-bar-mode nil)
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

;; Automatically reload files after they've been modified (typically in Visual C++)
(global-auto-revert-mode t)

;; Mule-UCS
(when (< emacs-major-version '22)
  (require 'un-define nil t)
  (require 'jisx0213 nil t))

;; 日本語環境設定
(set-language-environment "Japanese")

;; 環境固有の設定
(cond ((featurep 'meadow)
       (load "init-meadow"))
      (t
       (load "init-emacs")))
(when (eq window-system 'w32)
  (setenv "CYGWIN" "nodosfilewarning"))

;;; ----------------------------------------------------------------------
;;; Anthy
;;; ----------------------------------------------------------------------
(when (require 'anthy nil t)
  (setq default-input-method "japanese-anthy"))

;;; ----------------------------------------------------------------------
;;; ee
;;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp/ee")
(require 'ee-autoloads)

;;; ----------------------------------------------------------------------
;;; iswitchb
;;; ----------------------------------------------------------------------
;; (iswitchb-mode 1)
;; (iswitchb-default-keybindings)
;; (add-hook 'iswitchb-define-mode-map-hook
;;           (lambda ()
;;             (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
;;             (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)
;;             (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
;;             (define-key iswitchb-mode-map " " 'iswitchb-next-match)
;;             (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))

;;; ----------------------------------------------------------------------
;;; ibuffer
;;; ----------------------------------------------------------------------
(when (load-library "ibuffer")
  (require 'ibuffer)
  (setq ibuffer-formats
        '((mark modified read-only " " (name 30 30)
                " " (size 6 -1) " " (mode 16 16) " " (coding 15 15) " " filename)
          (mark " " (name 30 -1) " " (coding 15 15) " " filename)))
  (ibuffer-define-column
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
(setq abbrev-file-name (expand-file-name ".abbrev_defs" "~"))
(quietly-read-abbrev-file)
(add-hook 'pre-command-hook
          (lambda ()
            (setq abbrev-mode nil)))

;; dabbrev を強調表示
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
(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete")
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (when (boundp 'ac-modes)
    (setq ac-modes
          (append ac-modes
                  (list 'html-mode))))
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
  (setq ac-auto-start 1)
  (setq ac-dwim nil)
  (setq-default ac-sources '(ac-source-dictionary
                             ac-source-words-in-same-mode-buffers
                             ac-source-files-in-current-dir))
  (set-face-attribute 'ac-completion-face nil
		      :foreground "yellow" :underline t)
  (set-face-attribute 'ac-candidate-face nil
		      :background "darkgray" :underline nil)
  (set-face-attribute 'ac-selection-face nil
		      :background "steelblue")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))

;;; ----------------------------------------------------------------------
;;; snippet
;;; ----------------------------------------------------------------------
(require 'snippet nil t)
;;(require 'yasnippet-bundle nil t)

;;; ----------------------------------------------------------------------
;;; font-lock
;;; ----------------------------------------------------------------------
(setq font-lock-support-mode
      (if (fboundp 'jit-lock-mode) 'jit-lock-mode 'lazy-lock-mode))
(global-font-lock-mode t)

;;; ----------------------------------------------------------------------
;;; GUI/CUI 固有の設定
;;; ----------------------------------------------------------------------
(cond (window-system
       (require 'color-theme)
       (load "my-color-theme")
       (my-color-theme)
       (when (eq window-system 'w32)
         (require 'jaspace)
         (setq jaspace-alternate-eol-string "\xab\n")
         (setq jaspace-highlight-tabs t)
         (add-to-list 'jaspace-modes 'python-mode)
         (add-to-list 'jaspace-modes 'php-mode))))

;;; ----------------------------------------------------------------------
;;; 行末に存在するスペースを強調表示
;;; ----------------------------------------------------------------------
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t)
  (dolist (m '(calendar-mode-hook))
    (add-hook m
              '(lambda ()
                 (setq show-trailing-whitespace nil)))))

;;; ----------------------------------------------------------------------
;;; mpg123-mode
;;; ----------------------------------------------------------------------
(autoload 'mpg123 "mpg123" "A Front-end to mpg123" t)
(setq mpg123-startup-volume 70)
(load "id3")
(setq id3*coding 'shift_jis)

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
       (setq browse-url-browser-function 'browse-url-mozilla)
       (global-set-key [mouse-3] 'browse-url-mozilla))
      ((eq window-system 'w32)
       (setq browse-url-browser-function 'browse-url-default-windows-browser)
       (global-set-key [mouse-3] 'browse-url-default-windows-browser))
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
;;  		    :foreground "light steel blue" :bold t)

;;; ----------------------------------------------------------------------
;;; redo
;;; ----------------------------------------------------------------------
(when (require 'redo nil t)
  (define-key ctl-x-map (if window-system "U" "r") 'redo)
  (define-key global-map [?\C-.] 'redo))

;;; ----------------------------------------------------------------------
;;; migemo
;;; ----------------------------------------------------------------------
(when (require 'migemo nil t)
  (if (eq window-system 'w32)
      (progn (setq migemo-command "./cmigemo")
             (setq migemo-dictionary "C:/emacs-23.3-20110402/etc/migemo/migemo-dict")
             (setq migemo-coding-system 'japanese-shift-jis-unix)))
  (setq migemo-options (list "-q" "--emacs"))
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
;;; Wanderlust
;;; ----------------------------------------------------------------------
;; (when (locate-library "wl")
;;   (autoload 'wl "wl" "Wanderlust" t)
;;   (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
;;   (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
;;   ;; wl をデフォルトの MUA に
;;   (if (boundp 'mail-user-agent)
;;       (setq mail-user-agent 'wl-user-agent))
;;   (if (boundp 'read-mail-command)
;;       (setq read-mail-command 'wl)))

;;; ----------------------------------------------------------------------
;;; ange-ftp
;;; ----------------------------------------------------------------------
;; (setq ange-ftp-ftp-program-args '("-p" "-i" "-n" "-g" "-v" "-e"))

;;; ----------------------------------------------------------------------
;;; TRAMP
;;; ----------------------------------------------------------------------
;; (when (require 'tramp nil t)
;;   (modify-coding-system-alist 'process "plink" 'euc-japan-unix)
;;   (modify-coding-system-alist 'process "pscp" 'euc-jp-unix)
;;   (setq tramp-shell-prompt-pattern "^[^#$%>\n]*[#$%>][^#$%>\n ]* \\([^#$%>\n]*\\[.*\\][^#$%>\n]*\\)?")
;;   (setq tramp-debug-buffer t))

;;; ----------------------------------------------------------------------
;;; gpg.el
;;; ----------------------------------------------------------------------
(when (locate-library "gpg")
  (autoload 'gpg-after-find-file "gpg" nil t)
  (add-hook 'find-file-hooks 'gpg-after-find-file)
  (setq gpg-cipher "CAST5"))


;;; ----------------------------------------------------------------------
;;; howm
;;; ----------------------------------------------------------------------
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
(setq auto-coding-alist (cons '("\\.howm\\'" . utf-8-unix) auto-coding-alist))
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
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

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
;;; org-mode
;;; ----------------------------------------------------------------------
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-startup-indented t)
(setq org-return-follows-link t)
(setq org-replace-disputed-keys t)
(setq org-disputed-keys
      '(([(shift up)]            . [(meta \[)])
        ([(shift down)]          . [(meta \])])
        ([(shift left)]          . [(meta -)])
        ([(shift right)]         . [(meta =)])
        ([(control shift right)] . [(meta +)])
        ([(control shift left)]  . [(meta _)])
        ([(control shift left)]  . [(meta _)])
        ([(meta left)]           . [(meta ,)])
        ([(meta right)]          . [(meta .)])
        ([(meta shift left)]     . [(meta <)])
        ([(meta shift right)]    . [(meta >)])
        ))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate)
(setq org-directory "~/Dropbox/Documents/org/")
(setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-remember-templates
       '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
         ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
         ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
         ))
(global-set-key "\C-cr" 'org-remember)
(add-hook 'org-mode-hook
          '(lambda ()
             (local-unset-key [home])
             (local-unset-key [end])
             ))

;;; ----------------------------------------------------------------------
;;; Visual Studio .NET 2003
;;; ----------------------------------------------------------------------
;; (when (and (eq window-system 'w32)
;; 	   (require 'vsn nil t))
;;   (global-set-key [C-return] 'vsn-line-open-buffer)
;;   (setq vsn-open-exec "C:/Meadow/bin/vsn-open.vbs")
;;   (setq vsn-open-wait "200")
;;   (setq vsn-open-type "VSNET2003")
;;   (setq vsn-open-key "^g"))

;;; ----------------------------------------------------------------------
;;; speedbar
;;; ----------------------------------------------------------------------
(require 'speedbar)

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
	    (setq compilation-window-height 16)
	    ;; hideshow-mode
	    (when (locate-library "hideshow")
	      (require 'hideshow)
	      (hs-minor-mode 1))))

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
;;; gtags
;;; ----------------------------------------------------------------------
(when (locate-library "gtags")
  (autoload 'gtags-mode "gtags" "" t)
  (add-hook 'gtags-mode-hook
            '(lambda ()
               (local-set-key "\M-t" 'gtags-find-tag)
               (local-set-key "\M-r" 'gtags-find-rtag)
               (local-set-key "\M-s" 'gtags-find-symbol)
               (local-set-key "\C-t" 'gtags-pop-stack)
               (define-key gtags-mode-map [mouse-3] nil)
               (define-key gtags-select-mode-map [mouse-3] nil)))
  (add-hook 'c-mode-common-hook
            '(lambda()
               (gtags-mode 1)
               ;;(gtags-make-complete-list)
               )))

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
;;; psvn
;;; ----------------------------------------------------------------------
;; (when (require 'psvn nil t)
;;   (add-hook 'dired-mode-hook
;;             '(lambda ()
;;                (define-key dired-mode-map "V" 'svn-status)
;;                (turn-on-font-lock)))
;;   (setq svn-status-hide-unmodified t)
;;   (setq process-coding-system-alist
;;         (cons '("svn" . utf-8) process-coding-system-alist)))

;;; ----------------------------------------------------------------------
;;; dsvn
;;; ----------------------------------------------------------------------
(when (locate-library "dsvn")
  (autoload 'svn-status "dsvn" "Run `svn status'." t)
  (autoload 'svn-update "dsvn" "Run `svn update'." t)
  (add-hook 'dired-mode-hook
            '(lambda ()
               (define-key dired-mode-map "V" 'svn-status)))
  (setq svn-status-hide-unmodified t)
  (setq process-coding-system-alist
        (cons '("svn" . utf-8) process-coding-system-alist)))

;;; ----------------------------------------------------------------------
;;; magit
;;; ----------------------------------------------------------------------
(autoload 'magit-status "magit" nil t)

;;; ----------------------------------------------------------------------
;;; git-commit-mode
;;; ----------------------------------------------------------------------
(require 'git-commit nil t)

;;; ----------------------------------------------------------------------
;;; lisp-mode
;;; ----------------------------------------------------------------------
;;(add-hook 'lisp-mode-hook
;;          '(lambda ()
;;             (setq indent-tabs-mode nil)))

;;; ----------------------------------------------------------------------
;;; ruby-mode
;;; ----------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("config\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Rake\\|Cap\\|Gem\\|Guard\\)file$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(autoload 'ruby-electric-mode "ruby-electric" "Minor mode providing electric editing commands for ruby files" t)
(autoload 'rubydb "rubydb3x" "Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             (ruby-electric-mode t)
             (define-key ruby-mode-map "\C-cd" 'rubydb)
             (setq dabbrev-abbrev-skip-leading-regexp "[:@]")))

(defun ruby-insert-magic-comment-if-needed ()
  "バッファのcoding-systemをもとにmagic commentをつける。"
  (when (and (eq major-mode 'ruby-mode)
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

;;; ReFe
;; (when (locate-library "refe")
;;   (when (featurep 'meadow)
;;     (setq refe-coding-system 'shift_jis))
;;   (autoload 'refe "refe" "ReFe" t))

;;; ri-emacs
;; (when (setq ri-ruby-script (executable-find "ri-emacs"))
;;   (autoload 'ri "ri-ruby" nil t))

;;; rcodetools
;; (when (require 'rcodetools nil t)
;;   (setq rct-find-tag-if-available nil))

;;; auto-complete-ruby
;; (when (require 'auto-complete-ruby nil t)
;;   (setq ac-omni-completion-sources
;; 	'((ruby-mode . (("\\.\\=" . (ac-source-rcodetools)))))))

;;; ----------------------------------------------------------------------
;;; rails-mode
;;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-rails")
(require 'rails-lib)
(require 'rails)
(setq rails-indent-and-complete nil)
(define-keys rails-minor-mode-map
  ("\C-c\C-p"            'rails-lib:run-primary-switch)
  ("\C-c\C-n"            'rails-lib:run-secondary-switch)
  ([?\C-.]               'redo))
(define-keys rails-view-minor-mode-map
  ("\C-c\C-cp"           'rails-view-minor-mode:create-partial-from-selection))

;;; ----------------------------------------------------------------------
;;; python-mode
;;; ----------------------------------------------------------------------
(setq py-indent-offset 4)
;;(add-hook 'python-mode-hook
;;          '(lambda ()
;;             (setq indent-tabs-mode nil)))
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
;;; lua-mode
;;; ----------------------------------------------------------------------
(autoload 'lua-mode "lua-mode" "Major mode for editing lua scripts.")
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;; ----------------------------------------------------------------------
;;; yaml-mode
;;; ----------------------------------------------------------------------
(autoload 'yaml-mode "yaml-mode" "YAML Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             ;;(setq indent-tabs-mode nil)
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
		("\\.[Vv][Bb][Gg]$" . vbp-mode)
		("\\.html\\.erb$"   . html-mode)
		("\\.rhtml$"        . html-mode))
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; php-mode
;;; ----------------------------------------------------------------------
(autoload 'php-mode "php-mode" "PHP mode" t)
(custom-set-variables '(php-mode-force-pear t))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$" . php-mode))
(add-hook 'php-mode-hook
          '(lambda ()
	     ;; (set-buffer-file-coding-system 'euc-jp-unix)
             (define-key php-mode-map '[(control .)] nil)
             (define-key php-mode-map '[(control c)(control .)] 'php-show-arglist)
	     (setq dabbrev-abbrev-skip-leading-regexp "\\$")))

;;; ----------------------------------------------------------------------
;;; html-helper-mode
;;; ----------------------------------------------------------------------
;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; (setq html-helper-basic-offset 4)
;; (add-hook 'html-helper-mode-hook
;;        '(lambda ()
;;           (make-local-variable 'outline-minor-mode-prefix)
;;           (setq outline-minor-mode-prefix "\C-o")
;;           (make-local-variable 'outline-regexp)
;;           (setq outline-regexp "")
;;           (outline-minor-mode t)))

;;; ----------------------------------------------------------------------
;;; css-mode
;;; ----------------------------------------------------------------------
(autoload 'css-mode "css-mode" "CSS Editing Mode" t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-hook 'css-mode-hook
	  '(lambda ()
	     (define-key cssm-mode-map "\C-m" 'newline-and-indent)))

;;; ----------------------------------------------------------------------
;;; js2-mode (javascript)
;;; ----------------------------------------------------------------------
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; ----------------------------------------------------------------------
;;; scss-mode
;;; ----------------------------------------------------------------------
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil) ;; 自動コンパイルをオフにする
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;; ----------------------------------------------------------------------
;;; coffee-mode
;;; ----------------------------------------------------------------------
(autoload 'coffee-mode "coffee-mode" "CoffeeScript Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

;;; ----------------------------------------------------------------------
;;; haskell-mode
;;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode")
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(setq haskell-literate-default 'latex)
(setq haskell-doc-idle-delay 0)
(add-to-list 'auto-mode-alist '("\\.[hg]s$"  . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hi$"     . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[hg]s$" . literate-haskell-mode))

;;; ----------------------------------------------------------------------
;;; csharp-mode
;;; ----------------------------------------------------------------------
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;;; ----------------------------------------------------------------------
;;; po-mode
;;; ----------------------------------------------------------------------
(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)

;;; ----------------------------------------------------------------------
;;; mmm-mode
;;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp/mmm-mode")
(when (require 'mmm-mode nil t)
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 2)
  ;; 非 GUI 端末の場合
  (if (not window-system)
      (progn
	(set-face-background 'mmm-default-submode-face nil)
	(set-face-bold-p 'mmm-default-submode-face t)
	(set-face-background 'mmm-comment-submode-face nil)
	(set-face-bold-p 'mmm-comment-submode-face t)
	))
  ;; JavaScript 用に、html-mode内で javascript-mode を使えるようにします。
  (mmm-add-mode-ext-class nil "\\.html?\\'" 'html-javascript)
  (mmm-add-classes
   '((html-javascript
      :submode js2-mode
      :front "<script[^>]*>"
      :face mmm-comment-submode-face
      :back "</script>")))
  ;; ASP 用に、html-mode内で visual-basic-mode を使えるようにします。
  (mmm-add-classes
   '((asp
      :submode visual-basic-mode
      :front "<%"
      :back "%>"
      :insert ((?c asp nil @ "<%" @ " " _ " " @ "%>" @)
	       (?e asp nil @ "<%=" @ " " _ " " @ "%>" @)))))
  (mmm-add-mode-ext-class nil "\\.[Aa][Ss][PpAa]$" 'asp)
  ;; JSP 用に、html-mode内で java-mode を使えるようにします。
  (mmm-add-classes
   '((jsp
      :submode java-mode
      :front "<%"
      :back "%>"
      :insert ((?c jsp nil @ "<%" @ " " _ " " @ "%>" @)
	       (?e jsp nil @ "<%=" @ " " _ " " @ "%>" @)))))
  (mmm-add-mode-ext-class nil "\\.[Jj][Ss][PpAa]$" 'jsp)
  ;; eRuby用に、html-mode内でruby-modeを使えるようにします。
  ;; (mmm-add-classes
  ;;  '((eruby
  ;;     :submode ruby-mode
  ;;     :front "<%"
  ;;     :back "%>"
  ;;     :insert ((?c eruby nil @ "<%" @ " " _ " " @ "%>" @)
  ;;              (?e eruby nil @ "<%=" @ " " _ " " @ "%>" @)))))
  ;; (mmm-add-mode-ext-class nil "\\.[Rr][Hh][Tt][Mm][Ll]$" 'eruby)
  ;; ヒアドキュメント用に、ruby-mode内でtext-modeを使えるようにします。
  ;; (mmm-add-classes
  ;;  '((ruby-heredoc
  ;;     :front "<<-\\([a-zA-Z0-9_-]+\\)"
  ;;     :front-offset (end-of-line 1)
  ;;     :back "~1$"
  ;;     :save-matches 1
  ;;     :submode text-mode
  ;;     :insert ((?d ruby-heredoc "Here-document Name: " @ "<<" str _ "\n"
  ;;                  @ "\n" @ str "\n" @)))))
  ;; (mmm-add-mode-ext-class nil "\\.[Rr][Bb]$" 'ruby-heredoc)
  )

;;; ----------------------------------------------------------------------
;;; latex-mode
;;; ----------------------------------------------------------------------
(add-hook 'latex-mode-hook
          '(lambda ()
             (setq tex-verbatim-face nil)
             (defun tex-font-lock-suscript () nil)))

;;; ----------------------------------------------------------------------
;;; 拡張子に対応する編集モードを設定
;;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ("\\.doc$"               . text-mode)
                ("\\.[Hh][Tt][Mm][Ll]?$" . html-mode)     ;; HTML Document
                ("\\.[Aa][Ss][PpAa]$"    . html-mode)     ;; Active Server Page
                ("\\.[Tt][Pp][Ll]?$"     . html-mode)     ;; Smarty Template
                ("\\.[Jj][Ss][PpAa]$"    . html-mode)     ;; Java Server Pages
                ("\\.[ch]java$"          . java-mode)     ;; i-appli
                )
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; #!shebang に対応する編集モードを設定
;;; ----------------------------------------------------------------------
(setq interpreter-mode-alist
      (append '(
                ("ruby" . ruby-mode)
                )
              interpreter-mode-alist))

;;; ----------------------------------------------------------------------
;;; ChangeLog 用の設定
;;; ----------------------------------------------------------------------
(setq user-full-name "YAMAGUCHI, Seiji")
(setq user-mail-address "valda@underscore.jp")

;;; ----------------------------------------------------------------------
;;; テンプレートの自動挿入
;;; ----------------------------------------------------------------------
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (expand-file-name "insert" (expand-file-name ".emacs.d" "~")))

;;; ----------------------------------------------------------------------
;;; ~のつくバックアップファイルの保存場所の指定
;;; ----------------------------------------------------------------------
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "bak" "~"))
            backup-directory-alist))

;;; ----------------------------------------------------------------------
;;; filecache
;;; ----------------------------------------------------------------------
(require 'filecache)
(file-cache-add-directory-list
   (list "~"
         "~/.emacs.d"
         "~/.emacs.d/elisp"
         "~/bin"
         "~/Dropbox"
         "c:/Libraries/WTL80/include"
         "c:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/atlmfc/include"))

;;; ----------------------------------------------------------------------
;;; recentf-ext
;;; ----------------------------------------------------------------------
(require 'recentf-ext)
(setq recentf-max-saved-items 1000)

;;; ----------------------------------------------------------------------
;;; session
;;; ----------------------------------------------------------------------
(when (require 'session nil t)
  (setq history-length t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 100)
                                  (session-file-alist 100 t)
                                  (file-name-history 1000)))
  (setq session-globals-max-string 10000000)
  (add-hook 'after-init-hook 'session-initialize))

;;; ----------------------------------------------------------------------
;;; mcomplete/icomplete
;;; ----------------------------------------------------------------------
(if (require 'mcomplete nil t)
    (progn
      (turn-on-mcomplete-mode))
  (icomplete-mode 0))

;;; ----------------------------------------------------------------------
;;; zlc
;;; ----------------------------------------------------------------------
;; (when (require 'zlc nil t)
;;   (let ((map minibuffer-local-map))
;;     ;; like menu select
;;     (define-key map (kbd "<down>")  'zlc-select-next-vertical)
;;     (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
;;     (define-key map (kbd "<right>") 'zlc-select-next)
;;     (define-key map (kbd "<left>")  'zlc-select-previous))
;;   )

;;; ----------------------------------------------------------------------
;;; elscreen
;;; ----------------------------------------------------------------------
(when (require 'elscreen nil t)
  (setq elscreen-display-tab nil)
  ;; (defun elscreen-frame-title-update ()
  ;;   (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
  ;;     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
  ;;            (screen-to-name-alist (elscreen-get-screen-to-name-alist))
  ;;            (title (mapconcat
  ;;                    (lambda (screen)
  ;;                      (format "%d%s %s"
  ;;                              screen (elscreen-status-label screen)
  ;;                              (get-alist screen screen-to-name-alist)))
  ;;                    screen-list " ")))
  ;;       (if (fboundp 'set-frame-name)
  ;;           (set-frame-name title)
  ;;         (setq frame-title-format title)))))
  ;; (eval-after-load "elscreen"
  ;;   '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))
  (cond (window-system
         (elscreen-set-prefix-key "\C-z")
         (define-key elscreen-map "z" 'iconify-frame))
        (t
         (elscreen-set-prefix-key "\C-t"))))

;;; ----------------------------------------------------------------------
;;; flymake
;;; ----------------------------------------------------------------------
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
(when (require 'flymake nil t)
  ;; rails-mode で require されるのでコメントアウト
  ;; (defun flymake-ruby-init ()
  ;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
  ;;                        'flymake-create-temp-inplace))
  ;;          (local-file  (file-relative-name
  ;;                        temp-file
  ;;                        (file-name-directory buffer-file-name))))
  ;;     (list "ruby" (list "-c" local-file))))
  ;; (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
  ;; (push '("\\(Rake\\|Cap\\)file$" flymake-ruby-init) flymake-allowed-file-name-masks)
  ;; (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
  ;; (add-hook 'ruby-mode-hook
  ;;           '(lambda ()
  ;;              ;; Don't want flymake mode for ruby regions in rhtml files
  ;;              (if (not (null buffer-file-name)) (flymake-mode))))
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
;;; anything.el
;;; ----------------------------------------------------------------------
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-gtags)
(require 'anything-c-moccur)

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
     anything-c-source-bookmarks
     anything-c-source-gtags-select
     anything-c-source-recentf
     anything-c-source-file-cache)
   " *my-anything*"))

(global-set-key (if window-system (kbd "C-;") "\C-x;") 'my-anything)
(global-set-key "\M-x" 'anything-M-x)
(global-set-key "\C-xb" 'anything-buffers+)
(global-set-key "\M-y" 'anything-show-kill-ring)

;;; anything-gtags
(add-hook 'gtags-mode-hook
          '(lambda ()
               (local-set-key "\M-\C-t" 'anything-gtags-select)))

;;; anything-c-moccur
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

;;; anything-c-adaptive-history の保存を無効化
(remove-hook 'kill-emacs-hook
             'anything-c-adaptive-save-history)
(ad-disable-advice 'anything-exit-minibuffer
                   'before
                   'anything-c-adaptive-exit-minibuffer)
(ad-disable-advice 'anything-select-action
                   'before
                   'anything-c-adaptive-select-action)
(setq anything-c-adaptive-history-length 0)

;;; ----------------------------------------------------------------------
;;; gist
;;; ----------------------------------------------------------------------
(require 'gist)

;;; ----------------------------------------------------------------------
;;; anything-gist
;;; ----------------------------------------------------------------------
;; (require 'anything-gist)

;;; ----------------------------------------------------------------------
;;; typing-outputz.el
;;; ----------------------------------------------------------------------
;; (require 'pit)
;; (require 'typing-outputz)
;; (setq outputz-key
;;       (cdr (assoc 'key
;; 		  (pit/get 'outputz.com
;; 			   '(require ((key . "Your Outputz key")))))))
;; (global-typing-outputz-mode t)

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
                  ;; ("*anything*" :height 20)
                  ;; ("*anything moccur*" :height 20)
                  ;; ("*Anything Completions*" :height 20)
                  (dired-mode :height 20 :position top))
                popwin:special-display-config))
  (define-key global-map (kbd "C-x p") 'popwin:display-last-buffer))

;;; ----------------------------------------------------------------------
;;; diminish
;;; ----------------------------------------------------------------------
(when (require 'diminish nil t)
  ;;(diminish 'typing-outputz-mode)
  (diminish 'flymake-mode)
  (diminish 'auto-complete-mode)
  (add-hook 'jaspace-mode-hook
            (lambda ()
              (diminish 'jaspace-mode)))
  (add-hook 'ruby-electric-mode-hook
            (lambda ()
              (diminish 'ruby-electric-mode))))

;;; ----------------------------------------------------------------------
;;; auto-install
;;; ----------------------------------------------------------------------
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-compatibility-setup) ; 互換性確保
  (condition-case nil
      (auto-install-update-emacswiki-package-name t)
    (error (message "an error occurred, but going my way …"))))

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
;;; その他のグローバルキーバインドなど
;;; ----------------------------------------------------------------------
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
(global-set-key [?\C-'] 'expand-abbrev)
(global-set-key "\C-\M-g" 'keyboard-escape-quit)
(global-unset-key "\C-xf")
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

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(cd "~")

;;; end of file ;;;
