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
(setq case-replace t)
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
(setq require-final-newline t)
(setq mode-require-final-newline t)
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
;;; WSL
;;; ----------------------------------------------------------------------
(defun my-wsl-p ()
  (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))

(when (my-wsl-p)
  (defun reset-frame-parameter (frame)
    (sleep-for 0.1)
    (set-frame-parameter frame 'height 32))
  (add-hook 'after-make-frame-functions #'reset-frame-parameter))

;;; ----------------------------------------------------------------------
;;; フォント設定
;;; ----------------------------------------------------------------------
;; abcdefghijklmnopqrst
;; あいうえおかきくけこ
;; 🥺😼🐕🎴🌈🕒🍣🍰🍲🍗
(setq use-default-font-for-symbols nil)
(set-face-attribute 'default nil :family "Cica" :height 150)
(set-fontset-font t '(#x1F000 . #x1FAFF) "Noto Color Emoji")
(add-to-list 'face-font-rescale-alist '(".*Noto Color Emoji.*" . 0.82))

;;; ----------------------------------------------------------------------
;;; package.el
;;; ----------------------------------------------------------------------
(setq package-user-dir "~/.emacs.d/elpa")
(require 'package)
;; (setq package-archives
;;       '(("gnu"          . "http://mirrors.163.com/elpa/gnu/")
;;         ("melpa"        . "https://melpa.org/packages/")
;;         ("melpa-stable" . "https://stable.melpa.org/packages/")
;;         ("org"          . "http://orgmode.org/elpa/"))
;;       package-archive-priorities
;;       '(("melpa-stable" . 10)
;;         ("gnu"          . 5)
;;         ("melpa"        . 0)
;;         ("org"          . 20)))
;; (setq package-menu-hide-low-priority nil)
(setq package-archives
      '(("gnu"          . "http://mirrors.163.com/elpa/gnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/"))
      )
(package-initialize)

;; install packages by package.el
(defvar package-el-installing-package-list
  '(
    diminish
    use-package
    )
  "A list of packages to install by package.el at launch.")

(require 'cl-macs)
(let ((not-installed (cl-loop for x in package-el-installing-package-list
                              when (not (package-installed-p x))
                              collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;;; ----------------------------------------------------------------------
;;; use-package
;;; ----------------------------------------------------------------------
(defvar use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.001)
(setq use-package-compute-statistics t)

;;; ----------------------------------------------------------------------
;;; paradox
;;; ----------------------------------------------------------------------
(use-package paradox
  :ensure t
  :defer t
  :init
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-automatically-star t))

;;; ----------------------------------------------------------------------
;;; el-get
;;; ----------------------------------------------------------------------
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
    tempbuf
    cygwin-mount
    )
  "A list of packages to install by el-get at launch.")
(el-get 'sync el-get-installing-package-list)

;;; ---------------------------------------------------------------------
;;; monokai-theme
;;; ----------------------------------------------------------------------
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  (with-eval-after-load 'mozc-cand-posframe
    (set-face-attribute 'mozc-cand-posframe-normal-face nil
                        :background monokai-highlight-line
                        :foreground monokai-emphasis)
    (set-face-attribute 'mozc-cand-posframe-focused-face nil
                        :background monokai-blue
                        :foreground monokai-background)
    (set-face-attribute 'mozc-cand-posframe-footer-face nil
                        :foreground monokai-foreground)))

;;; ---------------------------------------------------------------------
;;; doom-theme
;;; ----------------------------------------------------------------------
(use-package doom-themes
  :disabled t
  :ensure t
  :config
  (load-theme 'doom-one t)
  ;;(load-theme 'doom-vibrant t)
  (with-eval-after-load 'mozc-cand-posframe
    (set-face-attribute 'mozc-cand-posframe-normal-face nil
                        :background (face-background 'tooltip)
                        :foreground (face-foreground 'tooltip))
    (set-face-attribute 'mozc-cand-posframe-focused-face nil
                        :background (face-background 'company-tooltip-selection)
                        :foreground (face-foreground 'tooltip)
                        :weight (face-attribute 'company-tooltip-selection :weight))
    (set-face-attribute 'mozc-cand-posframe-footer-face nil
                        :foreground (face-foreground 'tooltip))))

(use-package doom-modeline
  :disabled t
  :ensure t
  :config
  (doom-modeline-mode t))

;;; ----------------------------------------------------------------------
;;; W32-IME / mozc / ibus / uim
;;; ----------------------------------------------------------------------
(defun my-w32-ime-init()
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq-default w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)

  ;; 日本語入力時にカーソルの色を変える設定
  (add-hook 'w32-ime-on-hook (lambda () (set-cursor-color "red")))
  (add-hook 'w32-ime-off-hook (lambda () (set-cursor-color "green")))

  ;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
  (add-hook 'minibuffer-setup-hook 'deactivate-input-method)

  ;; isearch に移行した際に日本語入力を無効にする
  (add-hook 'isearch-mode-hook (lambda ()
                                 (deactivate-input-method)
                                 (setq w32-ime-composition-window (minibuffer-window))))
  (add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil)))

  ;; helm 使用中に日本語入力を無効にする
  (advice-add 'helm :around (lambda (orig-fun &rest args)
                              (let ((select-window-functions nil)
                                    (w32-ime-composition-window (minibuffer-window)))
                                (deactivate-input-method)
                                (apply orig-fun args)))))

(defun my-mozc-init()
  (use-package mozc
    :ensure t
    :config
    ;; Windows の mozc では、セッション接続直後 directモード になるので hiraganaモード にする
    (when (my-wsl-p)
      (advice-add 'mozc-session-execute-command
                  :after (lambda (&rest args)
                           (when (eq (nth 0 args) 'CreateSession)
                             ;; (mozc-session-sendkey '(hiragana)))))
                             (mozc-session-sendkey '(Hankaku/Zenkaku))))))
    (define-key global-map [henkan]
      (lambda () (interactive)
        (activate-input-method default-input-method)))
    (define-key global-map [muhenkan]
      (lambda () (interactive)
        (deactivate-input-method)))
    (define-key global-map [zenkaku-hankaku] 'toggle-input-method)
    (define-key isearch-mode-map [henkan] 'isearch-toggle-input-method)
    (define-key isearch-mode-map [muhenkan] 'isearch-toggle-input-method)
    (defadvice mozc-handle-event (around intercept-keys (event))
      "Intercept keys muhenkan and zenkaku-hankaku, before passing keys to mozc-server (which the function mozc-handle-event does), to properly disable mozc-mode."
      (if (member event (list 'zenkaku-hankaku 'muhenkan))
          (progn
            (mozc-clean-up-session)
            (toggle-input-method))
        (progn ;(message "%s" event) ;debug
          (if (company--active-p)
              (company-abort))
          ad-do-it)))
    (ad-activate 'mozc-handle-event))

  (use-package mozc-im
    :ensure t
    :config
    (setq default-input-method "japanese-mozc-im")
    ;; mozc-cursor-color を利用するための対策
    (defvar-local mozc-im-mode nil)
    (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
    (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
    (advice-add 'mozc-cursor-color-update
                :around (lambda (orig-fun &rest args)
                          (let ((mozc-mode mozc-im-mode))
                            (apply orig-fun args))))
    ;; isearch を利用する前後で IME の状態を維持するための対策
    (defvar-local mozc-im-state nil)
    (add-hook 'isearch-mode-hook
              (lambda () (setq mozc-im-state mozc-im-mode)))
    (add-hook 'isearch-mode-end-hook
              (lambda ()
                (unless (eq mozc-im-state mozc-im-mode)
                  (if mozc-im-state
                      (activate-input-method default-input-method)
                    (deactivate-input-method))))))

  (use-package mozc-popup
    :if (not (window-system))
    :ensure t
    :after mozc
    :config
    (setq mozc-candidate-style 'popup))

  (use-package mozc-cand-posframe
    :if (window-system)
    :ensure t
    :after mozc
    :config
    (setq mozc-candidate-style 'posframe))

  (use-package mozc-cursor-color
    ;; :ensure t ; el-get
    :after mozc
    :config
    (setq mozc-cursor-color-alist '((direct        . "green")
                                    (read-only     . "yellow")
                                    (hiragana      . "red")
                                    (full-katakana . "goldenrod")
                                    (half-ascii    . "dark orchid")
                                    (full-ascii    . "orchid")
                                    (half-katakana . "dark goldenrod"))))
  )

(cond ((eq window-system 'w32)
       (my-w32-ime-init))
      (t
       (my-mozc-init)))

;;; ----------------------------------------------------------------------
;;; ibuffer
;;; ----------------------------------------------------------------------
(use-package ibuffer
  :bind
  ("\C-x\C-b" . ibuffer)
  :config
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
      )))

;;; ----------------------------------------------------------------------
;;; ido
;;; ----------------------------------------------------------------------
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-use-filename-at-point 'guess)
  (ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :after ido
  :config
  (ido-vertical-mode 1))

;;; ----------------------------------------------------------------------
;;; smartrep.el
;;; ----------------------------------------------------------------------
(use-package smartrep
  :ensure t
  :commands smartrep-define-key
  :config
  (setq smartrep-mode-line-active-bg "DeepSkyBlue4"))

;;; ----------------------------------------------------------------------
;;; snippet.el
;;; ----------------------------------------------------------------------
(use-package snippet
  :ensure t
  :commands (snippet-insert))

;;; ----------------------------------------------------------------------
;;; yasnippet.el
;;; ----------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (add-to-list 'auto-mode-alist '("emacs\\.d/snippets/" . snippet-mode))
  :config
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

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
(use-package dabbrev-highlight) ; el-get

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
;;; company-mode
;;; ----------------------------------------------------------------------
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode +1)
  (define-key company-active-map (kbd "TAB")   'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-mode-map (kbd "M-TAB") 'company-complete)
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend
          company-echo-metadata-frontend))
  (setq company-require-match 'never)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-auto-expand t))

(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode)
  (setq company-transformers
        '(company-sort-by-statistics
          company-sort-by-backend-importance))
  :after company)

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode)
  :after company)

(use-package company-emoji
  :disabled t
  :ensure t
  :config
  (add-to-list 'company-backends 'company-emoji)
  :after company)

(use-package company-box
  :ensure t
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :after (company all-the-icons)
  :config
  (require 'desktop)
  (push '(company-box-mode nil) desktop-minor-mode-table))

(use-package company-web
  :ensure t
  :defer t
  :after company)

;;; ----------------------------------------------------------------------
;;; font-lock
;;; ----------------------------------------------------------------------
(setq font-lock-support-mode
      (if (fboundp 'jit-lock-mode) 'jit-lock-mode 'lazy-lock-mode))
(global-font-lock-mode t)

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
;;; winner-mode
;;; ----------------------------------------------------------------------
(winner-mode t)
(smartrep-define-key
    winner-mode-map "C-c" '(("<left>" . 'winner-undo)
                            ("<right>" . 'winner-redo)))

;;; ----------------------------------------------------------------------
;;; nswbuff
;;; ----------------------------------------------------------------------
(use-package nswbuff
  :ensure t
  :commands
  (nswbuff-switch-to-next-buffer nswbuff-switch-to-previous-buffer)
  :bind
  ([C-tab] . nswbuff-switch-to-next-buffer)
  ([C-iso-lefttab] . nswbuff-switch-to-previous-buffer)
  :custom
  (nswbuff-exclude-buffer-regexps
   '("^ .*"
     "^\\*Backtrace\\*"
     "^\\*[Ee]diff.*\\*"
     "^\\*Flycheck.*\\*"
     "^\\*Help\\*"
     "^\\*Ibuffer\\*"
     "^\\*Messages\\*"
     "^\\*Moccur\\*"
     "^\\*Rubocop.*\\*"
     "^\\*ansi-term.*\\*"
     "^\\*helm.*\\*"
     "^\\*magit.*"
     "^\\*rspec-compilation\\*"
     "^magit-process.*"))
  (nswbuff-clear-delay 3)
  (nswbuff-display-intermediate-buffers t)
  :init
  (smartrep-define-key
      global-map "C-x" '(("<left>" . 'nswbuff-switch-to-previous-buffer)
                         ("<right>" . 'nswbuff-switch-to-next-buffer))))

;;; ----------------------------------------------------------------------
;;; emacs-w3m と browse-url の設定
;;; ----------------------------------------------------------------------
(use-package w3m
  :if (executable-find "w3m")
  :ensure t
  :defer t
  :config
  (setq w3m-type 'w3m-ja)
  (setq w3m-use-cookies t)
  (setq w3m-accept-japanese-characters t))

(use-package browse-url
  :defer t
  :commands (browse-url-at-point browse-url-at-mouse)
  :init
  (bind-key "\C-xm" 'browse-url-at-point)
  (if (window-system)
      (bind-key [mouse-3] 'browse-url-at-mouse))
  :config
  (cond ((my-wsl-p)
         (setq browse-url-browser-function 'browse-url-generic)
         (setq browse-url-generic-program  "/init")
         (setq browse-url-generic-args '("/mnt/c/Windows/System32/rundll32.exe" "url.dll,FileProtocolHandler")))
        ((eq window-system 'x)
         (setq browse-url-browser-function 'browse-url-xdg-open))
        ((eq window-system 'w32)
         (setq browse-url-browser-function 'browse-url-default-windows-browser))
        ((fboundp 'w3m-browse-url)
         (setq browse-url-browser-function 'w3m-browse-url))))

;;; ----------------------------------------------------------------------
;;; undo-tree.el
;;; ----------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (bind-key "C-." 'undo-tree-redo))

;;; ----------------------------------------------------------------------
;;; migemo
;;; ----------------------------------------------------------------------
(use-package migemo
  :ensure t
  :config
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
(use-package dired
  :config
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-isearch-filenames t))

(use-package dired-x
  :after dired
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-omit-mode 1))))

(use-package dired-k
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("g" . dired-k))
  :hook (dired-initial-position))

(use-package wdired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode))
  :config
  (advice-add 'wdired-finish-edit
              :after (lambda (&rest args)
                       (deactivate-input-method)
                       (dired-k))))

;;; ----------------------------------------------------------------------
;;; howm
;;; ----------------------------------------------------------------------
(use-package howm
  :ensure t
  :commands (howm-list-all
             howm-list-recent
             howm-list-grep
             howm-keyword-to-kill-ring)
  :bind (("\C-c,," . howm-menu)
         ("\C-c,c" . howm-create))
  :mode ("\\.howm\\'" . howm-mode)
  :custom-face
  (howm-mode-title-face ((t (:foreground "cyan"))))
  (howm-reminder-normal-face ((t (:foreground "deep sky blue"))))
  :config
  (setq howm-compatible-to-ver1dot3 t)
  (setq howm-directory
        (cond ((string-equal system-name "SILVER")
               "D:/Dropbox/Documents/howm/")
              (t
               "~/Dropbox/Documents/howm/")))
  (add-hook 'find-file-hook
            (lambda ()
              (when (and
                     (buffer-file-name)
                     (string-match (expand-file-name howm-directory)
                                   (expand-file-name buffer-file-name)))
                (howm-mode))))
  (setq howm-menu-lang 'ja)
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
  (setq howm-excluded-file-regexp
        "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")
  ;; howmメニューの完了済みToDoは非表示にする
  (setq howm-todo-menu-types "[-+~!]")
  ;; いちいち消すのも面倒なので
  ;; 内容が 0 ならファイルごと削除する
  (defun delete-file-if-no-contents ()
    (when (and
           (buffer-file-name (current-buffer))
           (string-match (expand-file-name howm-directory)
                                   (expand-file-name buffer-file-name))
           (= (point-min) (point-max)))
      (delete-file
       (buffer-file-name (current-buffer)))))
  (add-hook 'after-save-hook #'delete-file-if-no-contents)
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
  (define-key howm-mode-map "\C-c\C-c" 'my-save-and-kill-buffer-howm)
  ;; 日付けの入力が面倒
  (eval-after-load "calendar"
    '(progn
       (define-key calendar-mode-map "\C-m" 'my-insert-day)
       (defun my-insert-day ()
         (interactive)
         (let ((day nil)
               (calendar-date-display-form
                '("[" year "-" (format "%02d" (string-to-int month))
                  "-" (format "%02d" (string-to-int day)) "]")))
           (setq day (calendar-date-string
                      (calendar-cursor-to-date t)))
           (exit-calendar)
           (insert day))))))

;;; ----------------------------------------------------------------------
;;; cc-mode
;;; ----------------------------------------------------------------------
(defconst my-cc-style
  '(
    ;; インデント幅を空白2コ分にする
    (c-basic-offset . 2)
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
            ;; my-cc-stye を登録して有効にする
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
            ;; (electric-pair-mode t)
            (define-key c-mode-base-map "\C-cc" 'compile)
            (define-key c-mode-base-map "\C-h" 'c-electric-backspace)
            (define-key c-mode-base-map "\C-xt" 'ff-find-other-file)
            (define-key c-mode-base-map [mouse-2] 'ff-mouse-find-other-file)))

(setq auto-mode-alist
      (append '(("\\.C\\'"            . c-mode)
                ("\\.[Hh]\\'"         . c++-mode)
                ("\\.[Hh][Pp][Pp]\\'" . c++-mode))
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; hideshow
;;; ----------------------------------------------------------------------
(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  (let ((ruby-mode-hs-info
         '(enh-ruby-mode
           "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do"
           "end"
           "#"
           ruby-move-to-block
           nil)))
    (if (not (member ruby-mode-hs-info hs-special-modes-alist))
        (setq hs-special-modes-alist
              (cons ruby-mode-hs-info hs-special-modes-alist)))))

;;; ----------------------------------------------------------------------
;;; moccur
;;; ----------------------------------------------------------------------
(use-package color-moccur
  :bind (("M-o"         . occur-by-moccur)
         ("C-c C-x C-o" . moccur))
  :config
  (setq moccur-split-word t) ; スペース区切りでAND検索
  (setq moccur-use-migemo t)
  (setq *moccur-buffer-name-exclusion-list*
        '(".+TAGS.+" "\.svn" "*Completions*" "*Messages*" " *migemo*"))
  (add-hook 'dired-mode-hook
            (lambda ()
              (bind-key "O" 'dired-do-moccur dired-mode-map))))

(use-package moccur-edit
  :after color-moccur
  :config
  (defadvice moccur-edit-change-file
      (after save-after-moccur-edit-buffer activate)
    (save-buffer)))

;;; ----------------------------------------------------------------------
;;; dsvn
;;; ----------------------------------------------------------------------
(use-package dsvn
  :ensure t
  :commands
  (svn-status svn-update)
  :config
  (setq svn-status-hide-unmodified t)
  (add-to-list 'process-coding-system-alist '("svn" . utf-8)))

;;; ----------------------------------------------------------------------
;;; magit
;;; ----------------------------------------------------------------------
(use-package magit
  :ensure t
  :defer t
  :bind ("\C-xg" . magit-status)
  :config
  (setq magit-push-always-verify nil)
  (setq magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18))
  (add-to-list 'auto-coding-alist '("COMMIT_EDITMSG" . utf-8-unix))
  (define-key magit-status-mode-map [C-tab] nil)
  (define-key magit-status-mode-map [C-iso-lefttab] nil)
  (define-key magit-diff-mode-map [C-tab] nil)
  (define-key magit-diff-mode-map [C-iso-lefttab] nil))

;;; ----------------------------------------------------------------------
;;; enhanced-ruby-mode
;;; ----------------------------------------------------------------------
(defun ruby-mode-set-frozen-string-literal-true ()
  (when (and
         (or (eq major-mode 'ruby-mode) (eq major-mode 'enh-ruby-mode))
         (buffer-file-name (current-buffer))
         (string-match "\\.rb" (buffer-file-name (current-buffer))))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (if (looking-at "^#!") (beginning-of-line 2))
      (unless (looking-at "^# frozen_string_literal: true")
        (insert "# frozen_string_literal: true\n\n")))))

(defun my-ruby-mode-setup ()
  "Hooks for Ruby mode."
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-deep-indent-paren nil)
  (inf-ruby-minor-mode t)
  ;;(electric-pair-mode t)
  (electric-indent-mode t)
  (electric-layout-mode t)
  (ruby-end-mode t)
  (rubocop-mode t)
  (add-hook 'before-save-hook 'ruby-mode-set-frozen-string-literal-true))

(use-package enh-ruby-mode
  :ensure t
  :defer t
  :interpreter ("ruby")
  :mode ("\\.rb\\'"
         "config\\.ru\\'"
         "\\(Rake\\|Cap\\|Gem\\|Guard\\)file\\'"
         "\\.xremap\\'")
  :config
  (add-hook 'enh-ruby-mode-hook 'my-ruby-mode-setup))

(use-package inf-ruby
  :ensure t
  :defer t)

(use-package company-inf-ruby
  :ensure t
  :after inf-ruby
  :config
  (add-hook 'inf-ruby-mode-hook
            (lambda ()
              (setq-local company-backends
                          (append '(company-inf-ruby) company-backends)))))

(use-package ruby-end
  :ensure t
  :defer t
  :diminish ruby-end-mode)

(use-package rubocop
  :ensure t :defer t)

;; ruby の symbol をいい感じに hippie-expand する
(defun hippie-expand-ruby-symbols (orig-fun &rest args)
  (if (eq major-mode 'enh-ruby-mode)
      (let ((table (make-syntax-table ruby-mode-syntax-table)))
        (modify-syntax-entry ?: "." table)
        (with-syntax-table table (apply orig-fun args)))
    (apply orig-fun args)))
(advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)

;;; ----------------------------------------------------------------------
;;; rspec-mode
;;; ----------------------------------------------------------------------
(use-package rspec-mode
  :ensure t :defer t)

;;; ----------------------------------------------------------------------
;;; python-mode
;;; ----------------------------------------------------------------------
(use-package python-mode
  :defer t
  :mode ("\\.pyw\\'")
  :init
  (setq py-indent-offset 4))

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
;;; php-mode
;;; ----------------------------------------------------------------------
(defun my-php-mode-setup ()
  (php-enable-psr2-coding-style)
  (setq flycheck-phpcs-standard "PSR2")
  ;;(electric-pair-mode t)
  (electric-indent-mode t)
  (electric-layout-mode t)
  (define-key php-mode-map '[(control .)] nil)
  (define-key php-mode-map '[(control c)(control .)] 'php-show-arglist)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  ;;(c-set-offset 'arglist-intro' +)
  (c-set-offset 'arglist-cont-nonempty' +)
  ;;(c-set-offset 'arglist-close' 0)
  (c-set-offset 'case-label +)
  ;;(require 'ac-php)
  ;;(add-to-list 'ac-sources 'ac-source-php)
  ;;(setq ac-sources (remove 'ac-source-dictionary ac-sources))
  )

(use-package php-mode
  :disabled t
  :ensure t
  :magic "\\`<\\?php$"
  :config
  (add-hook 'php-mode-hook 'my-php-mode-setup))

(use-package php-align
  :after php-mode
  :config
  (php-align-setup))

;;; ----------------------------------------------------------------------
;;; web-mode
;;; ----------------------------------------------------------------------
(defun my-web-mode-setup ()
  (setq-local company-backends
              (append '(company-web-html) company-backends))
  (define-key web-mode-map (kbd "C-'") 'company-web-html)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (when (string-match "\\.erb" (buffer-file-name (current-buffer)))
    (modify-syntax-entry ?% "w" web-mode-syntax-table))
  (when (string-match "\\.php" (buffer-file-name (current-buffer)))
    (modify-syntax-entry ?? "w" web-mode-syntax-table)))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.html\\.erb\\'"
         "\\.rhtml?\\'"
         "\\.php\\'")
  :config
  (add-hook 'web-mode-hook 'my-web-mode-setup))

;;; ----------------------------------------------------------------------
;;; js2-mode
;;; ----------------------------------------------------------------------
(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)

(use-package js2-mode
  :ensure t
  :defer t
  :config
  (add-hook 'js2-mode-hook
            (lambda()
              (setq js2-basic-offset 2)
              (electric-indent-mode t)
              (setq-local electric-layout-rules
                          '(
                            ;; (?\{ . after)
                            ;; (?\} . before)
                            ;; (?\; . after)
                            ))
              )))

;;; ----------------------------------------------------------------------
;;; rjsx-mode
;;; ----------------------------------------------------------------------
(use-package rjsx-mode
  :ensure t
  :mode (".*\\.jsx\\'" ".*\\.js\\'"))

;;; ----------------------------------------------------------------------
;;; add-node-module-path
;;; ----------------------------------------------------------------------
(use-package add-node-modules-path
  :ensure t
  :defer t
  :init
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path))
  (eval-after-load 'rjsx-mode
    '(add-hook 'rjsx-mode-hook #'add-node-modules-path)))

;;; ----------------------------------------------------------------------
;;; json-mode
;;; ----------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.babelrc\\'" "\\.eslintrc\\'"))

;;; ----------------------------------------------------------------------
;;; for json format
;;; ----------------------------------------------------------------------
(defun jq-format (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;; ----------------------------------------------------------------------
;;; coffee-mode
;;; ----------------------------------------------------------------------
(use-package coffee-mode
  :ensure t
  :mode ("\\.coffee\\'" "\\.coffee\\.erb\\'")
  :config
  (add-hook 'coffee-mode-hook
            '(lambda()
               (setq-local tab-width 2)
               (setq coffee-tab-width 2))))

;;; ----------------------------------------------------------------------
;;; typescript-mode
;;; ----------------------------------------------------------------------
(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq typescript-indent-level 2)
              (electric-indent-mode t)
              (setq-local electric-layout-rules
                          '(
                            ;; (?\{ . after)
                            ;; (?\} . before)
                            ;; (?\; . after)
                            ))
              )))

;;; ----------------------------------------------------------------------
;;; tide
;;; ----------------------------------------------------------------------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :defer t
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;;; ----------------------------------------------------------------------
;;; less-css-mode / scss-mode
;;; ----------------------------------------------------------------------
(defun my-css-mode-setup ()
  (electric-indent-mode t)
  (electric-layout-mode t)
  (setq-local electric-layout-rules
              '((?\{ . after) (?\} . before)))
  (setq-local company-backends
              (append '(company-css) company-backends)))

(use-package less-css-mode
  :ensure t
  :defer t
  :config
  (setq less-css-compile-at-save nil)
  (add-hook 'less-css-mode-hook 'my-css-mode-setup))

(use-package scss-mode
  :ensure t
  :defer t
  :config
  (setq scss-compile-at-save nil)
  (add-hook 'scss-mode-hook 'my-css-mode-setup)
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode)))

;;; ----------------------------------------------------------------------
;;; csharp-mode
;;; ----------------------------------------------------------------------
(use-package csharp-mode
  :ensure t
  :defer t)

;;; ----------------------------------------------------------------------
;;; po-mode
;;; ----------------------------------------------------------------------
(use-package po-mode
  ;; :ensure t ; el-get
  :defer t
  :mode ("\\.po\\'\\|\\.po\\." . po-mode)
  :commands (po-find-file-coding-system)
  :init (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                                    'po-find-file-coding-system))

;;; ----------------------------------------------------------------------
;;; es-mode
;;; ----------------------------------------------------------------------
(use-package es-mode
  :ensure t
  :defer t
  :mode ("\\.es$" . es-mode))

;;; ----------------------------------------------------------------------
;;; mmm-mode
;;; ----------------------------------------------------------------------
(use-package mmm-mode
  :disabled t
  :ensure t
  :config
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
  ;;(mmm-add-mode-ext-class nil "\\.erb\\'" 'mmm-eruby-mode)
  ;;(mmm-add-mode-ext-class nil "\\.rhtml\\'" 'mmm-eruby-mode)
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'mmm-php-mode)
  )

;;; ----------------------------------------------------------------------
;;; latex-mode
;;; ----------------------------------------------------------------------
(add-hook 'latex-mode-hook
          (lambda ()
            (setq tex-verbatim-face nil)
            (defun tex-font-lock-suscript () nil)))

;;; ----------------------------------------------------------------------
;;; rinari
;;; ----------------------------------------------------------------------
(use-package rinari
  :ensure t
  :config
  (setq rinari-exclude-major-modes
        '(magit-status-mode
          magit-log-edit-mode))
  (global-rinari-mode t))

;;; ----------------------------------------------------------------------
;;; editorconfig
;;; ----------------------------------------------------------------------
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;; ----------------------------------------------------------------------
;;; その他の major-mode
;;; ----------------------------------------------------------------------
(use-package yaml-mode
  :ensure t :defer t)

(use-package lua-mode
  :ensure t :defer t)

(use-package ini-mode
  :ensure t :defer t)

(use-package dockerfile-mode
  :ensure t :defer t)

(use-package vcl-mode
  :ensure t :defer t)

(use-package nginx-mode
  :ensure t :defer t
  :mode ("nginx\\(.*\\).conf[^/]*$"))

(use-package logstash-conf
  :ensure t :defer t)

;;; ----------------------------------------------------------------------
;;; その他の拡張子に対応する編集モードを設定
;;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ("\\.[ch]java\\'"          . java-mode)     ;; i-appli
                ("\\.doc\\'"               . text-mode)
                ("\\.text\\.erb\\'"        . text-mode)     ;; Text(erb)
                ("\\.rtext\\'"             . text-mode)     ;; Text(erb)
                )
              auto-mode-alist))

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
;;; ~のつくバックアップファイルaaの保存場所の指定
;;; ----------------------------------------------------------------------
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
             (cons "\\.*$" (expand-file-name "~/bak")))

;;; ----------------------------------------------------------------------
;;; recentf / recentf-ext
;;; ----------------------------------------------------------------------
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores"))
(setq recentf-auto-cleanup 'never)
;; http://qiita.com/itiut@github/items/d917eafd6ab255629346
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(setq recentf-auto-save-timer
      (run-with-idle-timer 30 t '(lambda ()
                                   (with-suppressed-message (recentf-save-list)))))

(use-package recentf-ext
  :ensure t)

;;; ----------------------------------------------------------------------
;;; session
;;; ----------------------------------------------------------------------
(use-package session
  :ensure t
  :hook (after-init . session-initialize)
  :init
  (setq history-length t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 1000)
                                  (session-file-alist 1000 t)
                                  (file-name-history 10000)))
  (setq session-globals-max-string 10000000)
  ;; anything/helmと一緒に使うために必要
  (setq session-save-print-spec '(t nil 40000)))

;;; ----------------------------------------------------------------------
;;; desktop
;;; ----------------------------------------------------------------------
(use-package desktop
  :ensure t
  :config
  (setq desktop-restore-eager 10)
  (desktop-save-mode 1))

;;; ----------------------------------------------------------------------
;;; elscreen
;;; ----------------------------------------------------------------------
(use-package elscreen
  :ensure t
  :init
  (setq elscreen-display-tab nil)
  :config
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
                                       ("n" . 'elscreen-next)
                                       ("<left>" . 'elscreen-previous)
                                       ("<right>" . 'elscreen-next))))

;;; ----------------------------------------------------------------------
;;; flycheck
;;; ----------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-gcc-language-standard "c++11")
  (setq flycheck-clang-language-standard "c++11")
  (setq flycheck-disabled-checkers
        (append '(
                  ;;python-flake8
                  ;;python-pylint
                  ruby-rubylint
                  javascript-jshint
                  javascript-jscs
                  )
                flycheck-disabled-checkers)))

(use-package flycheck-pyflakes
  :ensure t
  :after flycheck)

(use-package flycheck-posframe
  :if (window-system)
  :ensure t
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-border-width 1)
  :custom-face
  (flycheck-posframe-border-face ((t (:foreground "gray30"))))
  :config
  (set-face-background 'flycheck-posframe-background-face monokai-highlight-line))

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
(use-package bm
  :ensure t
  :config
  (setq-default bm-buffer-persistence t)
  (setq bm-repository-file "~/.emacs.d/.bm-repository")
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hook 'bm-buffer-restore)
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
;;; projectile
;;; ----------------------------------------------------------------------
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t))

;;; ----------------------------------------------------------------------
;;; helm
;;; ----------------------------------------------------------------------
(use-package helm
  :ensure t
  :diminish (helm-mode helm-migemo-mode)
  :bind
  ("C-;"     . helm-mini)
  ("C-c ;"   . helm-mini)
  ("M-x"     . helm-M-x)
  ("C-x b"   . helm-buffers-list)
  ("M-y"     . helm-show-kill-ring)
  ("C-x C-d" . helm-browse-project)
  ("C-x C-r" . helm-recentf)
  :config
  (require 'helm-config)
  (require 'helm-buffers)
  (require 'helm-files)
  (helm-migemo-mode +1)
  (setq helm-idle-delay 0.3)
  (setq helm-input-idle-delay 0.2)
  (setq helm-candidate-number-limit 100)
  (setq helm-buffer-max-length 50)
  (setq helm-truncate-lines t)
  (setq helm-inherit-input-method nil)
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-recentf
          helm-source-files-in-current-dir
          helm-source-buffer-not-found
          )))

(use-package helm-descbinds
  :ensure t
  :after helm
  :config (helm-descbinds-mode +1))

(use-package helm-projectile
  :ensure t
  :diminish projectile-mode
  :bind
  ("C-x C-p" . helm-projectile)
  ("C-x p"   . helm-projectile-switch-project)
  :config
  (helm-projectile-on))

(use-package helm-bm
  :ensure t
  :bind ("C-c b" . helm-bm))

(use-package helm-ls-git
  :ensure t
  :after helm)

(use-package helm-git-grep
  :ensure t
  :bind ("C-x C-g" . helm-git-grep))

(use-package helm-c-yasnippet
  :ensure t
  :bind ("C-c y" . helm-yas-complete)
  :config
  (setq helm-yas-space-match-any-greedy t))

(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :hook (prog-mode . helm-gtags-mode)
  :config
  (setq helm-gtags-auto-update t)
  (bind-key "M-t" 'helm-gtags-find-tag    helm-gtags-mode-map)
  (bind-key "M-r" 'helm-gtags-find-rtag   helm-gtags-mode-map)
  (bind-key "M-s" 'helm-gtags-find-symbol helm-gtags-mode-map)
  (bind-key "M-," 'helm-gtags-pop-stack   helm-gtags-mode-map)
  (smartrep-define-key
      helm-gtags-mode-map "C-c" '(("<" . 'helm-gtags-previous-history)
                                  (">" . 'helm-gtags-next-history))))

(use-package helm-flycheck
  :ensure t
  :after (flycheck)
  :config
  (bind-key "C-c ! h" 'helm-flycheck flycheck-mode-map))

(use-package helm-swoop
  :ensure t
  :defer t
  :init
  (setq helm-multi-swoop-edit-save t)
  (bind-key "M-i" 'helm-swoop)
  (bind-key "M-I" 'helm-swoop-back-to-last-point)
  (bind-key "C-c M-i" 'helm-multi-swoop)
  (bind-key "C-x M-i" 'helm-multi-swoop-all)
  (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
  :config
  (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
  )

(use-package helm-ag
  :ensure t
  :custom
  (helm-ag-base-command "rg --vimgrep --no-heading")
  (helm-ag-success-exit-status '(0 2))
  (helm-ag-insert-at-point 'symbol))

;;; ----------------------------------------------------------------------
;;; ivy
;;; ----------------------------------------------------------------------
;; (use-package ivy
;;   :ensure t
;;   :init
;;   (setq ivy-truncate-lines nil)
;;   (setq ivy-wrap t)
;;   :config
;;   (bind-key "<escape>" 'minibuffer-keyboard-quit ivy-minibuffer-map)
;;   (ivy-mode 1))

;; (use-package ivy-hydra
;;   :ensure t
;;   :config
;;   (setq ivy-read-action-function #'ivy-hydra-read-action))

;; (use-package counsel
;;   :ensure t
;;   :defer t
;;   :bind (
;;          ;; ("M-x" . counsel-M-x)
;;          ;; ("M-y" . counsel-yank-pop)
;;          ))

;; (use-package swiper
;;   :ensure t
;;   :defer t
;; )

;;; ----------------------------------------------------------------------
;;; anzu
;;; ----------------------------------------------------------------------
(use-package anzu
  :ensure t
  :config
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (setq anzu-use-migemo t)
  (global-anzu-mode t)
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;;; ----------------------------------------------------------------------
;;; gist
;;; ----------------------------------------------------------------------
(use-package gist
  :ensure t
  :defer t)

;;; ----------------------------------------------------------------------
;;; popwin.el
;;; ----------------------------------------------------------------------
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (setq pop-up-windows nil)
  ;;(setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:adjust-other-windows t)
  (setq popwin:special-display-config
        (append '(("*Backtrace*" :height 0.3)
                  ("*Kill Ring*" :height 0.4 :noselect t)
                  ("*Apropos*" :height 0.4)
                  ("*Help*" :height 0.4)
                  ("*sdic*" :height 0.3)
                  ("*Warnings*" :height 0.3)
                  ("*Google Translate*" :height 0.3)
                  ("^\\*helm" :regexp t :height 0.4)
                  ;;("\\*ag search.*\\*" :dedicated t :regexp t :height 0.4)
                  ("*git-gutter:diff*" :height 0.4 :stick t)
                  (" *auto-async-byte-compile*" :dedicated t :noselect t :height 0.2)
                  ;;("*rspec-compilation*" :height 0.4 :stick t :regexp t)
                  ;;(dired-mode :height 0.4 :position top)
                  )
                popwin:special-display-config))
  (define-key global-map (kbd "C-c l") 'popwin:display-last-buffer))

;;; ----------------------------------------------------------------------
;;; git-gutter.el
;;; ----------------------------------------------------------------------
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
  (run-with-idle-timer 1 t 'git-gutter)
  (global-git-gutter-mode t)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  (smartrep-define-key
      global-map "C-x v" '(("p" . 'git-gutter:previous-hunk)
                           ("n" . 'git-gutter:next-hunk))))

;;; ----------------------------------------------------------------------
;;; multi-term
;;; ----------------------------------------------------------------------
(use-package multi-term
  :disabled t
  :ensure t
  :defer t
  :config
  (setq multi-term-program shell-file-name)
  (defun my-term-toggle-line-char-mode ()
    (interactive)
    (if (term-in-line-mode) (term-char-mode) (term-line-mode)))
  (defun term-send-ctrl-z ()
    "Allow using ctrl-z to suspend in multi-term shells."
    (interactive)
    (term-send-raw-string "\C-z"))
  (defun term-send-ctrl-r ()
    "Allow using ctrl-z to suspend in multi-term shells."
    (interactive)
    (term-send-raw-string "\C-r"))
  (add-hook 'term-mode-hook
            (lambda()
              (add-to-list 'term-bind-key-alist '("C-z z" . term-send-ctrl-z))
              (bind-key "C-r" 'term-send-ctrl-r term-raw-map)
              (bind-key "ESC <C-return>" 'my-term-toggle-line-char-mode term-raw-map)
              (bind-key "ESC <C-return>" 'my-term-toggle-line-char-mode term-mode-map)
              )))

;;; ----------------------------------------------------------------------
;;; shell-pop
;;; ----------------------------------------------------------------------
(use-package shell-pop
  :disabled t
  :ensure t
  :defer t
  :bind ("<f12>" . shell-pop)
  :init
  (setq shell-pop-shell-type '("multi-term" "*terminal*" (lambda () (multi-term))))
  (setq shell-pop-window-position "bottom"))

;;; ----------------------------------------------------------------------
;;; vterm
;;; ----------------------------------------------------------------------
(use-package vterm
  :ensure t
  :defer t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-keymap-exceptions
   '("C-c"
     ;; "C-x"
     "C-u"
     "C-g"
     ;; "C-h"
     "C-l"
     ;; "M-x"
     "M-o"
     "C-v"
     "M-v"
     "C-y"
     "M-y"
     "<f12>")))

;;; ----------------------------------------------------------------------
;;; vterm-toggle
;;; ----------------------------------------------------------------------
(use-package vterm-toggle
  :ensure t
  :defer t
  :bind (([f12] . vterm-toggle)
         ([C-f12] . vterm-toggle-cd))
  :custom
  (vterm-toggle-scope 'project)
  :config
  ;; Show vterm buffer in the window located at bottom
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4))))

;;; ----------------------------------------------------------------------
;;; whitespace-mode
;;; ----------------------------------------------------------------------
(use-package whitespace
  :diminish global-whitespace-mode
  :custom-face
  (whitespace-space ((t (:italic nil))))
  (whitespace-newline ((t (:foreground "#335544" :bold t))))
  :config
  (setq whitespace-style
        '(face
          tabs spaces newline trailing space-before-tab space-after-tab
          space-mark tab-mark newline-mark))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (setq whitespace-display-mappings
        '(
          ;;(space-mark   ?\u3000 [?□] [?＿])          ; full-width space - square
          ;;(newline-mark ?\n    [?« ?\n] [?$ ?\n])    ; eol - left guillemet
          (newline-mark ?\n    [?↵ ?\n] [?$ ?\n])    ; eol - downwards arrow
          (tab-mark     ?\t    [?» ?\t] [?\\ ?\t])   ; tab - right guillemet
          ))
  ;;(set-face-italic-p 'whitespace-space nil)
  ;;(set-face-foreground 'whitespace-newline "#335544")
  ;;(set-face-bold-p 'whitespace-newline t)
  (setq whitespace-global-modes '(not dired-mode tar-mode magit-log-mode vterm-mode))
  (global-whitespace-mode 1))

;;; ----------------------------------------------------------------------
;;; google-translate.el
;;; ----------------------------------------------------------------------
(use-package google-translate
  :ensure t
  :defer t
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  :bind ("\C-ct" . google-translate-smooth-translate))

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
;;; delete-trailing-whitespace の hook の状態をモードラインに表示する
;;; http://syohex.hatenablog.com/entry/20130617/1371480584
;;; ----------------------------------------------------------------------
(defvar my/current-cleanup-state "")
;; 行末のスペース + ファイル末尾の連続する改行の除去を行う
(defun my/cleanup-for-spaces ()
  (interactive)
  (delete-trailing-whitespace)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))
(add-hook 'before-save-hook 'my/cleanup-for-spaces)
(setq-default mode-line-format
              (cons '(:eval my/current-cleanup-state)
                    mode-line-format))
(defun toggle-cleanup-spaces ()
  (interactive)
  (cond ((memq 'my/cleanup-for-spaces before-save-hook)
         (setq my/current-cleanup-state
               (propertize "[DT-]" 'face '((:foreground "turquoise1" :weight bold))))
         (remove-hook 'before-save-hook 'my/cleanup-for-spaces))
        (t
         (setq my/current-cleanup-state "")
         (add-hook 'before-save-hook 'my/cleanup-for-spaces)))
  (force-mode-line-update))
(global-set-key (kbd "C-c M-d") 'toggle-cleanup-spaces)

;;; ----------------------------------------------------------------------
;;; ファイルをシステムの関連付けで開く
;;; ----------------------------------------------------------------------
(defun my-file-open-by-windows (file)
  "ファイルをシステムの関連付けで開く"
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
(use-package open-junk-file
  :ensure t
  :defer t
  :bind ("\C-x\C-z" . open-junk-file))

;;; ----------------------------------------------------------------------
;;; lispxmp
;;; ----------------------------------------------------------------------
(use-package lispxmp
  :ensure t
  :commands lispxmp
  :init
  (define-key emacs-lisp-mode-map "\C-c\C-d" 'lispxmp))

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
;; (when (require 'auto-async-byte-compile nil t)
;;   (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;;   (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
(use-package auto-async-byte-compile
  :ensure t
  :config
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  :hook
  (emacs-lisp-mode-hook . enable-auto-async-byte-compile-mode))

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
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :hook (prog-mode . highlight-symbol-mode)
  :bind (([(control f3)] . highlight-symbol-at-point)
         ([f3]           . highlight-symbol-next)
         ([(shift f3)]   . highlight-symbol-prev)
         ([(meta f3)]    . highlight-symbol-query-replace)))

;;; ----------------------------------------------------------------------
;;; highlight-indent-guides
;;; ----------------------------------------------------------------------
(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :hook ((yaml-mode python-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-responsive t)
  (setq highlight-indent-guides-method 'character))

;;; ----------------------------------------------------------------------
;;; rainbow-mode
;;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((prog-mode text-mode) . rainbow-mode))

;;; ----------------------------------------------------------------------
;;; rainbow-delimiters
;;; ----------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; ----------------------------------------------------------------------
;;; wgrep
;;; ----------------------------------------------------------------------
(use-package wgrep
  :ensure t
  :defer t
  :init
  (setq wgrep-enable-key "r")
  (setq wgrep-auto-save-buffer t))

;;; ----------------------------------------------------------------------
;;; ag / wgrep-ag
;;; ----------------------------------------------------------------------
(use-package ag
  :ensure t
  :bind
  (("\C-ca" . ag)
   ("\C-c\C-a" . ag-project))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :ensure t
  :hook (ag-mode . wgrep-ag-setup)
  :config (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode))

;;; ----------------------------------------------------------------------
;;; ripgrep
;;; ----------------------------------------------------------------------
(use-package rg
  :ensure t
  :bind
  (("\C-c\C-s" . rg))
  :config
  (rg-enable-default-bindings))

;;; ----------------------------------------------------------------------
;;; tempbuf
;;; ----------------------------------------------------------------------
(use-package tempbuf
  :hook ((dired-mode
          magit-mode
          custom-mode-hook
          w3-mode-hook
          Man-mode-hook
          view-mode-hook
          helm-major-mode-hook)
         . turn-on-tempbuf-mode)
  :custom
  (tempbuf-kill-message nil)
  :init
  (add-hook 'compilation-mode-hook
            (lambda ()
              (when (string-match "*RuboCop " (buffer-name))
                'turn-on-tempbuf-mode)))
  (add-hook 'fundamental-mode-hook
            (lambda ()
              (when (string-match "*Flycheck error messages*" (buffer-name))
                'turn-on-tempbuf-mode))))

;;; ----------------------------------------------------------------------
;;; buffer-move
;;; ----------------------------------------------------------------------
(use-package buffer-move
  :ensure t
  :bind (([C-S-up]     . buf-move-up)
         ([C-S-down]   . buf-move-down)
         ([C-S-left]   . buf-move-left)
         ([C-S-right]  . buf-move-right)))

;;; ----------------------------------------------------------------------
;;; nyan-mode
;;; ----------------------------------------------------------------------
(use-package nyan-mode
  :ensure t
  :hook after-init
  :config
  (setq nyan-bar-length 16)
  (nyan-start-animation))

;;; ----------------------------------------------------------------------
;;; all-the-icons
;;; ----------------------------------------------------------------------
(use-package all-the-icons
  :ensure t
  :custom
  (all-the-icons-scale-factor 1.0))

;;; ----------------------------------------------------------------------
;;; all-the-icons-dired
;;; ----------------------------------------------------------------------
(use-package all-the-icons-dired
  :ensure t
  :diminish all-the-icons-dired-mode
  :hook (dired-mode . all-the-icons-dired-mode))

;;; ----------------------------------------------------------------------
;;; all-the-icons-ibuffer
;;; ----------------------------------------------------------------------
(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

;;; ----------------------------------------------------------------------
;;; neotree
;;; ----------------------------------------------------------------------
(use-package neotree
  :ensure t
  :after
  projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'icons)
  :bind
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

;;; ----------------------------------------------------------------------
;;; hide-mode-line
;;; ----------------------------------------------------------------------
(use-package hide-mode-line
  :ensure t
  :hook
  ((neotree-mode imenu-list-minor-mode) . hide-mode-line-mode))

;;; ----------------------------------------------------------------------
;;; 終了前に確認する
;;; ----------------------------------------------------------------------
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;;; ----------------------------------------------------------------------
;;; その他のキーバインド
;;; ----------------------------------------------------------------------
(find-function-setup-keys)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
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
(put 'dired-find-alternate-file 'disabled nil)

;;; ----------------------------------------------------------------------
(if (not (eq window-system 'w32))
    (use-package exec-path-from-shell
      :ensure t
      :config (exec-path-from-shell-initialize)))
(cd "~")

;;; end of file ;;;
