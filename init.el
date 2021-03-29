;;; -*- mode: lisp-interaction; coding: utf-8-unix -*-

;;; ----------------------------------------------------------------------
;;; 基本設定
;;; ----------------------------------------------------------------------
(custom-set-variables
 '(inhibit-startup-message t)
 '(scroll-conservatively 1)
 '(next-line-add-newlines nil)
 '(kill-whole-line t)
 '(case-replace t)
 '(transient-mark-mode t)
 '(indent-line-function 'indent-relative-maybe)
 '(truncate-partial-width-windows nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(line-move-visual nil)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(blink-matching-paren nil)
 '(confirm-kill-emacs nil)
 '(indicate-empty-lines t)
 '(mode-line-frame-identification " ")
 '(line-number-mode t)
 '(column-number-mode t)
 '(require-final-newline t)
 '(mode-require-final-newline t)
 '(ring-bell-function 'ignore)
 '(search-default-regexp-mode nil)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally)
 '(use-dialog-box nil)
 '(compilation-scroll-output 'first-error))

(temp-buffer-resize-mode t)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(delete-selection-mode t)
(show-paren-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

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
(defun my/wsl-p ()
  (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))

(when (my/wsl-p)
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
;; ■□◆◇■□◆◇……

(setq use-default-font-for-symbols nil)
(set-face-attribute 'default nil :family "Cica" :height 150)
(dolist (c '(?… ?■ ?□ ?◆ ?◇))
  (set-fontset-font t c "Ricty"))
(set-fontset-font t '(#x1F000 . #x1FAFF) "Noto Color Emoji")
(add-to-list 'face-font-rescale-alist '(".*Noto Color Emoji.*" . 0.82))

;;; ----------------------------------------------------------------------
;;; setting up package.el
;;; ----------------------------------------------------------------------
(custom-set-variables
 '(package-user-dir "~/.emacs.d/elpa")
 '(package-archives
   '(("gnu"   . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org"   . "https://orgmode.org/elpa/"))))
(require 'package)
(package-initialize)

;;; ----------------------------------------------------------------------
;;; install straight.el
;;; ----------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; ----------------------------------------------------------------------
;;; use-package
;;; ----------------------------------------------------------------------
(straight-use-package 'use-package)
(straight-use-package 'diminish)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(custom-set-variables
 '(use-package-verbose t)
 '(use-package-minimum-reported-time 0.001)
 '(use-package-compute-statistics t)
 '(use-package-enable-imenu-support t))

;;; ----------------------------------------------------------------------
;;; paradox
;;; ----------------------------------------------------------------------
(use-package paradox
  :ensure t
  :defer t
  :init (use-package async :ensure t)
  :custom
  (paradox-github-token t)
  (paradox-execute-asynchronously t)
  (paradox-automatically-star t))

;;; ---------------------------------------------------------------------
;;; monokai-theme
;;; ----------------------------------------------------------------------
(use-package monokai-theme
  :disabled t
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
                        :foreground monokai-foreground))
  (with-eval-after-load 'flycheck-posframe
    (set-face-background 'flycheck-posframe-background-face monokai-highlight-line)))

;;; ---------------------------------------------------------------------
;;; solarized-theme
;;; ----------------------------------------------------------------------
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark-high-contrast t)
  (custom-set-faces
   '(mode-line ((t (:underline nil))))
   '(mode-line-inactive ((t (:underline nil)))))
  (with-eval-after-load 'mozc-cand-posframe
    (set-face-attribute 'mozc-cand-posframe-normal-face nil
                        :background 'unspecified :foreground 'unspecified
                        :inherit 'company-tooltip)
    (set-face-attribute 'mozc-cand-posframe-focused-face nil
                        :background 'unspecified :foreground 'unspecified
                        :inherit 'company-tooltip-selection)
    (set-face-attribute 'mozc-cand-posframe-footer-face nil
                        :background 'unspecified :foreground 'unspecified
                        :inherit '(company-tooltip-annotation company-tooltip))))

;;; ----------------------------------------------------------------------
;;; W32-IME / mozc / ibus / uim
;;; ----------------------------------------------------------------------
(defun my-w32-ime-init()
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq-default w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (use-package tr-ime
    :ensure t
    :config (tr-ime-advanced-install))
  (w32-ime-initialize)
  ;; IME 制御（yes/no などの入力の時に IME を off にする）MELPA 掲載版用
  (w32-ime-wrap-function-to-control-ime 'universal-argument)
  (w32-ime-wrap-function-to-control-ime 'read-string)
  (w32-ime-wrap-function-to-control-ime 'read-char)
  (w32-ime-wrap-function-to-control-ime 'read-from-minibuffer)
  (w32-ime-wrap-function-to-control-ime 'y-or-n-p)
  (w32-ime-wrap-function-to-control-ime 'yes-or-no-p)
  (w32-ime-wrap-function-to-control-ime 'map-y-or-n-p)
  (w32-ime-wrap-function-to-control-ime 'register-read-with-preview)
  (modify-all-frames-parameters '((ime-font . "Cica-14")))

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
    (when (my/wsl-p)
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
    :custom (mozc-candidate-style 'popup))

  (use-package mozc-cand-posframe
    :if (window-system)
    :ensure t
    :after mozc
    :custom (mozc-candidate-style 'posframe))

  (use-package mozc-el-extensions
    :straight (:host github :repo "iRi-E/mozc-el-extensions")
    :after mozc
    :no-require t
    :config
    (require 'mozc-cursor-color)
    (setq mozc-cursor-color-alist '((direct        . "green")
                                    (read-only     . "yellow")
                                    (hiragana      . "red")
                                    (full-katakana . "goldenrod")
                                    (half-ascii    . "dark orchid")
                                    (full-ascii    . "orchid")
                                    (half-katakana . "dark goldenrod")))))

(cond ((eq window-system 'w32)
       (my-w32-ime-init))
      (t
       (my-mozc-init)))

;;; ----------------------------------------------------------------------
;;; yasnippet.el
;;; ----------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  ;; Remove Yasnippet's default tab key binding
  (bind-keys :map yas-minor-mode-map
             ("<tab>" . nil)
             ("TAB" . nil))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;;; ----------------------------------------------------------------------
;;; abbrev/dabbrev
;;; ----------------------------------------------------------------------
(setq save-abbrevs t)
(setq abbrev-file-name (expand-file-name "~/.emacs.d/.abbrev_defs"))
(quietly-read-abbrev-file)
(setq-default abbrev-mode nil)

;;; ----------------------------------------------------------------------
;;; hippie-expand
;;; ----------------------------------------------------------------------
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(custom-set-variables
 '(hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-complete-abbrev
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))
 '(dabbrev-case-fold-search t)
 '(dabbrev-case-replace t))
(bind-key "/" 'hippie-expand esc-map)

;;; ----------------------------------------------------------------------
;;; company-mode
;;; ----------------------------------------------------------------------
(use-package company
  :ensure t
  :diminish company-mode
  :custom
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend
                       company-echo-metadata-frontend))
  (company-require-match 'never)
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-auto-expand t)
  :config
  (global-company-mode +1)
  (define-key company-active-map (kbd "TAB")   'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-mode-map (kbd "M-TAB") 'company-complete))

(use-package company-statistics
  :ensure t
  :custom
  (company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  :config
  (company-statistics-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

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
  :defer t)

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
(use-package windmove
  :config (windmove-default-keybindings))

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
;;; hydra
;;; ----------------------------------------------------------------------
(use-package hydra :ensure t)
(defhydra hydra-resize-window nil
  "Resize Window"
  ("<up>" enlarge-window "enlarge vertically")
  ("<down>" shrink-window "shrink vertically")
  ("<left>" shrink-window-horizontally "shrink horizontally")
  ("<right>" enlarge-window-horizontally "enlarge horizontally"))

;;; ----------------------------------------------------------------------
;;; iflipb
;;; ----------------------------------------------------------------------
(use-package iflipb
  :ensure t
  :commands (iflipb-next-buffer iflipb-previous-buffer iflipb-kill-buffer)
  :init
  (defhydra hydra-buff (global-map "C-x")
    "iflipb"
    ("<left>" iflipb-previous-buffer "previous buffer")
    ("<right>" iflipb-next-buffer "next buffer")))

;;; ----------------------------------------------------------------------
;;; winner-mode
;;; ----------------------------------------------------------------------
(use-package winner
  :custom (winner-dont-bind-my-keys t)
  :config
  (winner-mode t)
  (defhydra hydra-winner (winner-mode-map "C-c")
    "Winner"
    ("<left>" winner-undo "undo")
    ("<right>" winner-redo "redo")))

;;; ----------------------------------------------------------------------
;;; nswbuff
;;; ----------------------------------------------------------------------
;; (use-package nswbuff
;;   :ensure t
;;   :commands
;;   (nswbuff-switch-to-next-buffer nswbuff-switch-to-previous-buffer)
;;   :bind
;;   ([C-tab] . nswbuff-switch-to-next-buffer)
;;   ([C-iso-lefttab] . nswbuff-switch-to-previous-buffer)
;;   :custom
;;   (nswbuff-exclude-buffer-regexps
;;    '("^ .*"
;;      "^\\*Backtrace\\*"
;;      "^\\*[Ee]diff.*\\*"
;;      "^\\*Flycheck.*\\*"
;;      "^\\*Help\\*"
;;      "^\\*Ibuffer\\*"
;;      "^\\*Messages\\*"
;;      "^\\*Moccur\\*"
;;      "^\\*Rubocop.*\\*"
;;      "^\\*ansi-term.*\\*"
;;      "^\\*helm.*\\*"
;;      "^\\*rspec-compilation\\*"
;;      "^\\*magit.*"
;;      "^magit-process.*"
;;      "^\\*git-gutter:diff\\*"
;;      "^\\*straight-process\\*"
;;      "^\\*Calendar\\*"
;;      "^\\*Paradox Report\\*"))
;;   (nswbuff-clear-delay 3)
;;   (nswbuff-display-intermediate-buffers t))

;;; ----------------------------------------------------------------------
;;; emacs-w3m と browse-url の設定
;;; ----------------------------------------------------------------------
(use-package w3m
  :if (executable-find "w3m")
  :ensure t
  :defer t
  :custom (w3m-use-cookies t))

(use-package browse-url
  :defer t
  :commands (browse-url-at-point browse-url-at-mouse)
  :init
  (bind-key "\C-xm" 'browse-url-at-point)
  (if (window-system)
      (bind-key [mouse-3] 'browse-url-at-mouse))
  :config
  (cond ((my/wsl-p)
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
  :bind ("C-." . 'undo-tree-redo)
  :config (global-undo-tree-mode))

;;; ----------------------------------------------------------------------
;;; migemo
;;; ----------------------------------------------------------------------
(use-package migemo
  :ensure t
  :hook (after-init . migemo-init)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary (cond ((eq window-system 'w32)
                            "~/scoop/apps/cmigemo/current/cmigemo-default-win32/dict/utf-8/migemo-dict")
                           (t
                            "/usr/share/cmigemo/utf-8/migemo-dict")))
  (migemo-coding-system 'utf-8-unix)
  (migemo-use-pattern-alist nil)
  (migemo-use-frequent-pattern-alist t)
  (migemo-pattern-alist-length 1024)
  (migemo-isearch-min-length 2))

;;; ----------------------------------------------------------------------
;;; dired 関係
;;; ----------------------------------------------------------------------
(use-package dired-x
  :custom
  (ls-lisp-ignore-case t)
  (ls-lisp-dirs-first t)

  (dired-listing-switches "-aFl --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-isearch-filenames t)
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
;;; Dropbox のパス
;;; ----------------------------------------------------------------------
(use-package f :ensure t)
(setq my-dropbox-directory
      (cond ((string-equal system-name "SILVER")
             "D:/Dropbox/")
            (t
             "~/Dropbox/")))

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
  :custom
  (howm-directory (f-join my-dropbox-directory "Documents/howm"))
  (howm-menu-lang 'ja)
  (howm-process-coding-system 'utf-8)
  ;; 「最近のメモ」一覧時にタイトル表示
  (howm-list-recent-title t)
  ;; 全メモ一覧時にタイトル表示
  (howm-list-all-title t)
  ;; メニューを 2 時間キャッシュ
  (howm-menu-expiry-hours 2)
  ;; howm の時は auto-fill で
  ;; (add-hook 'howm-mode-on-hook 'auto-fill-mode)
  ;; RET でファイルを開く際, 一覧バッファを消す
  ;; C-u RET なら残る
  (howm-view-summary-persistent nil)
  ;; 検索しないファイルの正規表現
  (howm-excluded-file-regexp "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")
  ;; howmメニューの完了済みToDoは非表示にする
  (howm-todo-menu-types "[-+~!]")
  :custom-face
  (howm-mode-title-face ((t (:foreground "cyan"))))
  (howm-reminder-normal-face ((t (:foreground "deep sky blue"))))
  :config
  (add-hook 'find-file-hook
            (lambda ()
              (when (and
                     (buffer-file-name)
                     (string-match (expand-file-name howm-directory)
                                   (expand-file-name buffer-file-name)))
                (howm-mode))))
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
  (with-eval-after-load 'calendar
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
;;; org-mode
;;; ----------------------------------------------------------------------
;; 組み込みの org を使わない
(assq-delete-all 'org package--builtins)
(use-package org
  :ensure t
  :defer t
  :bind
  ("\C-c c" . org-capture)
  ("\C-c a" . org-agenda)
  :custom
  (org-directory (f-join my-dropbox-directory "Documents/org"))
  (org-default-notes-file (f-join org-directory "notes.org"))
  (org-capture-templates
   '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %i\n")
     ("m" "Memo" entry (file+headline org-default-notes-file "Memos")
      "* %?\n  Entered on %U\n %i\n %a\n" :empty-lines 1)
     ))
  (org-agenda-files `(,org-directory))
  (org-agenda-include-diary t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-format-date "%Y/%m/%d (%a)")
  (org-mobile-directory (f-join my-dropbox-directory "アプリ/MobileOrg"))
  (org-mobile-inbox-for-pull (f-join org-directory "from-mobile.org"))
  (org-replace-disputed-keys t)
  (org-use-speed-commands t)
  (org-log-done t)
  (org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w@/!)" "|" "DONE(d!/!)" "CANCELED(c@/!)")))
  (org-todo-keyword-faces '(("SOMEDAY"   . (:foreground "CadetBlue4" :weight bold))
                            ("WAITING"   . (:foreground "orange3" :weight bold))
                            ("CANCELLED" . org-done)))
  (org-use-fast-todo-selection 'expert)
  (org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 1)))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path t)
  :config
  (bind-keys :map org-mode-map
             ([S-C-up]   . nil)
             ([S-C-down] . nil)
             ("C-c ,"    . nil)
             :map org-read-date-minibuffer-local-map
             ([S-up]    . (lambda () (interactive)
                            (org-eval-in-calendar '(calendar-backward-week 1))))
             ([S-down]  . (lambda () (interactive)
                            (org-eval-in-calendar '(calendar-forward-week 1))))
             ([S-left]  . (lambda () (interactive)
                            (org-eval-in-calendar '(calendar-backward-day 1))))
             ([S-right] . (lambda () (interactive)
                            (org-eval-in-calendar '(calendar-forward-day 1)))))
  (with-eval-after-load 'org-agenda
    (bind-keys :map org-agenda-mode-map
               ([S-C-up] . nil)
               ([S-C-down] . nil)))
  ;; "*Org Select*" とか " *Agenda Commands*" を shackle で制御したいので、
  ;; org-switch-to-buffer-other-window をバイパスしたり delete-other-windows を呼べなくする
  (defun bypass-org-switch-to-buffer-other-window (orig-fun &rest args)
    (cl-letf (((symbol-function 'org-switch-to-buffer-other-window)
               (symbol-function 'switch-to-buffer-other-window)))
      (apply orig-fun args)))
  (defun skip-delete-other-windows (orig-fun &rest args)
    (cl-letf (((symbol-function 'delete-other-windows)
               (lambda (&rest args) nil)))
      (apply orig-fun args)))
  (advice-add 'org-capture :around #'bypass-org-switch-to-buffer-other-window)
  (advice-add 'org-capture :around #'skip-delete-other-windows)
  (advice-add 'org-agenda :around #'bypass-org-switch-to-buffer-other-window)
  (advice-add 'org-agenda :around #'skip-delete-other-windows))

(use-package org-bullets
  :ensure t
  :custom (org-bullets-bullet-list '("✿" "◉" "○" "►" "•"))
  :hook (org-mode . org-bullets-mode))

(use-package org-mobile-sync
  :disabled t
  :ensure t
  :after org
  :config
  (org-mobile-sync-mode 1))

;;; ----------------------------------------------------------------------
;;; calendar / japanese-holidays
;;; ----------------------------------------------------------------------
(use-package japanese-holidays
  :ensure t
  :after calendar
  :custom
  (calendar-mark-holidays-flag t)    ; 祝日をカレンダーに表示
  (japanese-holiday-weekend '(0 6))  ; 土日を祝日として表示
  (japanese-holiday-weekend-marker   ; 土曜日を水色で表示
   '(holiday nil nil nil nil nil japanese-holiday-saturday))
  (calendar-month-header '(propertize
                           (format "%d年 %s月" year month)
                           'font-lock-face 'calendar-month-header))
  :config
  (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
   (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (let ((array ["日" "月" "火" "水" "木" "金" "土"]))
    (setq calendar-day-header-array array
          calendar-day-name-array array))
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (defun my/japanese-holiday-show (&rest _args)
    (let* ((date (calendar-cursor-to-date t))
           (calendar-date-display-form '((format "%s年 %s月 %s日（%s）" year month day dayname)))
           (date-string (calendar-date-string date))
           (holiday-list (calendar-check-holidays date)))
      (when holiday-list
        (message "%s: %s" date-string (mapconcat #'identity holiday-list "; ")))))
  (add-hook 'calendar-move-hook 'my/japanese-holiday-show))

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
  :ensure t
  :bind (("M-o"         . occur-by-moccur)
         ("C-c C-x C-o" . moccur))
  :custom
  (moccur-split-word t) ; スペース区切りでAND検索
  (moccur-use-migemo t)
  (*moccur-buffer-name-exclusion-list*
   '(".+TAGS.+" "\.svn" "*Completions*" "*Messages*" " *migemo*"))
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (bind-key "O" 'dired-do-moccur dired-mode-map))))

(use-package moccur-edit
  ;;:straight (:host github :repo "myuhe/moccur-edit.el")
  :straight t
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
  :custom
  (svn-status-hide-unmodified t)
  :config
  (add-to-list 'process-coding-system-alist '("svn" . utf-8)))

;;; ----------------------------------------------------------------------
;;; magit
;;; ----------------------------------------------------------------------
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-push-always-verify nil)
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  :config
  (add-to-list 'auto-coding-alist '("COMMIT_EDITMSG" . utf-8-unix))
  (bind-key [C-tab]         nil magit-status-mode-map)
  (bind-key [C-iso-lefttab] nil magit-status-mode-map)
  (bind-key [C-tab]         nil magit-diff-mode-map)
  (bind-key [C-iso-lefttab] nil magit-diff-mode-map))

(add-hook 'git-commit-mode-hook (lambda ()
                                  (setq-local fill-column 80)
                                  (display-fill-column-indicator-mode t)))

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

(defun my/ruby-mode-setup ()
  (inf-ruby-minor-mode t)
  (electric-indent-mode t)
  (electric-layout-mode t)
  (ruby-end-mode t)
  (rubocop-mode t)
  (modify-syntax-entry ?: ".")
  (add-hook 'before-save-hook 'ruby-mode-set-frozen-string-literal-true))

(use-package enh-ruby-mode
  :ensure t
  :defer t
  :interpreter ("ruby")
  :mode ("\\.rb\\'"
         "config\\.ru\\'"
         "\\(Rake\\|Cap\\|Gem\\|Guard\\)file\\'"
         "\\.xremap\\'")
  :custom
  (enh-ruby-add-encoding-comment-on-save nil)
  (enh-ruby-deep-indent-paren nil)
  :config
  (add-hook 'enh-ruby-mode-hook #'my/ruby-mode-setup))

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
  :ensure t :defer t
  :custom (rubocop-keymap-prefix (kbd "C-c C-c C-r")))

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
  :straight (:host github :repo "tetsujin/emacs-php-align")
  :after php-mode
  :config
  (php-align-setup))

;;; ----------------------------------------------------------------------
;;; web-mode
;;; ----------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.html\\.erb\\'"
         "\\.rhtml?\\'"
         "\\.php\\'")
  :custom
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  :config
  (bind-key "C-'" 'company-web-html web-mode-map)
  ;;(bind-key "C-c C-r" nil web-mode-map)
  (defun my/web-mode-setup ()
    (setq-local company-backends
                (append '(company-web-html) company-backends))
    (when (string-match "\\.erb" (buffer-file-name (current-buffer)))
      (modify-syntax-entry ?% "w"))
    (when (string-match "\\.php" (buffer-file-name (current-buffer)))
      (modify-syntax-entry ?? "w")))
  (add-hook 'web-mode-hook #'my/web-mode-setup))

;;; ----------------------------------------------------------------------
;;; js-mode
;;; ----------------------------------------------------------------------
(custom-set-variables
 '(js-chain-indent t)
 '(js-indent-level 2)
 '(js-indent-first-init 'dynamic))

;;; ----------------------------------------------------------------------
;;; js2-mode
;;; ----------------------------------------------------------------------
(use-package js2-mode
  :if (< emacs-major-version 27)
  :ensure t
  :defer t
  :custom
  (js2-include-browser-externs nil)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  (js2-highlight-external-variables nil)
  (js2-include-jslint-globals nil)
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
  :if (< emacs-major-version 27)
  :ensure t
  :mode (".*\\.jsx\\'" ".*\\.js\\'"))

;;; ----------------------------------------------------------------------
;;; add-node-module-path
;;; ----------------------------------------------------------------------
(use-package add-node-modules-path
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path))
  (with-eval-after-load 'rjsx-mode
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
(defun my/css-mode-setup ()
  (electric-indent-mode t)
  (electric-layout-mode t)
  (setq-local electric-layout-rules
              '((?\{ . after) (?\} . before)))
  (setq-local company-backends
              (append '(company-css) company-backends)))

(use-package less-css-mode
  :ensure t
  :defer t
  :custom
  (less-css-compile-at-save nil)
  :config
  (add-hook 'less-css-mode-hook #'my/css-mode-setup))

(use-package scss-mode
  :ensure t
  :defer t
  :mode ("\\.scss\\'")
  :custom
  (scss-compile-at-save nil)
  :config
  (add-hook 'scss-mode-hook #'my/css-mode-setup))

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
  :straight t
  :defer t
  :mode ("\\.po\\'\\|\\.po\\.")
  :commands (po-find-file-coding-system)
  :init
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                              'po-find-file-coding-system))

;;; ----------------------------------------------------------------------
;;; es-mode
;;; ----------------------------------------------------------------------
(use-package es-mode
  :ensure t
  :defer t
  :mode ("\\.es\\'"))

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
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'mmm-php-mode))

;;; ----------------------------------------------------------------------
;;; editorconfig
;;; ----------------------------------------------------------------------
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;; ----------------------------------------------------------------------
;;; yaml-mode
;;; ----------------------------------------------------------------------
(use-package yaml-mode
  :ensure t :defer t)

;;; ----------------------------------------------------------------------
;;; ansible
;;; ----------------------------------------------------------------------
(use-package ansible
  :ensure t
  :defer t
  :init
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (when (string-match "ansible.*/\\(tasks\\|handlers\\)/.*\\.yml\\'"
                                   (buffer-file-name (current-buffer)))
                 (ansible 1)))))

;;; ----------------------------------------------------------------------
;;; その他の major-mode
;;; ----------------------------------------------------------------------
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
  :mode ("nginx.*\\.conf[^/]*\\'"))

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
(custom-set-variables
 '(user-full-name "YAMAGUCHI, Seiji")
 '(user-mail-address "valda@underscore.jp"))

;;; ----------------------------------------------------------------------
;;; ~のつくバックアップファイルの保存場所の指定
;;; ----------------------------------------------------------------------
(custom-set-variables
 '(backup-directory-alist `(("" . ,(expand-file-name "~/bak"))))
 '(delete-old-versions t)
 '(make-backup-files t))

;;; ----------------------------------------------------------------------
;;; recentf / recentf-ext
;;; ----------------------------------------------------------------------
(use-package recentf
  :custom
  (recentf-save-file "~/.emacs.d/.recentf")
  (recentf-max-saved-items 2000)
  (recentf-exclude '(".recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "bookmarks"))
  (recentf-auto-cleanup 'never)
  :config
  (advice-add 'recentf-save-list
              :around (lambda (orig-fun &rest args)
                        (let ((inhibit-message t))
                          (apply orig-fun args))))
  (run-with-idle-timer 30 t 'recentf-save-list)
  (recentf-mode 1))

(use-package recentf-ext
  :ensure t :after recentf)

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
  :custom
  (elscreen-display-tab nil)
  :config
  (elscreen-set-prefix-key "\C-z")
  (bind-key "\C-z" 'elscreen-toggle elscreen-map)
  (defhydra hydra-switch-elscreen (global-map "\C-z")
    "Switch ElScreen"
    ("<left>"  elscreen-previous "previous")
    ("<right>" elscreen-next     "next")
    ("K"       elscreen-kill     "kill"))
  (elscreen-start))

;;; ----------------------------------------------------------------------
;;; flycheck
;;; ----------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-gcc-language-standard "c++11")
  (flycheck-clang-language-standard "c++11")
  (flycheck-disabled-checkers '(
                                ;;python-flake8
                                ;;python-pylint
                                ruby-rubylint
                                javascript-jshint
                                javascript-jscs
                                )))

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
  (add-hook 'pre-command-hook #'flycheck-posframe-hide-posframe))

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
  :custom
  (bm-buffer-persistence t)
  :config
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'auto-save-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  ;; M$ Visual Studio key setup.
  (bind-key "<C-f2>" 'bm-toggle)
  (bind-key "<f2>"   'bm-next)
  (bind-key "<S-f2>" 'bm-previous))

;;; ----------------------------------------------------------------------
;;; ivy
;;; ----------------------------------------------------------------------
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  ("C-;"     . ivy-switch-buffer)
  ("C-c ;"   . ivy-switch-buffer)
  ("<f6>"    . ivy-resume)
  :custom
  (bind-key* "C-c C-r" 'ivy-resume)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-height 20)
  (ivy-on-del-error-function #'ignore)
  :config
  (setf (alist-get t ivy-re-builders-alist) #'ivy--regex-ignore-order)
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :defer t
  :bind
  ("M-i" . swiper)
  ("M-I" . swiper-thing-at-point)
  (:map isearch-mode-map
        ("M-i" . swiper-from-isearch)))

(use-package counsel
  :ensure t
  :defer t
  :custom
  (counsel-yank-pop-separator "\n----------\n")
  (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  :bind
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop)
  ("C-x C-f" . counsel-find-file)
  ("C-x b" . counsel-switch-buffer)
  ("C-x C-r" . counsel-buffer-or-recentf)
  ("C-c C-f" . counsel-fzf)
  :config
  (ivy-configure 'counsel-M-x :initial-input "")
  ;;(setf (alist-get 'counsel-M-x ivy-re-builders-alist) #'ivy--regex-ignore-order)
  (advice-add 'counsel-switch-buffer
              :around (lambda (orig-fun &rest args)
                        (let ((ivy-use-virtual-buffers nil))
                          (apply orig-fun args)))))

(use-package ivy-hydra
  :ensure t
  :config
  (setq ivy-read-action-function #'ivy-hydra-read-action))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package ivy-yasnippet
  :ensure t
  :bind ("C-c y" . ivy-yasnippet))

(use-package counsel-gtags
  :ensure t
  :hook (prog-mode . counsel-gtags-mode)
  :custom (counsel-gtags-auto-update t)
  :config
  (bind-keys :map counsel-gtags-mode-map
             ("M-t" . counsel-gtags-find-definition)
             ("M-r" . counsel-gtags-find-reference)
             ("M-s" . counsel-gtags-find-symbol))
  (defhydra hydra-counsel-gtags (counsel-gtags-mode-map "ESC")
    "Exploring the gtags context stack"
    ("<" counsel-gtags-go-backward "go backward")
    (">" counsel-gtags-go-forward "go forward")))

;;; ivy インターフェイスで bm.el の bookmark を選択
;;; https://www.reddit.com/r/emacs/comments/700xck/ivy_with_bmel_bookmark_manager/
(defun bm-counsel-get-list (bookmark-overlays)
  (-map (lambda (bm)
          (with-current-buffer (overlay-buffer bm)
            (let* ((line (replace-regexp-in-string "\n$" "" (buffer-substring (overlay-start bm)
                                                                              (overlay-end bm))))
                   ;; line numbers start on 1
                   (line-num (+ 1 (count-lines (point-min) (overlay-start bm))))
                   (name (format "%s:%d - %s" (buffer-name) line-num line))
                   )
              `(,name . ,bm)
              )
            )
          )
        bookmark-overlays))
(defun bm-counsel-find-bookmark ()
  (interactive)
  (let* ((bm-list (bm-counsel-get-list (bm-overlays-lifo-order t)))
         (bm-hash-table (make-hash-table :test 'equal))
         (search-list (-map (lambda (bm) (car bm)) bm-list)))

    (-each bm-list (lambda (bm)
                     (puthash (car bm) (cdr bm) bm-hash-table)
                     ))

    (ivy-read "Find bookmark: "
              search-list
              :require-match t
              :keymap counsel-describe-map
              :action (lambda (chosen)
                        (let ((bookmark (gethash chosen bm-hash-table)))
                          (switch-to-buffer (overlay-buffer bookmark))
                          (bm-goto bookmark)
                          ))
              :sort t
              )))
(bind-key "C-c b" 'bm-counsel-find-bookmark)

;;; ivy で migemo を使う
;;; https://www.yewton.net/2020/05/21/migemo-ivy/
(require 'dash)
(require 's)
(defun ytn-ivy-migemo-re-builder (str)
  (let* ((sep " \\|\\^\\|\\.\\|\\*")
         (splitted (--map (s-join "" it)
                          (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
                                          (s-split "" str t)))))
    (s-join "" (--map (cond ((s-equals? it " ") ".*?")
                            ((s-matches? sep it) it)
                            (t (migemo-get-pattern it)))
                      splitted))))
(setf (alist-get 'swiper ivy-re-builders-alist) #'ytn-ivy-migemo-re-builder)

;;; git status からファイル選択 (helm-browse-project の代替)
(defun my/counsel-git-status-list (dir)
  (let ((default-directory dir))
    (split-string
     (shell-command-to-string "git status -s --")
     "\n"
     t)))
(defun my/counsel-git-status-action (x)
  (when (string-match "\\`[ MADRCU\\?]\\{2\\} \\(?:.*? -> \\)?\\(.*\\)\\'" x)
    (with-ivy-window
      (let ((default-directory (ivy-state-directory ivy-last)))
        (find-file (match-string 1 x))))))
(defun my/counsel-git-status (&optional initial-input)
  (interactive)
  (let ((default-directory (counsel-locate-git-root)))
    (ivy-read "Git status: " (my/counsel-git-status-list default-directory)
              :initial-input initial-input
              :action #'my/counsel-git-status-action
              :caller 'counsel-git-status)))
(bind-key "C-c d" 'my/counsel-git-status)

;;; ----------------------------------------------------------------------
;;; projectile
;;; ----------------------------------------------------------------------
(use-package projectile
  :ensure t
  :diminish projectile-mode
  ;;:custom
  ;;(projectile-completion-system 'ivy)
  ;;(projectile-switch-project-action 'counsel-projectile)
  :config
  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (projectile-mode +1))

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   (helm-projectile-on))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

(use-package projectile-rails
  :ensure t
  :config
  (bind-key "C-c r" 'projectile-rails-command-map projectile-rails-mode-map)
  (projectile-rails-global-mode))

;;; ----------------------------------------------------------------------
;;; amx
;;; ----------------------------------------------------------------------
(use-package amx
  :ensure t
  :custom (amx-history-length 20)
  :config (amx-mode 1))

;;; ----------------------------------------------------------------------
;;; anzu
;;; ----------------------------------------------------------------------
(use-package anzu
  :ensure t
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-use-migemo t)
  :config
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
;;; shackle
;;; ----------------------------------------------------------------------
(use-package shackle
  :ensure t
  :config
  (setq shackle-rules
        '(
          (compilation-mode :align below :size 0.4)
          (rspec-compilation-mode :align below :size 0.4)
          (help-mode :align below :select t :popup t)
          (magit-status-mode :other t :select t)
          (calendar-mode :align below :popup t)
          ("*Backtrace*" :align below :size 0.3 :noselect t)
          ("*Apropos*" :align below :size 0.4 :select t)
          ("*Warnings*" :align below :size 0.3)
          ("*Org Select*" :align below :size 0.3)
          ("^CAPTURE-.*\\.org\\'" :regexp t :align below :size 0.3)
          ;;("*Org Agenda*" :other t :select t)
          (" *Agenda Commands*" :align below :size 0.3)
          (" *Org todo*" :align below :size 0.3 :popup t)
          ("*rg*" :other t :select t :inhibit-window-quit t)
          ("*git-gutter:diff*" :align below :size 0.4)
          ("\\(Messages\\|Output\\|Report\\)\\*\\'" :regexp t :align below :size 0.3)
          ))
  (shackle-mode 1))

;;;; test
;; (display-buffer (get-buffer-create " *Org todo*"))

;;; ----------------------------------------------------------------------
;;; popper.el
;;; ----------------------------------------------------------------------
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-display-control nil)
  :config
  (setq popper-reference-buffers
        '(
          compilation-mode
          rspec-compilation-mode
          help-mode
          "\\*Backtrace\\*"
          "\\*Apropos\\*"
          "\\*Warnings\\*"
          "\\*Messages\\*"
          "Output\\*$"
          "Report\\*$"
          "\\*git-gutter:diff\\*"
          ))
  (popper-mode +1))

;;; ----------------------------------------------------------------------
;;; git-gutter.el
;;; ----------------------------------------------------------------------
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :custom
  (git-gutter:update-hooks '(after-save-hook after-revert-hook))
  :config
  (run-with-idle-timer 1 t 'git-gutter)
  (global-git-gutter-mode t)
  (defhydra hydra-git-gutter nil
    "git hunk"
    ("p" git-gutter:previous-hunk "previous")
    ("n" git-gutter:next-hunk "next")
    ("s" git-gutter:stage-hunk "stage")
    ("r" git-gutter:revert-hunk "revert")
    ("d" git-gutter:popup-hunk "diff"))
  (bind-key "C-c g" 'hydra-git-gutter/body))

;;; ----------------------------------------------------------------------
;;; vterm / vterm-toggle
;;; ----------------------------------------------------------------------
(unless (eq window-system 'w32)
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
                   (window-height . 0.4)))))

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
  :bind ("\C-c t" . google-translate-smooth-translate))

;;; ----------------------------------------------------------------------
;;; japanese-(hankaku|zenkaku)-region の俺俺変換テーブル
;;; ----------------------------------------------------------------------
(with-eval-after-load 'japan-util
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
(use-package auto-async-byte-compile
  :ensure t
  :custom (auto-async-byte-compile-exclude-files-regexp "/junk/")
  :hook (emacs-lisp-mode-hook . enable-auto-async-byte-compile-mode))

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
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

;;; ----------------------------------------------------------------------
;;; rainbow-mode
;;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((prog-mode text-mode conf-mode) . rainbow-mode))

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
  :custom
  (wgrep-enable-key "r")
  (wgrep-auto-save-buffer t))

;;; ----------------------------------------------------------------------
;;; ag / wgrep-ag
;;; ----------------------------------------------------------------------
(use-package ag
  :ensure t
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t)
  (ag-reuse-buffers t))

(use-package wgrep-ag
  :ensure t
  :hook (ag-mode . wgrep-ag-setup)
  :bind (:map ag-mode-map ("r" . wgrep-change-to-wgrep-mode)))

;;; ----------------------------------------------------------------------
;;; ripgrep
;;; ----------------------------------------------------------------------
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

;;; ----------------------------------------------------------------------
;;; tempbuf
;;; ----------------------------------------------------------------------
(use-package tempbuf
  :straight (:host github :repo "valda/tempbuf")
  :hook ((
          dired-mode
          custom-mode-hook
          w3-mode-hook
          Man-mode-hook
          view-mode-hook
          compilation-mode-hook
          calendar-mode-hook
          )
         . turn-on-tempbuf-mode)
  :custom
  (tempbuf-kill-message nil)
  :init
  (add-hook 'fundamental-mode-hook
            (lambda ()
              (when (string-match "*Flycheck error messages*" (buffer-name))
                'turn-on-tempbuf-mode))))

;;; ----------------------------------------------------------------------
;;; nyan-mode
;;; ----------------------------------------------------------------------
(use-package nyan-mode
  :ensure t
  :hook after-init
  :custom (nyan-bar-length 16)
  :config (nyan-start-animation))

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
;;; ibuffer
;;; ----------------------------------------------------------------------
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (add-to-list 'ibuffer-never-show-predicates "^\\*flycheck-posframe-buffer\\*")
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

(use-package ibuffer-vc
  :ensure t
  :hook (ibuffer . (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic)))))

(use-package all-the-icons-ibuffer
  :ensure t
  :custom
  (all-the-icons-ibuffer-formats
   `((mark modified read-only ,(if (>= emacs-major-version 26) 'locked "") vc-status-mini
           " " (icon 2 2 :left :elide)
           ,(propertize " " 'display `(space :align-to 10))
           (name 30 30 :left :elide)
           " " (size-h 9 -1 :right)
           " " (mode+ 16 16 :left :elide)
           " " (coding 12 12 :left)
           " " filename-and-process+)
     (mark " " (name 30 -1) " " (coding 15 15) " " filename)))
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
;;; which-key
;;; ----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :custom
  (which-key-idle-delay 3.0)
  (which-key-idle-secondary-delay 0.5)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

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

;;; ----------------------------------------------------------------------
;;; narrowing などの操作を有効化
;;; ----------------------------------------------------------------------
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; ----------------------------------------------------------------------
;;; exec-path-from-shell
;;; ----------------------------------------------------------------------
(use-package exec-path-from-shell
  :unless (eq window-system 'w32)
  :ensure t
  :config (exec-path-from-shell-initialize))

;;; ----------------------------------------------------------------------
;;; gnuserv
;;; ----------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
 (server-start))

;;; ----------------------------------------------------------------------
;;; load custom.el
;;; ----------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(cd "~")

(provide 'init)
;;; init.el ends here
