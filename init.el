;;; init.el --- My init.el -*- coding: utf-8-unix; lexical-binding: t -*-
;; Author: valda <valda68k@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

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
 '(compilation-scroll-output 'first-error)
 '(find-file-visit-truename t)
 '(vc-follow-symlinks t)
 '(auto-revert-check-vc-info nil)
 '(history-length t)
 '(inhibit-compacting-font-caches t)
 '(user-full-name "YAMAGUCHI, Seiji")
 '(user-mail-address "valda@underscore.jp")
 '(backup-directory-alist `(("" . ,(expand-file-name "~/bak"))))
 '(delete-old-versions t)
 '(make-backup-files t)
 '(read-extended-command-predicate #'command-completion-default-include-p))

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
;;; WSL2
;;; ----------------------------------------------------------------------
(defun my/wsl-p ()
  (string-match "Microsoft" (with-temp-buffer
                              (insert-file-contents "/proc/version")
                              (buffer-string))))

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
;; ■□◆◇←↓↑→……

(setq-default line-spacing 0)  ;; 行間を狭くする
(setq use-default-font-for-symbols nil)
(set-face-attribute 'default nil :font "Ricty Discord" :height 150)
;; Cicaを使うと右寄せの時の文字数がずれるので Ricty Discord に戻す
;; (set-face-attribute 'default nil :font "Cica" :height 150)
;; (dolist (c '(?… ?■ ?□ ?◆ ?◇ ?← ?↓ ?↑ ?→)) (set-fontset-font t c "Ricty Discord"))
;; ;; Box Drawing (U+2500-U+257F) を別のフォントで描画
;; (set-fontset-font t '(#x2500 . #x257F) "Noto Sans Mono" nil 'prepend)
;; ;; Block Elements (U+2580-U+259F) も念のため
;; (set-fontset-font t '(#x2580 . #x259F) "Noto Sans Mono" nil 'prepend)
(set-fontset-font t '(#x1F000 . #x1FAFF) "Noto Color Emoji")
(add-to-list 'face-font-rescale-alist '(".*Noto Color Emoji.*" . 0.82))

;;; ----------------------------------------------------------------------
;;; elpaca
;;; ----------------------------------------------------------------------
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode +1))


;;; ----------------------------------------------------------------------
;;; use-package
;;; ----------------------------------------------------------------------
(custom-set-variables
 '(use-package-verbose t)
 '(use-package-compute-statistics t)
 '(use-package-minimum-reported-time 0.01)
 '(use-package-enable-imenu-support t))
(use-package diminish :ensure t :demand t)

;;; ----------------------------------------------------------------------
;;; nerd-icons
;;; ----------------------------------------------------------------------
(use-package nerd-icons
  :ensure t)

;;; ----------------------------------------------------------------------
;;; kind-icon
;;; ----------------------------------------------------------------------
;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; ---------------------------------------------------------------------
;;; monokai-theme
;;; ----------------------------------------------------------------------
(use-package monokai-theme
  :disabled
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
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-scale-org-headlines nil)
  (solarized-scale-outline-headlines nil)
  (solarized-high-contrast-mode-line t)
  :custom-face
  (mode-line          ((t (:overline nil :underline nil :foreground "White" :background "DarkCyan" :box nil))))
  :config
  (with-eval-after-load 'mozc-cand-posframe
    (set-face-attribute 'mozc-cand-posframe-normal-face nil
                        :background "#191a1b" :foreground 'unspecified)
    (set-face-attribute 'mozc-cand-posframe-focused-face nil
                        :background "#00415e" :foreground "white")
    (set-face-attribute 'mozc-cand-posframe-footer-face nil
                        :inherit 'completions-annotations))
  (with-eval-after-load 'flycheck-posframe
    (set-face-attribute 'flycheck-posframe-background-face nil
                        :background (face-attribute 'mode-line :background)))
  (with-eval-after-load 'highlight-symbol
    (setq highlight-symbol-colors
          `(,@(mapcar
               (lambda (color) (solarized-color-blend color (face-attribute 'default :background) 0.4))
               '("yellow" "DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
                 "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")))))
  (with-eval-after-load 'tab-bar
    (set-face-attribute 'tab-bar nil
                        :foreground 'unspecified :background 'unspecified
                        :height 0.9
                        :inherit 'mode-line-inactive)
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :foreground 'unspecified :background 'unspecified
                        :inherit '(mode-line-inactive tab-bar))
    (set-face-attribute 'tab-bar-tab nil
                        :foreground 'unspecified :background 'unspecified :bold t
                        :inherit '(mode-line-active tab-bar)))
  (load-theme 'solarized-dark-high-contrast t))

;;; ----------------------------------------------------------------------
;;; doom-modeline
;;; ----------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 34)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes t)
  (doom-modeline-github t)
  (doom-modeline-workspace-name nil)
  :custom-face
  (doom-modeline-highlight ((t (:foreground "GhostWhite" :background "DeepSkyBlue4" :inherit mode-line-buffer-id))))
  (doom-modeline-panel     ((t (:inherit doom-modeline-highlight))))
  (doom-modeline-bar       ((t (:background "DeepSkyBlue2" :inherit mode-line-buffer-id))))
  :config
  (use-package async :ensure t)
  (use-package ghub :ensure t)
  (with-eval-after-load 'flycheck
    (diminish 'flycheck-mode)))

;;; ----------------------------------------------------------------------
;;; nyan-mode
;;; ----------------------------------------------------------------------
(use-package nyan-mode
  :ensure t
  :hook emacs-startup
  :custom (nyan-bar-length 16)
  :config (nyan-start-animation))

;;; ----------------------------------------------------------------------
;;; tab-bar
;;; ----------------------------------------------------------------------
(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice t)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-hints t)
  (tab-bar-show 1)
  :config
  (defvar my/tab-prefix-key-map (make-sparse-keymap))
  (define-key my/tab-prefix-key-map "\C-c" 'tab-new)
  (define-key my/tab-prefix-key-map "c" 'tab-new)
  (define-key my/tab-prefix-key-map "\C-k" 'tab-close)
  (define-key my/tab-prefix-key-map "k" 'tab-close)
  (define-key my/tab-prefix-key-map "K" 'tab-close-other)
  (define-key my/tab-prefix-key-map "\C-p" 'tab-previous)
  (define-key my/tab-prefix-key-map "p" 'tab-previous)
  (define-key my/tab-prefix-key-map "\C-n" 'tab-next)
  (define-key my/tab-prefix-key-map "n" 'tab-next)
  (define-key my/tab-prefix-key-map "\C-z" 'tab-recent)
  (define-key my/tab-prefix-key-map "'" 'tab-bar-switch-to-tab)
  (define-key my/tab-prefix-key-map "1" (lambda () (interactive) (tab-select 1)))
  (define-key my/tab-prefix-key-map "2" (lambda () (interactive) (tab-select 2)))
  (define-key my/tab-prefix-key-map "3" (lambda () (interactive) (tab-select 3)))
  (define-key my/tab-prefix-key-map "4" (lambda () (interactive) (tab-select 4)))
  (define-key my/tab-prefix-key-map "5" (lambda () (interactive) (tab-select 5)))
  (define-key my/tab-prefix-key-map "6" (lambda () (interactive) (tab-select 6)))
  (define-key my/tab-prefix-key-map "7" (lambda () (interactive) (tab-select 7)))
  (define-key my/tab-prefix-key-map "8" (lambda () (interactive) (tab-select 8)))
  (define-key my/tab-prefix-key-map "9" (lambda () (interactive) (tab-select 9)))
  (define-key my/tab-prefix-key-map "A" 'tab-bar-rename-tab)
  (define-key my/tab-prefix-key-map "i" (lambda () (interactive) (setq tab-bar-tab-hints (null tab-bar-tab-hints))))
  (define-key my/tab-prefix-key-map "t" (lambda () (interactive) (tab-bar-mode (if tab-bar-mode -1 t))))
  (bind-key "C-z" my/tab-prefix-key-map))

;;; ----------------------------------------------------------------------
;;; good-scroll
;;; ----------------------------------------------------------------------
(use-package good-scroll
  :ensure t
  :config
  (good-scroll-mode 1))

;;; ----------------------------------------------------------------------
;;; W32-IME / mozc / ibus / uim
;;; ----------------------------------------------------------------------
(defun my/w32-ime-init()
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
  (add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil))))

(defun my/mozc-init()
  (use-package mozc
    :ensure (:host github :repo "google/mozc" :files ("src/unix/emacs/mozc.el"))
    :config
    ;; Windows の mozc では、セッション接続直後 directモード になるので hiraganaモード にする
    ;; (when (my/wsl-p)
    ;;   (advice-add 'mozc-session-execute-command
    ;;               :after (lambda (&rest args)
    ;;                        (when (eq (nth 0 args) 'CreateSession)
    ;;                          ;; (mozc-session-sendkey '(hiragana)))))
    ;;                          (mozc-session-sendkey '(Hankaku/Zenkaku))))))
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
        (progn
          ;;(message "%s" event) ;debug
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
    (add-hook 'minibuffer-setup-hook 'deactivate-input-method))

  (use-package mozc-popup
    :unless (display-graphic-p)
    :ensure t
    :after mozc
    :custom (mozc-candidate-style 'popup))

  (use-package mozc-cand-posframe
    :if (display-graphic-p)
    :ensure t
    :after mozc
    :custom (mozc-candidate-style 'posframe))

  (use-package mozc-cursor-color
    :ensure (:host github :repo "iRi-E/mozc-el-extensions" :main "mozc-cursor-color.el")
    :after mozc
    :config
    (setq mozc-cursor-color-alist
          '((direct        . "green")
            (read-only     . "yellow")
            (hiragana      . "red")
            (full-katakana . "goldenrod")
            (half-ascii    . "dark orchid")
            (full-ascii    . "orchid")
            (half-katakana . "dark goldenrod")))))

(cond ((eq window-system 'w32)
       (my/w32-ime-init))
      (t
       (my/mozc-init)))

;;; ----------------------------------------------------------------------
;;; yasnippet.el
;;; ----------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)
  :config
  ;; 無効化: デフォルトのTABバインディング
  (bind-keys :map yas-minor-mode-map
             ("<tab>" . nil)
             ("TAB" . nil))
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; ----------------------------------------------------------------------
;;; dabbrev / hippie-expand
;;; ----------------------------------------------------------------------
(eval-after-load 'abbrev
  '(with-eval-after-load 'diminish
     (diminish 'abbrev-mode)))
(custom-set-variables
 '(dabbrev-case-fold-search t)
 '(dabbrev-case-replace t)
 '(hippie-expand-try-functions-list
   '(yas/hippie-try-expand
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name)))
(bind-key "/" 'hippie-expand esc-map)

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
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-resize-window (global-map "C-x")
    "Resize Window"
    ("^" enlarge-window "enlarge vertically")
    ("_" shrink-window "shrink vertically")
    ("{" shrink-window-horizontally "shrink horizontally")
    ("}" enlarge-window-horizontally "enlarge horizontally")))

;;; ----------------------------------------------------------------------
;;; iflipb
;;; ----------------------------------------------------------------------
(use-package iflipb
  :ensure t
  :commands (iflipb-next-buffer iflipb-previous-buffer iflipb-kill-buffer)
  :init
  (with-eval-after-load 'hydra
    (defhydra hydra-buff (global-map "C-x")
      "iflipb"
      ("<left>" iflipb-previous-buffer "previous buffer")
      ("<right>" iflipb-next-buffer "next buffer"))))

;;; ----------------------------------------------------------------------
;;; winner-mode
;;; ----------------------------------------------------------------------
(use-package winner
  :custom (winner-dont-bind-my-keys t)
  :config
  (winner-mode t)
  (with-eval-after-load 'hydra
    (defhydra hydra-winner (winner-mode-map "C-c")
      "Winner"
      ("<left>" (progn
                  (winner-undo)
                  (setq this-command 'winner-undo))
       "back")
      ("<right>" winner-redo "forward"
       :exit t :bind nil))))

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
;;; undo-tree
;;; ----------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind ("C-." . 'undo-tree-redo)
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  :config (global-undo-tree-mode))

;;; ----------------------------------------------------------------------
;;; migemo
;;; ----------------------------------------------------------------------
(defvar migemo-command-path (executable-find "cmigemo"))
(use-package migemo
  :ensure t
  :if migemo-command-path
  :custom
  (migemo-command migemo-command-path)
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary (cond ((eq window-system 'w32)
                            "~/scoop/apps/cmigemo/current/cmigemo-default-win32/dict/utf-8/migemo-dict")
                           (t
                            "/usr/share/cmigemo/utf-8/migemo-dict")))
  (migemo-coding-system 'utf-8-unix)
  (migemo-use-pattern-alist nil)
  (migemo-use-frequent-pattern-alist t)
  (migemo-pattern-alist-length 1024)
  (migemo-isearch-min-length 2)
  :config
  (migemo-init))

;;; ----------------------------------------------------------------------
;;; dired 関係
;;; ----------------------------------------------------------------------
(custom-set-variables
 '(ls-lisp-ignore-case t)
 '(ls-lisp-dirs-first t)
 '(dired-listing-switches "-aFl --group-directories-first")
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always)
 '(dired-isearch-filenames t)
 '(dired-omit-mode t))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(add-hook 'dired-load-hook
          (lambda ()
            (require 'dired-x)
            (require 'wdired)
            (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
            (bind-key "C-c o" 'dired-open-file dired-mode-map)
            (advice-add 'wdired-finish-edit
                        :after (lambda ()
                                 (deactivate-input-method)
                                 (dired-k)))))

(use-package dired-k
  :ensure t
  :bind (:map dired-mode-map
              ("g" . dired-k))
  :hook ((dired-initial-position . dired-k)
         (dired-after-readin-hook . dired-k-no-revert)))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;;; ----------------------------------------------------------------------
;;; Dropbox のパス
;;; ----------------------------------------------------------------------
;; (defvar my/dropbox-directory
;;   (cond ((string-equal (system-name) "SILVER")
;;          "D:/Dropbox/")
;;         (t
;;          "~/Dropbox/")))
(defvar my/dropbox-directory "~/Dropbox/")


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
  (howm-directory (expand-file-name "Documents/howm" my/dropbox-directory))
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
  (defun my/save-and-kill-buffer-howm ()
    (interactive)
    (when (and
           (buffer-file-name)
           (string-match (expand-file-name howm-directory)
                         (expand-file-name buffer-file-name)))
      (save-buffer)
      (kill-buffer nil)))
  (define-key howm-mode-map "\C-c\C-c" 'my/save-and-kill-buffer-howm)
  ;; 日付けの入力が面倒
  (with-eval-after-load 'calendar
    (define-key calendar-mode-map "\C-m" 'my/insert-day)
    (defun my/insert-day ()
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
(use-package org
  :ensure t
  :bind
  ("\C-c c" . org-capture)
  ("\C-c a" . org-agenda)
  :custom
  (org-directory (expand-file-name "Documents/org/" my/dropbox-directory))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-capture-templates
   '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %i\n")
     ("m" "Memo" entry (file+headline org-default-notes-file "Memos")
      "* %?\n  Entered on %U\n %i\n %a\n" :empty-lines 1)
     ))
  (org-agenda-files (list org-directory))
  (org-agenda-include-diary t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-format-date "%Y/%m/%d (%a)")
  (org-agenda-log-mode-items '(closed))
  (org-replace-disputed-keys t)
  (org-use-speed-commands t)
  (org-log-done t)
  (org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELED(c@/!)")))
  (org-todo-keyword-faces '(("SOMEDAY"   . (:foreground "CadetBlue4" :weight bold))
                            ("WAITING"   . (:foreground "orange3" :weight bold))
                            ("CANCELED" . org-done)))
  (org-use-fast-todo-selection 'expert) ; C-c C-t のTODO切り替え時に補完メニューなしで1文字選択できる
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
  :disabled
  :ensure t
  :custom (org-bullets-bullet-list '("◉" "○" "✿" "●" "►" "•"))
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (setq org-modern-todo-faces '(("SOMEDAY"  :background "cyan4" :foreground "black")
                           ("WAITING"  :background "DarkOrange2"    :foreground "black"))))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (expand-file-name "Documents/org/roam" my/dropbox-directory))
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)

  ;; ノート作成用テンプレート
  (setq org-roam-capture-templates
        '(("d" "Default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :note:\n#+date: %U\n\n")
           :unnarrowed t)

          ("t" "Tech Note" plain
           "* 概要\n%?\n\n* 詳細\n\n* 関連ノート\n"
           :if-new (file+head "tech/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :tech:\n#+date: %U\n\n")
           :unnarrowed t)

          ("m" "Memo" plain
           "- %?"
           :if-new (file+head "memos/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :memo:\n#+date: %U\n\n")
           :unnarrowed t)))

  ;; 日次ノートのテンプレート
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "Default" entry
           "* %<%H:%M> %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n")))))

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
(defconst my/cc-style
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
            ;; my/cc-stye を登録して有効にする
            (c-add-style "PERSONAL" my/cc-style t)
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
  :custom
  (hs-hide-comments-when-hiding-all nil)
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

(use-package hideshow-org
  :ensure (:host github :repo "secelis/hideshow-org")
  :custom
  (bind-key "\C-ch" 'hs-org/minor-mode))

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
  :ensure (:host github :repo "myuhe/moccur-edit.el")
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
(use-package llama :ensure t)
(use-package transient :ensure t)
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
  (bind-key [C-iso-lefttab] nil magit-diff-mode-map)
  (add-hook 'git-commit-mode-hook (lambda ()
                                    (setq-local fill-column 80)
                                    (display-fill-column-indicator-mode t))))


;;; ----------------------------------------------------------------------
;;; (enhanced-)ruby-mode
;;; ----------------------------------------------------------------------
(use-package enh-ruby-mode
  :ensure t
  :defer t
  :interpreter ("ruby")
  :mode ("\\.rb\\'"
         "config\\.ru\\'"
         "\\(Rake\\|Cap\\|Gem\\|Guard\\)file\\'"
         "\\.prawn\\'"
         "\\.jbuilder\\'"
         "\\.xremap\\'")
  :custom
  (enh-ruby-add-encoding-comment-on-save nil)
  (enh-ruby-deep-indent-paren nil)
  :config
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (inf-ruby-minor-mode t)
              (electric-indent-mode t)
              (electric-layout-mode t)
              (rubocop-mode t)
              (modify-syntax-entry ?: "."))))

(use-package inf-ruby
  :ensure t
  :defer t)

(use-package ruby-end
  :ensure t
  :hook (enh-ruby-mode . ruby-end-mode)
  :diminish ruby-end-mode)

(use-package rubocop
  :ensure t :defer t
  :diminish rubocop-mode
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
(defun my/php-mode-setup ()
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
  :ensure t
  :config
  (add-hook 'php-mode-hook 'my/php-mode-setup))

(use-package php-align
  :ensure (:host github :repo "tetsujin/emacs-php-align")
  :after php-mode
  :config
  (php-align-setup))

;;; ----------------------------------------------------------------------
;;; web-mode
;;; ----------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.erb\\'"
         "\\.rhtml?\\'"
         "\\.php\\'")
  :custom
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-closing t)
  (web-mode-auto-close-style 2)
  (web-mode-tag-auto-close-style 2)
  :config
  (defun my/web-mode-setup ()
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
    (add-hook 'js2-mode-hook #'add-node-modules-path))
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

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
  ;;(flycheck-mode +1)
  ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :defer t
  :init
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

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
  :ensure t
  :defer t
  :mode ("\\.po\\'\\|\\.po\\.")
  :commands (po-find-file-coding-system)
  :init
  (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                              'po-find-file-coding-system))

;;; ----------------------------------------------------------------------
;;; es-mode (elasticsearch)
;;; ----------------------------------------------------------------------
(use-package es-mode
  :ensure t
  :defer t
  :mode ("\\.es\\'"))

;;; ----------------------------------------------------------------------
;;; mmm-mode
;;; ----------------------------------------------------------------------
(use-package mmm-mode
  :disabled
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
;;; recentf / recentf-ext
;;; ----------------------------------------------------------------------
(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-max-saved-items 2000)
  (recentf-exclude '("recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "bookmarks"))
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
;;; desktop
;;; ----------------------------------------------------------------------
(use-package desktop
  :custom
  (desktop-restore-frames nil)
  (desktop-restore-eager 10)
  (desktop-globals-to-save '(desktop-missing-file-warning
                             tags-file-name
                             tags-table-list
                             register-alist))
  :config
  (desktop-save-mode 1))

;;; ----------------------------------------------------------------------
;;; session
;;; ----------------------------------------------------------------------
(use-package session
  :ensure t
  :hook (emacs-startup . session-initialize)
  :custom
  (session-save-file-coding-system 'no-conversion)
  (session-globals-max-string 10000000)
  (session-initialize '(de-saveplace session places keys menus))
  (session-globals-include '((kill-ring 1000)
                             (session-file-alist 1000 t)
                             (file-name-history 1000)
                             search-ring regexp-search-ring))
  (session-save-print-spec '(t nil 40000)))

;;; ----------------------------------------------------------------------
;;; persistent-scratch
;;; ----------------------------------------------------------------------
(use-package persistent-scratch
  :ensure t
  :hook (emacs-startup . persistent-scratch-setup-default))

;;; ----------------------------------------------------------------------
;;; scratch バッファを消さないようにする
;;; ----------------------------------------------------------------------
(defun my/make-scratch (&optional arg)
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
                (progn (my/make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my/make-scratch 1))))

;;; ----------------------------------------------------------------------
;;; kill-ring に同じ内容の文字列を複数入れない
;;; ----------------------------------------------------------------------
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;;; ----------------------------------------------------------------------
;;; flycheck
;;; ----------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :hook (emacs-startup . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-gcc-language-standard "c++11")
  (flycheck-clang-language-standard "c++11")
  (flycheck-disabled-checkers '(
                                ;;python-flake8
                                ;;python-pylint
                                ruby-rubylint
                                javascript-jshint
                                javascript-jscs
                                scss
                                )))

(use-package flycheck-pyflakes
  :ensure t
  :after flycheck)

(use-package flycheck-posframe
  :disabled
  :if (window-system)
  :ensure t
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-border-width 1)
  :custom-face
  (flycheck-posframe-border-face ((t (:foreground "gray30"))))
  :config
  (add-hook 'pre-command-hook #'flycheck-posframe-hide-posframe))

;; flymake のハイライトを無効にする
(with-eval-after-load 'flymake
  (custom-set-variables
   '(flymake-error-bitmap nil)
   '(flymake-note-bitmap nil)
   '(flymake-warning-bitmap nil)
   )
  (set-face-underline 'flymake-error nil)
  (set-face-underline 'flymake-note nil)
  (set-face-underline 'flymake-warning nil))

;;; ----------------------------------------------------------------------
;;; bm
;;; ----------------------------------------------------------------------
(use-package bm
  :ensure t
  :custom
  (bm-buffer-persistence t)
  :config
  (add-hook 'emacs-startup-hook 'bm-repository-load)
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
;;; gxref
;;; ----------------------------------------------------------------------
(use-package gxref
  :ensure t
  :after xref
  :config
  (add-to-list 'xref-backend-functions 'gxref-xref-backend))

;;; ----------------------------------------------------------------------
;;; projectile
;;; ----------------------------------------------------------------------
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (bind-key "C-c C-p" 'projectile-command-map projectile-mode-map)
  (projectile-mode +1))

(use-package projectile-rails
  :ensure t
  :config
  (bind-key "C-c r" 'projectile-rails-command-map projectile-rails-mode-map)
  (add-hook 'projectile-rails-mode-hook
            #'(lambda ()
                (with-eval-after-load 'yasnippet
                  (yas-activate-extra-mode 'rails-mode))))
  (projectile-rails-global-mode))

;;; ----------------------------------------------------------------------
;;; orderless
;;; ----------------------------------------------------------------------
(use-package orderless
  :ensure t
  :commands (orderless-filter)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; ----------------------------------------------------------------------
;;; vertico/consult/marginalia/embark
;;; ----------------------------------------------------------------------
(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  ;;(vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :custom
  (savehist-coding-system 'no-convertion)
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-;" . consult-buffer)
         ("C-x b" . consult-buffer)
         ("C-c b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("M-g" . consult-goto-line)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.2
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (;;("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package all-the-icons-completion
;;   :ensure t
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))

(use-package nerd-icons-completion
  :ensure t
  :hook (emacs-startup . nerd-icons-completion-mode))

(use-package consult-projectile
  :ensure t
  :config
  (bind-keys :map projectile-mode-map
             ("C-c p p" . consult-projectile-switch-project)
             ("C-c p d" . consult-projectile-find-dir)
             ("C-c p f" . consult-projectile-find-file)))

;;; ----------------------------------------------------------------------
;;; corfu/cape
;;; ----------------------------------------------------------------------
(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info corfu-history))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preselect 'prompt)
  (corfu-popupinfo-delay 0.5)
  (corfu-on-exact-match nil)
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (defun my/corfu-insert-or-newline ()
    (interactive)
    (if (>= corfu--index 0)
        (corfu--insert 'finished)
      (corfu-quit)
      (call-interactively 'newline)))
  (bind-keys :map corfu-map
             ("RET" . my/corfu-insert-or-newline)))

(use-package corfu-terminal
  :ensure
  (corfu-terminal :type git
                  :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless
  (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p i" . cape-ispell)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-emoji))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-gtags)))

(use-package nerd-icons-corfu
  :ensure (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; ----------------------------------------------------------------------
;;; eglot
;;; ----------------------------------------------------------------------
;; (use-package eglot
;;   :ensure t
;;   :hook ((enh-ruby-mode ruby-mode python-mode) . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs `(enh-ruby-mode ,@(alist-get 'ruby-mode eglot-server-programs)))
;;   (add-to-list 'eglot-server-programs
;;                '((web-mode :language-id "html") . ("npx" "tailwindcss-language-server" "--stdio"))))

;;; ----------------------------------------------------------------------
;;; lsp-mode
;;; ----------------------------------------------------------------------

;; Ruby用LSPの設定（enh-ruby-mode対応）
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (enh-ruby-mode . my/setup-ruby-lsp)

  :custom
  ;; bundler経由で起動する設定
  (lsp-ruby-lsp-server-command '("bundle" "exec" "ruby-lsp"))
  (lsp-solargraph-use-bundler t)

  :config
  ;; Gemfile を見て LSP クライアントを選ぶ
  (defun my/lsp-ruby-client-from-gemfile ()
    (let* ((gemfile (locate-dominating-file default-directory "Gemfile"))
           (gemfile-path (and gemfile (expand-file-name "Gemfile" gemfile))))
      (when gemfile-path
        (with-temp-buffer
          (insert-file-contents gemfile-path)
          (cond
           ((re-search-forward "gem ['\"]ruby-lsp['\"]" nil t) 'ruby-lsp-enh)
           ((re-search-forward "gem ['\"]solargraph['\"]" nil t) 'solargraph-enh)
           (t nil))))))

  ;; enh-ruby-mode 用の LSP クライアントを手動登録
  (add-to-list 'lsp-language-id-configuration '(enh-ruby-mode . "ruby"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("bundle" "exec" "ruby-lsp"))
    :major-modes '(enh-ruby-mode)
    :priority -1
    :server-id 'ruby-lsp-enh))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("bundle" "exec" "solargraph" "stdio"))
    :major-modes '(enh-ruby-mode)
    :priority -1
    :server-id 'solargraph-enh))

  ;; フックで自動セットアップ
  (defun my/setup-ruby-lsp ()
    (let ((client (my/lsp-ruby-client-from-gemfile)))
      (when client
        (setq-local lsp-enabled-clients (list client))
        (lsp-deferred)))))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)           ;; ポップアップ邪魔だから無効
  (lsp-ui-sideline-enable nil)      ;; 行横もうざいので無効
  (lsp-eldoc-enable-hover t)        ;; ミニバッファに表示させる
  (lsp-headerline-breadcrumb-enable nil)) ;; 上のファイルパス表示もOFF

;;; ----------------------------------------------------------------------
;;; copilot
;;; ----------------------------------------------------------------------
(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook ((prog-mode git-commit-mode) . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :custom
  (copilot-indent-offset-warning-disable t)
  :config
  (setq warning-suppress-log-types '((copilot copilot-exceeds-max-char))))

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
          (compilation-mode :align below :size 0.3)
          (rspec-compilation-mode :align below :size 0.3)
          ;;(help-mode :align below :select t :popup t) ;; conflict company-quickhelp
          (calendar-mode :align below :popup t)
          (epa-key-list-mode :align below :size 0.3)
          ("*Backtrace*" :align below :size 0.3 :noselect t)
          ("*Apropos*" :align below :size 0.4 :select t)
          ("*Warnings*" :align below :size 0.1)
          ("*Org Select*" :align below :size 0.3)
          ("^CAPTURE-.*\\.org\\'" :regexp t :align below :size 0.3)
          ;;("*Org Agenda*" :other t :select t)
          (" *Agenda Commands*" :align below :size 0.3)
          (" *Org todo*" :align below :size 0.3 :popup t)
          ("*rg*" :other t :select t :inhibit-window-quit t)
          ("*git-gutter:diff*" :align below :size 0.4)
          ("\\(Messages\\|Report\\)\\*\\'" :regexp t :align below :size 0.3)
          ("*Google Translate*" :align below :size 0.3 :popup t :select t)
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
          epa-key-list-mode
          "\\*Backtrace\\*"
          "\\*Apropos\\*"
          "\\*Warnings\\*"
          "\\(Messages\\|Report\\)\\*\\'"
          "\\*git-gutter:diff\\*"
          ))
  (popper-mode +1))

;;; ----------------------------------------------------------------------
;;; git-gutter.el
;;; ----------------------------------------------------------------------
(use-package git-gutter-fringe
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
(use-package vterm
  :unless (eq window-system 'w32)
  :ensure t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-keymap-exceptions
   '("C-c"
     ;; "C-x"
     "C-u"
     "C-g"
     "C-h"
     "C-l"
     ;; "M-x"
     "M-o"
     "C-v"
     "M-v"
     "C-y"
     "M-y"
     "<f12>")))

(use-package vterm-toggle
  :unless (eq window-system 'w32)
  :ensure t
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
  :diminish whitespace-mode
  :custom-face
  (whitespace-tab ((t (:foreground "#335544" :inverse-video nil :bold t))))
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
          ;; (space-mark   ?\u3000 [?□] [?＿])         ; full-width space - square
          ;; 改行マークを表示すると copilot.el と競合するのでコメントアウト
          ;; (newline-mark ?\n    [?↵ ?\n] [?$ ?\n])    ; eol - downwards arrow
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
  (google-translate-translation-directions-alist '(("en" . "ja")))
  (google-translate-backend-method 'curl)
  :bind ("\C-c t" . google-translate-smooth-translate))

;;; ----------------------------------------------------------------------
;;; gptel
;;; ----------------------------------------------------------------------
(defun my/fetch-api-key (host &optional env-var)
  "指定された HOST または ENV-VAR から APIキーを取得。"
  (or (and env-var (getenv env-var))
      (let* ((auth-info (car (auth-source-search :host host
                                                  :user "apikey"
                                                  :port "https")))
             (secret (plist-get auth-info :secret)))
        (when secret
          (if (functionp secret)
              (funcall secret)
            secret)))
      (error "APIキーが見つかりません: %s or %s" host env-var)))

(defun my/get-openai-api-key ()
  "OpenAI APIキーを取得"
  (my/fetch-api-key "api.openai.com" "OPENAI_API_KEY"))

(defun my/get-anthropic-api-key ()
  "Anthropic APIキーを取得"
  (my/fetch-api-key "api.anthropic.com" "ANTHROPIC_API_KEY"))

(use-package gptel
  :ensure (gptel :type git :host github :repo "karthink/gptel")
  :custom
  (gptel-api-key (my/get-openai-api-key)))

;;; ----------------------------------------------------------------------
;;; japanese-(hankaku|zenkaku)-region の俺俺変換テーブル
;;; ----------------------------------------------------------------------
(with-eval-after-load 'japan-util
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
  (put-char-code-property ?， 'jisx0201 ?,))
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
(defun my/file-open-by-windows (file)
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
  :bind (:map emacs-lisp-mode-map ("C-c C-e" . lispxmp)))

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
  :disabled
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
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-responsive 'top))

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
  :ensure (:host github :repo "valda/tempbuf")
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
;;; ibuffer
;;; ----------------------------------------------------------------------
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (require 'ibuf-ext)
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

;; (use-package all-the-icons-ibuffer
;;   :ensure t
;;   :custom
;;   (all-the-icons-ibuffer-formats
;;    `((mark modified read-only ,(if (>= emacs-major-version 26) 'locked "") vc-status-mini
;;            " " (icon 2 2 :left :elide)
;;            ,(propertize " " 'display `(space :align-to 10))
;;            (name 30 30 :left :elide)
;;            " " (size-h 9 -1 :right)
;;            " " (mode+ 16 16 :left :elide)
;;            " " (coding 12 12 :left)
;;            " " filename-and-process+)
;;      (mark " " (name 30 -1) " " (coding 15 15) " " filename)))
;;   :config
;;   (all-the-icons-ibuffer-mode 1))

(use-package nerd-icons-ibuffer
  :ensure t
  :custom
  (nerd-icons-ibuffer-formats
   `((mark modified read-only ,(if (>= emacs-major-version 26) 'locked "") vc-status-mini
           " " (icon 2 2 :left :elide)
           ,(propertize " " 'display `(space :align-to 10))
           (name 30 30 :left :elide)
           " " (size-h 9 -1 :right)
           " " (mode+ 16 16 :left :elide)
           " " (coding 12 12 :left)
           " " filename-and-process+)
     (mark " " (name 30 -1) " " (coding 15 15) " " filename)))
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

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
  ("C-t" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
             ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (original-neo-smart-open neo-smart-open))
      (setq neo-smart-open t)
      (unwind-protect
          (if (and (fboundp 'neo-global--window-exists-p)
                   (neo-global--window-exists-p))
              (neotree-hide)
            (progn
              (neotree-show)
              (if project-dir
                  (neotree-dir project-dir))
              (if file-name
                  (neotree-find file-name))))
        (setq neo-smart-open original-neo-smart-open)))))

;;; ----------------------------------------------------------------------
;;; which-key
;;; ----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 3.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  :config
  (which-key-setup-side-window-right-bottom)
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
       (global-set-key [mouse-2] 'mouse-yank-at-click))
      ((eq window-system 'w32)
       (global-set-key [mouse-2] 'mouse-yank-at-click)))
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
  :init (exec-path-from-shell-initialize))

;;; ----------------------------------------------------------------------
;;; gnuserv
;;; ----------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;; ----------------------------------------------------------------------
;;; load custom.el
;;; ----------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(cd "~")

(provide 'init)
;;; init.el ends here
