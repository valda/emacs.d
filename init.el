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
(setq inhibit-startup-message t
      scroll-conservatively 1
      next-line-add-newlines nil
      kill-whole-line t
      case-replace t
      transient-mark-mode t
      indent-line-function #'indent-relative-maybe
      line-move-visual nil
      blink-matching-paren nil
      confirm-kill-emacs nil
      indicate-empty-lines t
      ring-bell-function #'ignore
      compilation-scroll-output 'first-error
      find-file-visit-truename t
      vc-follow-symlinks t
      auto-revert-check-vc-info nil
      inhibit-compacting-font-caches t
      imenu-auto-rescan t
      word-wrap-by-category t)

;; モードライン / 表示まわり
(setq mode-line-frame-identification " "
      line-number-mode t
      column-number-mode t)

;; UI / インタフェース
(setq use-dialog-box nil)

;; ediff
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

;; バックアップ / 保存まわり
(setq backup-directory-alist `(("." . ,(expand-file-name "~/bak")))
      delete-old-versions t
      make-backup-files t)

;; 補完 / 履歴
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      search-default-regexp-mode nil
      history-length t
      read-extended-command-predicate #'command-completion-default-include-p)

;; ユーザー情報
(setq user-full-name "YAMAGUCHI, Seiji"
      user-mail-address "valda68k@gmail.com")

;; バッファローカル系
(setq-default
 indent-tabs-mode nil
 tab-width 4
 truncate-partial-width-windows nil
 require-final-newline t
 mode-require-final-newline t)

(temp-buffer-resize-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(delete-selection-mode t)
(show-paren-mode t)
(pixel-scroll-precision-mode t)
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
;; 1234567890abcdefghijklmn |
;; あいうえおかきくけこさし |
;; 🥹😼🐕🎴🌈🕒🍣🍰🍲🍗😭✨ |
;; ■□◆◇←↓↑→●◎◯… |

(setq-default line-spacing 0)           ;; 行間を狭くする
(setq use-default-font-for-symbols nil) ;; シンボルは他のフォントから補完
(set-face-attribute 'default nil :font "MomiageMono Nerd Font" :height 150)
;; ■□◆◇←↓↑→ のフォントを Ricty Discord で描画
(dolist (c '(?… ?■ ?□ ?◆ ?◇ ?← ?↓ ?↑ ?→ ?◯ ?● ?◎)) (set-fontset-font t c "Ricty Discord"))
(add-to-list 'face-font-rescale-alist '(".*Ricty Discord.*" . 1.2))
;; Emoji Symbols (U+1F300-U+1F5FF) を Noto Color Emoji で描画
(set-fontset-font t '(#x1F300 . #x1F5FF) "Noto Color Emoji" nil 'prepend)
(add-to-list 'face-font-rescale-alist '(".*Noto Color Emoji.*" . 0.95))

;;; ----------------------------------------------------------------------
;;; elpaca
;;; ----------------------------------------------------------------------
(defvar elpaca-installer-version 0.11)
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode +1))

;;; ----------------------------------------------------------------------
;;; use-package のログ・統計・imenu設定
;;; ----------------------------------------------------------------------
(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0.01
      use-package-enable-imenu-support t)

(defun my/imenu-use-package-hook ()
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\'" (buffer-file-name)))
    (add-to-list 'imenu-generic-expression
                 '("Use-Package" "^\\s-*(use-package\\s-+\\(\\_<.+?\\_>\\)" 1))))

(add-hook 'emacs-lisp-mode-hook #'my/imenu-use-package-hook)

;;; ----------------------------------------------------------------------
;;; diminish (use-packageが利用)
;;; ----------------------------------------------------------------------
(use-package diminish
  :ensure t
  :demand t)

;;; ----------------------------------------------------------------------
;;; exec-path-from-shell
;;; ----------------------------------------------------------------------
(use-package exec-path-from-shell
  :unless (eq window-system 'w32)
  :ensure t
  :init (exec-path-from-shell-initialize))

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
  (add-hook 'isearch-mode-end-hook (lambda () (setq w32-ime-composition-window nil))))

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
    (bind-keys
     ;; 変換キーで入力メソッドを有効化
     ([henkan] . (lambda () (interactive) (activate-input-method default-input-method)))
     ;; XF86Launch5キーでも入力メソッドを有効化
     ("<XF86Launch5>" . (lambda () (interactive) (activate-input-method default-input-method)))
     ;; 無変換キーで入力メソッドを無効化
     ([muhenkan] . (lambda () (interactive) (deactivate-input-method)))
     ;; XF86Toolsキーで入力メソッドを無効化
     ("<XF86Tools>" . (lambda () (interactive) (deactivate-input-method)))
     ;; 全角半角キーで入力メソッドを切り替え
     ([zenkaku-hankaku] . toggle-input-method)
     ;; isearch-modeのキー設定
     :map isearch-mode-map
     ([henkan] . isearch-toggle-input-method)
     ([muhenkan] . isearch-toggle-input-method)
     ([zenkaku-hankaku] . isearch-toggle-input-method))

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
          '((direct        . "YellowGreen")
            (read-only     . "LightGoldenrod")
            (hiragana      . "IndianRed")
            (full-katakana . "goldenrod")
            (half-ascii    . "dark orchid")
            (full-ascii    . "orchid")
            (half-katakana . "dark goldenrod")))))

(cond ((eq window-system 'w32)
       (my/w32-ime-init))
      (t
       (my/mozc-init)))

;;; ----------------------------------------------------------------------
;;; nerd-icons
;;; ----------------------------------------------------------------------
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :hook (emacs-startup . nerd-icons-completion-mode))

;;; ---------------------------------------------------------------------
;;; solarized-theme
;;; ----------------------------------------------------------------------
(use-package solarized-theme
  :disabled
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

;;; ---------------------------------------------------------------------
;;; catppuccin-theme
;;; ----------------------------------------------------------------------
(use-package catppuccin-theme
  :if (display-graphic-p)
  :ensure t
  :custom
  (catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin :no-confirm))

;;; ----------------------------------------------------------------------
;;; doom-modeline
;;; ----------------------------------------------------------------------
(use-package doom-modeline
  :if (display-graphic-p)
  :ensure t
  :hook (emacs-startup . doom-modeline-mode)
  :init
  (setq doom-modeline-github nil)
  :custom
  (doom-modeline-height 34)
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-workspace-name nil)
  :custom-face
  (doom-modeline-highlight ((t (:foreground "GhostWhite" :background "chocolate4" :inherit mode-line-buffer-id))))
  (doom-modeline-panel     ((t (:inherit doom-modeline-highlight))))
  (doom-modeline-bar       ((t (:background "IndianRed" :inherit mode-line-buffer-id)))))


;;; ----------------------------------------------------------------------
;;; window-divider
;;; ----------------------------------------------------------------------
(setq window-divider-default-right-width 5)
(let ((default-bg (face-attribute 'default :background)))
  (set-face-foreground 'window-divider default-bg)
  (set-face-foreground 'window-divider-first-pixel default-bg)
  (set-face-foreground 'window-divider-last-pixel "gray30"))
(window-divider-mode 1)

;;; ----------------------------------------------------------------------
;;; nyan-mode
;;; ----------------------------------------------------------------------
(use-package nyan-mode
  :if (display-graphic-p)
  :ensure t
  :hook (emacs-startup . nyan-mode)
  :custom (nyan-bar-length 16) (nyan-wavy-trail t)
  :config (nyan-start-animation))

;;; ----------------------------------------------------------------------
;;; pulsar
;;; ----------------------------------------------------------------------
(use-package pulsar
  :ensure t
  :config
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-blue)
  ;; integration with the `consult' package:
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-center)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-center)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)

  (pulsar-global-mode 1))

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

  ;; ユーティリティ関数
  (defun my/toggle-tab-bar-tab-hints ()
    (interactive)
    (setq tab-bar-tab-hints (not tab-bar-tab-hints)))

  (defun my/toggle-tab-bar-mode ()
    (interactive)
    (tab-bar-mode (if tab-bar-mode -1 1)))

  ;; 基本操作
  (define-key my/tab-prefix-key-map "c" #'tab-new)
  (define-key my/tab-prefix-key-map "\C-c" #'tab-new)
  (define-key my/tab-prefix-key-map "k" #'tab-close)
  (define-key my/tab-prefix-key-map "\C-k" #'tab-close)
  (define-key my/tab-prefix-key-map "K" #'tab-close-other)
  (define-key my/tab-prefix-key-map "p" #'tab-previous)
  (define-key my/tab-prefix-key-map "\C-p" #'tab-previous)
  (define-key my/tab-prefix-key-map "n" #'tab-next)
  (define-key my/tab-prefix-key-map "\C-n" #'tab-next)
  (define-key my/tab-prefix-key-map "\C-z" #'tab-recent)
  (define-key my/tab-prefix-key-map "'" #'tab-bar-switch-to-tab)
  (define-key my/tab-prefix-key-map "A" #'tab-bar-rename-tab)
  (define-key my/tab-prefix-key-map "i" #'my/toggle-tab-bar-tab-hints)
  (define-key my/tab-prefix-key-map "t" #'my/toggle-tab-bar-mode)

  ;; 1〜9 に tab-select を割り当てる
  (dotimes (i 9)
    (define-key my/tab-prefix-key-map
                (number-to-string (1+ i))
                `(lambda () (interactive) (tab-select ,(1+ i)))))

  (bind-key "C-z" my/tab-prefix-key-map))

;;; ----------------------------------------------------------------------
;;; which-key
;;; ----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;;; ----------------------------------------------------------------------
;;; vertico
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
;;; consult/marginalia/embark
;;; ----------------------------------------------------------------------
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-," . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-;" . consult-buffer)
         ("C-x b" . consult-buffer)
         ("C-c b" . consult-bookmark)
         ("C-c i" . consult-imenu)
         ("M-y" . consult-yank-pop)
         ("M-g" . consult-goto-line)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep-dwim)
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

  (defun consult-ripgrep-dwim ()
    "DWIM version of `consult-ripgrep'.
1. Region active → use region as initial query.
2. Symbol at point → use symbol as initial query.
3. Otherwise → empty query.
Search directory: project root if available, else `default-directory'."
    (interactive)
    (let ((initial (cond
                    ((use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))
                    ((thing-at-point 'symbol t))
                    (t nil)))
          (dir (or (consult--project-root) default-directory)))
      (consult-ripgrep dir initial)))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)))

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
  (corfu-popupinfo-mode))

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
  :init
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package company
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-gtags))
  (advice-add 'company-mode :override (lambda (&optional _) (message "🔇 company-mode suppressed"))))

(use-package nerd-icons-corfu
  :ensure (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; ----------------------------------------------------------------------
;;; yasnippet.el
;;; ----------------------------------------------------------------------
;; (use-package yasnippet
;;   :ensure t
;;   :diminish yas-minor-mode
;;   :hook
;;   (prog-mode . yas-minor-mode)
;;   :config
;;   ;; 無効化: デフォルトのTABバインディング
;;   (bind-keys :map yas-minor-mode-map
;;              ("<tab>" . nil)
;;              ("TAB" . nil))
;;   (yas-reload-all))

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet)


;;; ----------------------------------------------------------------------
;;; tempel
;;; ----------------------------------------------------------------------
(use-package tempel
  :ensure t
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook #'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-setup-capf)
  (add-hook 'text-mode-hook #'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode))

(use-package tempel-collection :ensure t)

;;; ----------------------------------------------------------------------
;;; dabbrev / hippie-expand
;;; ----------------------------------------------------------------------
(with-eval-after-load 'abbrev
  (with-eval-after-load 'diminish
    (diminish 'abbrev-mode)))

(defun try-expand-abbrev (old)
  (if (expand-abbrev) t nil))

;; 補完の挙動設定
(setq dabbrev-case-fold-search t   ; 補完時に大文字小文字を区別しない
      dabbrev-case-replace t       ; 挿入時も元の単語の大文字小文字に合わせる
      hippie-expand-try-functions-list
      '(try-expand-abbrev
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))

;; (with-eval-after-load 'yasnippet
;;   (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand))

;; M-/ で hippie-expand（`dabbrev-expand` の上位互換）
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
          (lambda ()
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
    (defhydra hydra-iflipb (:hint nil)
      "iflipb"
      ("<left>"  iflipb-previous-buffer "previous" :exit nil)
      ("<right>" iflipb-next-buffer "next" :exit nil)
      ("<down>" consult-buffer "consult-buffer" :exit t)
      ("q" nil "quit" :exit t)
      ("<return>" nil)
      ))

  (defun my/iflipb-previous-and-hydra ()
    (interactive)
    (iflipb-previous-buffer)
    (hydra-iflipb/body))

  (defun my/iflipb-next-and-hydra ()
    (interactive)
    (iflipb-next-buffer nil)
    (hydra-iflipb/body))

  (bind-key "C-x <right>" #'my/iflipb-next-and-hydra)
  (bind-key "C-x <left>"  #'my/iflipb-previous-and-hydra))

;;; ----------------------------------------------------------------------
;;; winner-mode
;;; ----------------------------------------------------------------------
(use-package winner
  :custom
  (winner-dont-bind-my-keys t)  ;; ← 自動バインドを無効にしておく
  :config
  (winner-mode 1)

  (with-eval-after-load 'hydra
    (defhydra hydra-winner (:hint nil)
      "winner"
      ("<left>"  winner-undo "undo" :exit nil)
      ("<right>" winner-redo "redo" :exit nil)
      ("q" nil "quit" :exit t)
      ("<return>" nil)
      ))

  (defun my/winner-undo-and-hydra ()
    (interactive)
    (winner-undo)
    (hydra-winner/body))

  (defun my/winner-redo-and-hydra ()
    (interactive)
    (winner-redo)
    (hydra-winner/body))

  (bind-key "C-c <left>" #'my/winner-undo-and-hydra)
  (bind-key "C-c <right>" #'my/winner-redo-and-hydra))

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
  :bind ("C-." . undo-tree-redo)
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
;;; dired 関係の設定
;;; ----------------------------------------------------------------------
(setq ls-lisp-ignore-case t                 ; ファイル名の大文字・小文字を区別しない
      ls-lisp-dirs-first t                  ; ディレクトリを先に表示（ls-lisp 用）
      dired-listing-switches "-aFl --group-directories-first" ; GNU ls 用表示オプション
      dired-dwim-target t                   ; 他ウィンドウに Dired があればそこをコピー先に提案
      dired-recursive-copies 'always        ; 再帰コピーを常に確認なしでOK
      dired-isearch-filenames t             ; isearch 時にファイル名のみ検索
      dired-omit-mode t)                    ; ドットファイルなどを非表示にする

(defun my/dired-open-file ()
  "In Dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(defun my/dired-copy-file-path-relative ()
  "Copy file path relative to projectile root, or full path if not in a project."
  (interactive)
  (let* ((full-path (dired-get-file-for-visit))
         (path (if (and (fboundp 'projectile-project-root)
                        (projectile-project-p))
                   (file-relative-name full-path (projectile-project-root))
                 full-path)))
    (kill-new path)
    (message "Copied: %s" path)))

(with-eval-after-load 'dired
  (require 'dired-x)
  (require 'wdired)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
  (bind-key "C-c o" 'my/dired-open-file dired-mode-map)
  (bind-key "W" 'my/dired-copy-file-path-relative dired-mode-map)
  (advice-add 'wdired-finish-edit
              :after (lambda ()
                       (deactivate-input-method)
                       (dired-k))))

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
;;; adaptive-wrap
;;; ----------------------------------------------------------------------
(use-package adaptive-wrap
  :ensure t
  :custom
  (adaptive-wrap-extra-indent 0)
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;;; ----------------------------------------------------------------------
;;; org-mode
;;; ----------------------------------------------------------------------
(defvar my/syncthing-directory "~/Sync/") ; Syncthing で同期しているディレクトリ
(use-package org
  :ensure t
  :custom
  (org-directory (expand-file-name "org" my/syncthing-directory))
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
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-display-inline-images)
  (bind-keys ("\C-c a" . org-agenda)
             :map org-mode-map
             ([S-C-up]   . nil)
             ([S-C-down] . nil)
             ("C-c ,"    . nil)
             ("C-'"      . nil)
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

(use-package org-modern
  :ensure t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (setq org-modern-todo-faces '(("SOMEDAY"  :background "cyan4" :foreground "black")
                                ("WAITING"  :background "DarkOrange2"    :foreground "black"))))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-directory (expand-file-name "org/roam" my/syncthing-directory))
  :custom
  (org-roam-db-update-method 'idle)
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
        '(("d" "Default" plain "%?"
           :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :note:\n#+date: %U\n\n")
           :unnarrowed t)))

  ;; 日次ノートのテンプレート
  (setq org-roam-dailies-capture-templates
        '(("d" "Default" entry
           "* %<%H:%M> %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n")))))

;;; ----------------------------------------------------------------------
;;; markdown-mode
;;; ----------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (markdown-fontify-code-blocks-natively t)
  (markdown-fontify-headings-face-dynamically t)
  (markdown-indent-on-enter 'indent-and-new-item))

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
  (add-hook 'kill-emacs-hook (lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))
  ;; M$ Visual Studio key setup.
  (bind-key "<C-f2>" 'bm-toggle)
  (bind-key "<f2>"   'bm-next)
  (bind-key "<S-f2>" 'bm-previous))

;;; ----------------------------------------------------------------------
;;; xref
;;; ----------------------------------------------------------------------
(use-package xref
  :bind ( ;; ("M-." . xref-find-definitions) ; embark-dwim に割り当て
         ("M-?" . xref-find-references)
         ("M-," . xref-go-back))
  :custom
  (xref-search-program 'ripgrep))

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
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("C-c C-p" . projectile-command-map))
  :init
  (setq projectile-project-search-path '("~/wc"))
  (setq projectile-ignored-project-function
        (lambda (project-root)
          (string-prefix-p (expand-file-name "~/.emacs.d/elpaca/repos/")
                           (expand-file-name project-root))))
  (setq projectile-auto-discover t)
  (setq projectile-auto-cleanup-known-projects t)
  :config
  (projectile-mode +1))

(use-package projectile-rails
  :ensure t
  :config
  (bind-key "C-c r" 'projectile-rails-command-map projectile-rails-mode-map)
  ;; (add-hook 'projectile-rails-mode-hook
  ;;           (lambda ()
  ;;             (with-eval-after-load 'yasnippet
  ;;               (yas-activate-extra-mode 'rails-mode))))
  (projectile-rails-global-mode))

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :config
  (bind-keys :map projectile-mode-map
             ("C-c p p" . consult-projectile-switch-project)
             ("C-c p d" . consult-projectile-find-dir)
             ("C-c p f" . consult-projectile-find-file)))

;;; ----------------------------------------------------------------------
;;; lsp-mode
;;; ----------------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (((ruby-mode ruby-ts-mode enh-ruby-mode) . my/setup-ruby-lsp)
         ((typescript-mode tsx-ts-mode js-ts-mode) . lsp-deferred))
  :custom
  (lsp-completion-provider :capf)
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-pyright-use-library-code-for-types t)
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-clients-svelte-server "svelteserver")
  (lsp-enable-snippet nil)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git$"
     "[/\\\\]node_modules$"
     "[/\\\\]vendor$"
     "[/\\\\]\\.bundle$"
     "[/\\\\]tmp$"
     "[/\\\\]log$"
     "[/\\\\]public/packs$"))
  :bind (:map lsp-mode-map
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer))
  :config
  ;; Gemfile に任意の gem が含まれているかどうかをチェックする関数
  (defun my/gemfile-has (gem-name)
    (when-let ((gemfile (locate-dominating-file default-directory "Gemfile")))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "Gemfile" gemfile))
        (re-search-forward (format "gem ['\"]%s['\"]" gem-name) nil t))))

  ;; (enh-)ruby-mode 用LSPの設定
  (defun my/setup-ruby-lsp ()
    (when-let ((client
                (cond
                 ((my/gemfile-has "ruby-lsp") 'ruby-lsp-ls)
                 ((my/gemfile-has "solargraph") 'ruby-ls)
                 (t nil))))
      ;; bundler経由で起動する設定
      (setq-local lsp-ruby-lsp-use-bundler t)
      (setq-local lsp-solargraph-use-bundler t)
      (setq-local lsp-enabled-clients (list client))
      (lsp-deferred))))

(use-package lsp-pyright
  :if (executable-find "pyright")
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :config
  (setq lsp-pyright-typechecking-mode "basic"))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)                 ;; ポップアップ邪魔だから無効
  (lsp-ui-doc-position 'at-point)         ;; ポップアップの位置をカーソル位置に
  (lsp-ui-sideline-enable t)              ;; サイドラインの表示 → ON
  (lsp-ui-sideline-show-hover nil)        ;; カーソル乗せたときの doc 表示 → OFF（うざい）
  (lsp-ui-sideline-show-code-actions nil) ;; アクション（リファクタリングのヒントなど）の表示 → OFF
  (lsp-ui-sideline-show-diagnostics t)    ;; 警告・エラー表示 → ON（便利）
  (lsp-ui-sideline-show-symbol nil)       ;; カーソル下の symbol 情報 → OFF（チラつく）
  (lsp-ui-sideline-delay 0.5)             ;; 表示までの遅延 → チラつき防止
  (lsp-ui-sideline-ignore-duplicate t)    ;; 同じメッセージ繰り返さない
  :custom-face
  ;; catppuccin macchiato 風
  (lsp-ui-peek-peek         ((t (:background "#1E2030"))))
  (lsp-ui-peek-list         ((t (:background "#1E2030"))))
  (lsp-ui-peek-filename     ((t (:foreground "#8AADF4" :weight bold))))
  (lsp-ui-peek-selection    ((t (:background "#363A4F" :foreground "#CAD3F5" :weight bold))))
  (lsp-ui-peek-header       ((t (:background "#1E2030" :foreground "#C6A0F6" :weight bold))))
  (lsp-ui-peek-highlight    ((t (:background "#EED49F" :foreground "#24273A"
                                             :distant-foreground "#F4DBD6"
                                             :box (:line-width -1 :color "#A5ADCB")))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c C-i" . lsp-ui-imenu)
              ("C-c C-d" . lsp-ui-doc-glance)))

;;; ----------------------------------------------------------------------
;;; editorconfig
;;; ----------------------------------------------------------------------
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;; ----------------------------------------------------------------------
;;; whitespace-mode
;;; ----------------------------------------------------------------------
(use-package whitespace
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
  (with-eval-after-load 'whitespace
    (with-eval-after-load 'diminish
      (diminish 'whitespace-mode)))
  (global-whitespace-mode 1))

;;; ----------------------------------------------------------------------
;;; 行末の空白とファイル末尾の余分な改行を削除する
;;; ----------------------------------------------------------------------
(defun my/cleanup-for-spaces ()
  "Delete trailing whitespace and excessive newlines at the end of the buffer."
  (interactive)
  (delete-trailing-whitespace)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook #'my/cleanup-for-spaces)

(defun toggle-cleanup-for-spaces ()
  "Toggle automatic cleanup of trailing spaces before save."
  (interactive)
  (if (memq #'my/cleanup-for-spaces before-save-hook)
      (progn
        (remove-hook 'before-save-hook #'my/cleanup-for-spaces)
        (message "🧹 Cleanup for spaces: OFF"))
    (add-hook 'before-save-hook #'my/cleanup-for-spaces)
    (message "🧹 Cleanup for spaces: ON")))

(global-set-key (kbd "C-c M-d") #'toggle-cleanup-for-spaces)

;;; ----------------------------------------------------------------------
;;; cc-mode
;;; ----------------------------------------------------------------------
(defconst my/cc-style
  '(
    ;; インデント幅を空白2コ分に設定
    (c-basic-offset . 2)
    ;; tab キーでインデントを実行
    (c-tab-always-indent . t)
    ;; コメント行のインデント幅
    (c-comment-only-line-offset . 0)

    ;; カッコ前後の自動改行処理の設定
    (c-hanging-braces-alist
     . (
        (class-open before after)   ; クラス宣言の'{'前後
        (class-close before)        ; クラス宣言の'}'前
        (defun-open before after)   ; 関数宣言の'{'前後
        (defun-close before after)  ; 関数宣言の'}'前後
        (inline-close after)        ; インライン関数の'}'後
        (brace-list-open after)     ; 配列・列挙型宣言の'{'後
        (brace-list-close before)   ; 配列・列挙型宣言の'}'前
        (block-open after)          ; ブロックの'{'後
        (block-close . c-snug-do-while) ; ステートメントの'}'前
        (substatement-open after)   ; サブステートメントの'{'後
        (statement-case-open after) ; case 文の'{'後
        (extern-lang-open before after) ; 他言語リンケージの'{'前後
        (extern-lang-close before)  ; 他言語リンケージの'}'前
        (namespace-open before after) ; 名前空間の'{'前後
        (namespace-close before)    ; 名前空間の'}'前
        ))

    ;; コロン前後の自動改行処理の設定
    (c-hanging-colons-alist
     . (
        (case-label after)          ; case ラベルの':'後
        (label after)               ; ラベルの':'後
        (access-label after)        ; アクセスラベルの':'後
        (member-init-intro after)   ; コンストラクタのメンバ初期化の':'後
        ))

    ;; 挿入された余計な空白文字のキャンセル条件設定
    (c-cleanup-list . (
                       brace-else-brace   ; else の直前の空白を削除
                       brace-elseif-brace ; else if の直前の空白を削除
                       brace-catch-brace  ; catch の直前の空白を削除
                       empty-defun-braces ; 関数定義後の空白削除
                       defun-close-semi   ; 関数閉じ括弧後のセミコロン削除
                       list-close-comma   ; 配列宣言後のコンマ削除
                       scope-operator     ; スコープ演算子間の空白削除
                       ))

    ;; オフセット設定
    (c-offsets-alist
     . (
        (arglist-intro . ++)       ; 引数リスト開始行
        (arglist-close . c-lineup-arglist) ; 引数リスト終了行
        (substatement-open . 0)    ; サブステートメントの開始行
        (statement-case-open . +)  ; case 文後の'{'位置
        (statement-cont . ++)      ; ステートメント継続行
        (case-label . 0)           ; case 文ラベル位置
        (label . 0)                ; ラベル位置
        (block-open . 0)           ; ブロック開始行
        (inline-open . 0)          ; インライン関数開始中括弧
        (member-init-intro . ++)   ; メンバ初期化リスト開始行
        ))

    ;; インデント時に構文解析情報を表示
    (c-echo-syntactic-information-p . t)
    )
  "My C/C++ Programming Style")

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; my/cc-stye を登録して有効にする
            (c-add-style "PERSONAL" my/cc-style t)
            ;; 自動改行を有効にする
            (when (fboundp 'c-toggle-auto-newline)
              (c-toggle-auto-newline t))
            ;; セミコロンで自動改行しない
            (setq c-hanging-semi&comma-criteria nil)
            ;; コンパイルコマンドの設定
            (setq compile-command "make -k") ; Cygwin の make
            (setq compilation-window-height 16)
            ;; キーバインディング設定
            (define-key c-mode-base-map "\C-cc" 'compile)
            (define-key c-mode-base-map "\C-xt" 'ff-find-other-file)
            (define-key c-mode-base-map [mouse-2] 'ff-mouse-find-other-file)))

;; C/C++ ファイルの自動モード設定
(setq auto-mode-alist
      (append '(("\\.C\\'"            . c-mode)
                ("\\.[Hh]\\'"         . c++-mode)
                ("\\.[Hh][Pp][Pp]\\'" . c++-mode))
              auto-mode-alist))
;;; ----------------------------------------------------------------------
;;; (enhanced-)ruby-mode
;;; ----------------------------------------------------------------------
(use-package enh-ruby-mode
  :ensure t
  :interpreter ("ruby")
  :mode ("\\.rb\\'"
         "\\.rake\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "\\.prawn\\'"
         "\\.jbuilder\\'"
         "\\.xremap\\'"
         "\\(Rake\\|Cap\\|Gem\\|Guard\\)file\\'"
         "config\\.ru\\'")
  :custom
  (enh-ruby-add-encoding-comment-on-save nil)
  (enh-ruby-deep-indent-paren nil)
  (enh-ruby-deep-indent-construct nil)
  :config
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (inf-ruby-minor-mode t)
              (electric-indent-mode t)
              (electric-layout-mode t)
              (rubocop-mode t)
              (modify-syntax-entry ?: ".")
              (modify-syntax-entry ?@ "w")
              (modify-syntax-entry ?$ "w")
              (modify-syntax-entry ?? "w")
              (modify-syntax-entry ?! "w")
              (modify-syntax-entry ?= "w"))))

(use-package inf-ruby
  :ensure t)

(use-package ruby-end
  :ensure t
  :hook (enh-ruby-mode . ruby-end-mode)
  :diminish ruby-end-mode)

(use-package rubocop
  :ensure t
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

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)
  (add-hook 'python-mode-hook
            (lambda ()
              (when-let ((venv (locate-dominating-file default-directory ".venv")))
                (pyvenv-activate (expand-file-name ".venv" venv))))))

;;; ----------------------------------------------------------------------
;;; add-node-module-path
;;; ----------------------------------------------------------------------
(use-package add-node-modules-path
  :ensure t
  :defer t)

;;; ----------------------------------------------------------------------
;;; php-mode
;;; ----------------------------------------------------------------------
(use-package php-mode
  :ensure t
  :config
  (defun my/php-mode-setup ()
    (php-enable-psr2-coding-style)
    (setq flycheck-phpcs-standard "PSR2")
    (electric-indent-mode t)
    (electric-layout-mode t)
    (define-key php-mode-map '[(control .)] nil)
    (define-key php-mode-map '[(control c)(control .)] 'php-show-arglist)
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4)
    (setq-local indent-tabs-mode nil)
    (c-set-offset 'arglist-cont-nonempty' +)
    (c-set-offset 'case-label +))
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
  (web-mode-block-padding 2)
  (web-mode-part-padding 2)
  (web-mode-enable-auto-closing t)
  (web-mode-auto-close-style 2)

  :config
  (defun my/web-mode-setup ()
    (add-node-modules-path)
    (setq web-mode-indentation-params
          '(("case-extra-offset" . nil)))
    (cond
     ((string= web-mode-engine "erb")
      (modify-syntax-entry ?% "w")
      (modify-syntax-entry ?? "w"))
     ((string= web-mode-engine "php")
      (modify-syntax-entry ?? "w"))))

  (add-hook 'web-mode-hook #'my/web-mode-setup)

  (defun my/web-mode-svelte-setup ()
    "Svelte 用に web-mode の継続行インデントを 2スペ固定にする。"
    (when (and buffer-file-name
               (string= (file-name-extension buffer-file-name) "svelte"))
      ;; 基本オフセットを 2 に変更
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-indentation-params
            '(("lineup-args"       . nil)     ; 引数リスト
              ("lineup-calls"      . nil)     ; 関数呼び出しチェーン
              ("lineup-concats"    . nil)     ; 文字列の連結
              ("lineup-quotes"     . nil)     ; 文字列リテラル
              ("lineup-ternary"    . nil)     ; ?: 三項演算子
              ("lineup-operators"  . nil)     ; + - * / 等
              ("case-extra-offset" . nil)     ; case 文
              ))))

  (add-hook 'web-mode-hook #'my/web-mode-svelte-setup))

;;; ----------------------------------------------------------------------
;;; js-mode
;;; ----------------------------------------------------------------------
(setq js-indent-level 2
      js-indent-align-list-continuation nil)

;;; ----------------------------------------------------------------------
;;; js2-mode
;;; ----------------------------------------------------------------------
(use-package js2-mode
  :if (< emacs-major-version 27)
  :ensure t
  :custom
  (js2-include-browser-externs nil)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  (js2-highlight-external-variables nil)
  (js2-include-jslint-globals nil)
  :config
  (add-hook 'js2-mode-hook
            (lambda()
	          (add-node-modules-path)
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
;;; svelte-mode
;;; ----------------------------------------------------------------------
(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'"
  :hook (svelte-mode . lsp))

;;; ----------------------------------------------------------------------
;;; json-mode
;;; ----------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.babelrc\\'" "\\.eslintrc\\'"))

;; jqでJSONを整形
(defun jq-format (beg end)
  "Format JSON in the region using jq."
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;; ----------------------------------------------------------------------
;;; typescript-mode
;;; ----------------------------------------------------------------------
(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
	          (add-node-modules-path)
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
            (lambda ()
              (when (string-match "ansible.*/\\(tasks\\|handlers\\)/.*\\.yml\\'"
                                  (buffer-file-name (current-buffer)))
                (ansible 1)))))

;;; ----------------------------------------------------------------------
;;; その他の major-mode
;;; ----------------------------------------------------------------------
(use-package coffee-mode
  :ensure t
  :mode ("\\.coffee\\'" "\\.coffee\\.erb\\'")
  :config
  (add-hook 'coffee-mode-hook
            (lambda()
              (setq-local tab-width 2)
              (setq coffee-tab-width 2))))

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
;;; tree-sitter
;;; ----------------------------------------------------------------------
(setq treesit-language-source-alist
      '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python"))))

;; 一度だけ実行（C-x C-eで評価）
;; (mapc #'treesit-install-language-grammar '(javascript typescript tsx ruby python))

(add-to-list 'major-mode-remap-alist
             '(javascript-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(typescript-mode . tsx-ts-mode)) ;; JSX込みならtsx-ts-mode

;;; ----------------------------------------------------------------------
;;; その他の拡張子に対応する編集モードを設定
;;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ("\\.[ch]java\\'"          . java-mode)     ;; i-appli
                ("\\.doc\\'"               . text-mode)
                ("\\.text\\.erb\\'"        . text-mode)     ;; Text(erb)
                ("\\.rtext\\'"             . text-mode)     ;; Text(erb)
                ("\\.[mc]js\\'"            . js-mode)
                )
              auto-mode-alist))

;;; ----------------------------------------------------------------------
;;; treemacs
;;; ----------------------------------------------------------------------
(use-package treemacs
  :ensure t
  :after projectile
  :bind
  (("C-c t t" . treemacs)                 ; トグル表示
   ("C-c t f" .                           ; 現在のファイルにジャンプ
    my/treemacs-find-file-or-add-project+focus)
   ("C-'"     . my/toggle-treemacs-focus) ; スマートトグル
   :map treemacs-mode-map
   ("<left>"  . treemacs-COLLAPSE-action)
   ("<right>" . treemacs-TAB-action))
  :custom
  (treemacs-width 30)                     ; ウィンドウの幅
  (treemacs-no-png-images t)              ; PNGアイコンを使わない
  (treemacs-is-never-other-window nil)    ; 他のウィンドウに切り替えない
  (treemacs-silent-refresh t)             ; 静かに再描画
  (treemacs-silent-filewatch t)
  (treemacs-follow-after-init t)          ; 起動時にプロジェクトを追尾
  (treemacs-text-scale -1)
  (treemacs-select-when-already-in-treemacs 'close)
  :config
  (defun my/treemacs-find-file-or-add-project+focus ()
    "Add project if needed, jump to file (or dir in dired) in Treemacs, and focus Treemacs window."
    (interactive)
    (let* ((path (cond
                  ;; diredバッファなら開いているディレクトリを使う
                  ((derived-mode-p 'dired-mode)
                   (expand-file-name default-directory))
                  ;; それ以外は visiting file or default-directory
                  (buffer-file-name)
                  (t (expand-file-name default-directory))))
           (project (treemacs--find-project-for-path path)))
      ;; プロジェクトが登録されてなければ追加
      (unless project
        (let ((root (when (and (fboundp 'projectile-project-root)
                               (ignore-errors (projectile-project-root)))
                      (projectile-project-root))))
          (if root
              (treemacs-add-project-to-workspace
               root (file-name-nondirectory (directory-file-name root)))
            ;; projectile で取れなかった場合は path 側を追加
            (let ((dir (if (file-directory-p path)
                           (expand-file-name path)
                         (file-name-directory (expand-file-name path)))))
              (treemacs-add-project-to-workspace
               dir (file-name-nondirectory (directory-file-name dir)))))))
      ;; Treemacs側でファイルを表示
      ;; デバッグ出力
      (message "path: %s, project: %s" path project)
      (if (file-directory-p path)
          (treemacs-select-window)
        (treemacs-find-file))))

  (defun my/toggle-treemacs-focus ()
    "Toggle Treemacs visibility and focus intelligently.
- If Treemacs is focused, close it.
- If Treemacs is open but not focused, focus it.
- If Treemacs is not open, open it."
    (interactive)
    (let ((treemacs-window (treemacs-get-local-window)))
      (cond
       ;; Treemacs is focused → kill the window (close)
       ((eq (selected-window) treemacs-window)
	    (delete-window treemacs-window))
       ;; Treemacs is open but not focused → focus it
       (treemacs-window
	    (select-window treemacs-window))
       ;; Treemacs is not open → open it
       (t
	    (my/treemacs-find-file-or-add-project+focus)))))

  ;; UIカスタム：背景色をテーマに連動して明るめにする
  (defun my/treemacs-set-bg-based-on-theme ()
    "Set Treemacs background based on current theme's default background."
    (let* ((default-bg (face-attribute 'default :background nil t))
           (treemacs-bg (color-lighten-name default-bg 10)))
      (custom-set-faces
       `(treemacs-window-background-face ((t (:background ,treemacs-bg)))))))

  ;; モード起動時にUI/フォント調整
  (defun my/treemacs-ui-setup ()
    "Setup UI tweaks for Treemacs buffer."
    (variable-pitch-mode 1)
    (setq variable-pitch-use-font-rescale nil)
    (setq-local line-spacing 2)
    (setq-local truncate-lines t))

  ;; モードフック登録
  (add-hook 'treemacs-mode-hook #'my/treemacs-set-bg-based-on-theme)
  (add-hook 'treemacs-mode-hook #'my/treemacs-ui-setup)
  (add-hook 'after-load-theme-hook #'my/treemacs-set-bg-based-on-theme)
  (add-hook 'image-mode-hook #'treemacs-find-file)

  ;; Treemacs上で tab-new したとき、scratchバッファを自動で開く
  (with-eval-after-load 'tab-bar
    (defun my/tab-bar-new-tab-with-fallback (orig_fn &rest args)
      (let ((tab (apply orig_fn args)))
        (when (string-prefix-p " *Treemacs-" (buffer-name))
          (switch-to-buffer "*scratch*"))
        tab))
    (advice-add 'tab-bar-new-tab :around #'my/tab-bar-new-tab-with-fallback))

  ;; Treemacs機能のON
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-git-commit-diff-mode t))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  :config
  (treemacs-project-follow-mode))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-tab-bar
  :disabled  ; treemacs-follow-mode との競合を避けるために無効化
  :ensure t
  :after (treemacs tab-bar)
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;;; ----------------------------------------------------------------------
;;; flycheck
;;; ----------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :hook (emacs-startup . global-flycheck-mode)
  :diminish flycheck-mode
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

;; flymake のハイライトを無効にする
(with-eval-after-load 'flymake
  ;; ビットマップ（左フリンジのアイコン）を非表示にする
  (setq flymake-error-bitmap nil
        flymake-warning-bitmap nil
        flymake-note-bitmap nil)
  ;; アンダーラインをオフにする
  (set-face-underline 'flymake-error nil)
  (set-face-underline 'flymake-warning nil)
  (set-face-underline 'flymake-note nil))

;;; ----------------------------------------------------------------------
;;; copilot
;;; ----------------------------------------------------------------------
;; (use-package copilot
;;   :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . copilot-accept-completion)
;;               ("TAB" . copilot-accept-completion)
;;               ("C-TAB" . copilot-accept-completion-by-word)
;;               ("C-<tab>" . copilot-accept-completion-by-word))
;;   :custom
;;   (copilot-idle-delay 0.7)
;;   (copilot-indent-offset-warning-disable t)
;;   (copilot-max-char-warning-disable t))

;;; ----------------------------------------------------------------------
;;; codeium
;;; ----------------------------------------------------------------------
(use-package codeium
  :ensure (:host github :repo "Exafunction/codeium.el")
  :init
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t))

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
;;; shackle
;;; ----------------------------------------------------------------------
(use-package shackle
  :ensure t
  :custom
  (shackle-rules '((compilation-mode :align below :size 0.3)
                   (rspec-compilation-mode :align below :size 0.4 :select t)
                   (calendar-mode :align below :popup t)
                   (epa-key-list-mode :align below :size 0.3)
                   ("*Backtrace*" :align below :size 0.3 :noselect t)
                   ("*Apropos*" :align below :size 0.4 :select t)
                   ("*Warnings*" :align below :size 0.1)
                   ("*Org Select*" :align below :size 0.3)
                   ("^CAPTURE-.*\\.org\\'" :regexp t :select t :popup t :inhibit-window-quit t)
                   (" *Agenda Commands*" :align below :size 0.3)
                   (" *Org todo*" :align below :size 0.3 :popup t)
                   ("*rg*" :align right :size 0.4 :select t :popup t)
                   ("*git-gutter:diff*" :align below :size 0.4)
                   ("\\(Messages\\|Report\\)\\*\\'" :regexp t :align below :size 0.3)
                   ("*Google Translate*" :popup t :select t)))
  :config
  (shackle-mode 1))

;;; ----------------------------------------------------------------------
;;; popper.el
;;; ----------------------------------------------------------------------
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-display-control nil)
  (popper-reference-buffers '(compilation-mode
                              rspec-compilation-mode
                              help-mode
                              epa-key-list-mode
                              "\\*Backtrace\\*"
                              "\\*Apropos\\*"
                              "\\*Warnings\\*"
                              "\\(Messages\\|Report\\)\\*\\'"
                              "\\*rg\\*"
                              "\\*git-gutter:diff\\*"))
  :config
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
;;; google-translate.el
;;; ----------------------------------------------------------------------
(use-package google-translate
  :ensure t
  :defer t
  :custom
  (google-translate-default-source-language "auto")
  (google-translate-default-target-language "ja")
  (google-translate-translation-directions-alist '(("en" . "ja") ("ja" . "en")))
  (google-translate-backend-method 'curl)
  :bind (("\C-c t g" . google-translate-at-point-autodetect)
         ("\C-c t G" . google-translate-smooth-translate))
  :config
  (require 'cl-lib)
  (defun google-translate-at-point-autodetect (&optional override-p)
    "選択リージョンを整形し、内容に応じて翻訳方向を自動判定して翻訳する。"
    (interactive "P")
    (cl-letf* ((orig-fn (symbol-function 'google-translate-translate))
               ((symbol-function 'google-translate-translate)
                (lambda (source-language target-language text &optional output-destination)
                  (when (use-region-p)
                    (setq text (funcall region-extract-function nil))
                    (deactivate-mark)
                    (when (fboundp 'cua-cancel)
                      (cua-cancel)))

                  ;; 整形処理（改行→スペース、コメントマーク除去、空白除去）
                  (let* ((lines (split-string text "\n"))
                         (stripped-lines
                          (cl-remove-if
                           #'string-empty-p
                           (mapcar (lambda (line)
                                     (string-trim
                                      (replace-regexp-in-string
                                       "^[ \t]*\\([#;]+\\|//\\|--\\)[ \t]*" "" line)))
                                   lines)))
                         (str (mapconcat #'identity stripped-lines " ")))

                    (if current-prefix-arg
                        (funcall orig-fn source-language target-language str output-destination)
                      (if (>= (/ (* (length (replace-regexp-in-string "[[:ascii:]]" "" str)) 100)
                                 (length str)) ; ゼロ除算対策
                              20)
                          (funcall orig-fn "ja" "en" str output-destination)
                        (funcall orig-fn "en" "ja" str output-destination)))))))
      (google-translate-at-point override-p))))

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
  (gptel-api-key (my/get-openai-api-key))
  (gptel-model 'gpt-4.1-mini))

;;; ----------------------------------------------------------------------
;;; org-ai
;;; ----------------------------------------------------------------------
(use-package org-ai
  :ensure t
  :after org
  :custom
  (org-ai-default-chat-model "gpt-4.1-mini")
  (org-ai-openai-api-token (my/get-openai-api-key))
  :hook (org-mode . org-ai-mode))

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
;;; eldoc-mode
;;; ----------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;; ----------------------------------------------------------------------
;;; symbol-overlay
;;; ----------------------------------------------------------------------
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("<C-f3>"     . symbol-overlay-put)             ;; 現在のsymbolをハイライト
         ("<f3>"       . symbol-overlay-jump-next)       ;; 次のハイライトへ移動
         ("<S-f3>"     . symbol-overlay-jump-prev)       ;; 前のハイライトへ戻る
         ("ESC <C-f3>" . symbol-overlay-remove-all)      ;; 全ハイライト消す
         ("ESC <f3>"   . symbol-overlay-query-replace))  ;; ハイライト対象を置換
  :custom
  (setq symbol-overlay-idle-time 0.2))

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
;;; tempbuf
;;; ----------------------------------------------------------------------
(use-package tempbuf
  :ensure (:host github :repo "valda/tempbuf")
  :hook ((dired-mode
          custom-mode-hook
          w3-mode-hook
          Man-mode-hook
          view-mode-hook
          compilation-mode-hook
          calendar-mode-hook) . turn-on-tempbuf-mode)
  :custom
  (tempbuf-kill-message nil)
  :init
  (defun my/tempbuf-mode-if-match (pattern)
    (when (and (buffer-name)
               (string-match pattern (buffer-name)))
      (turn-on-tempbuf-mode)))
  (add-hook 'fundamental-mode-hook
            (lambda () (my/tempbuf-mode-if-match "\\*Flycheck error messages\\*")))
  (add-hook 'diff-mode-hook
            (lambda () (my/tempbuf-mode-if-match "\\*git-gutter:diff\\*"))))

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
;;; hide-mode-line
;;; ----------------------------------------------------------------------
(use-package hide-mode-line
  :ensure t
  :hook
  ((imenu-list-minor-mode treemacs-mode) . hide-mode-line-mode))

;;; ----------------------------------------------------------------------
;;; desktop / session
;;; ----------------------------------------------------------------------
(use-package desktop
  :defer t
  :custom
  (desktop-restore-frames nil)
  (desktop-restore-eager 10)
  (desktop-globals-to-save '(desktop-missing-file-warning
                             tags-file-name
                             tags-table-list
                             register-alist)))

(use-package session
  :defer t
  :custom
  (session-save-file-coding-system 'no-conversion)
  (session-globals-max-string 10000000)
  (session-initialize '(de-saveplace session places keys menus))
  (session-globals-include '((kill-ring 1000)
                             (session-file-alist 1000 t)
                             (file-name-history 1000)
                             search-ring regexp-search-ring))
  (session-save-print-spec '(t nil 40000)))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (desktop-save-mode 1)
            (desktop-read)
            (session-initialize)))

;;; ----------------------------------------------------------------------
;;; kill-ring に同じ内容の文字列を複数入れない
;;; ----------------------------------------------------------------------
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

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
