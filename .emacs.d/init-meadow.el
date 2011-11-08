;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-

;;; 規定の文字コードを UTF-8 に
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(set-w32-system-coding-system 'japanese-shift-jis-dos)
(set-clipboard-coding-system 'japanese-shift-jis-dos)
(setq default-keyboard-coding-system 'japanese-shift-jis-dos)
(setq default-terminal-coding-system 'japanese-shift-jis-dos)
(setq default-file-name-coding-system 'japanese-shift-jis)
(setq default-process-coding-system '(japanese-shift-jis-dos . japanese-shift-jis-dos))
(prefer-coding-system 'japanese-shift-jis)

;;; IMEの設定
(mw32-ime-initialize)
(setq default-input-method "MW32-IME")

;; IME インジケータの設定
(setq-default mw32-ime-mode-line-state-indicator "[--]")
(setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(add-hook 'mw32-ime-on-hook
          (function (lambda () (set-cursor-height 2))))
(add-hook 'mw32-ime-off-hook
          (function (lambda () (set-cursor-height 4))))

;; IMEのモードごとにカーソル色を変える
(add-hook 'mw32-ime-on-hook
          (function (lambda () (set-cursor-color "red"))))
(add-hook 'mw32-ime-off-hook
          (function (lambda () (set-cursor-color "green"))))

;;; マウスカーソルを消す設定
(setq w32-hide-mouse-on-key t)
(setq w32-hide-mouse-timeout 5000)

;;; shell の設定

;;; Cygwin の bash を使う場合
(setq explicit-shell-file-name "bash.exe")
(setq shell-file-name "sh.exe")
(setq shell-command-switch "-c")

;;; Virtually UN*X!にある tcsh.exe を使う場合
;; (setq explicit-shell-file-name "tcsh.exe")
;; (setq shell-file-name "tcsh.exe")
;; (setq shell-command-switch "-c")

;;; WindowsNT に付属の CMD.EXE を使う場合。
;; (setq explicit-shell-file-name "CMD.EXE")
;; (setq shell-file-name "CMD.EXE")
;; (setq shell-command-switch "\\/c")


;;; argument-editing の設定
(require 'mw32script)
(mw32script-init)


;;; 印刷の設定
;; この設定で M-x print-buffer RET などでの印刷ができるようになります
;;
;;  notepad に与えるパラメータの形式の設定
(define-process-argument-editing "notepad"
  (lambda (x) (general-process-argument-editing-function x nil t)))

(defun w32-print-region (start end
                               &optional lpr-prog delete-text buf display
                               &rest rest)
(interactive)
(let ((tmpfile (expand-file-name (make-temp-name "w32-print-")
                                   temporary-file-directory))
        (coding-system-for-write w32-system-coding-system))
  (write-region start end tmpfile nil 'nomsg)
  (call-process "notepad" nil nil nil "/p" tmpfile)
  (and (file-writable-p tmpfile) (delete-file tmpfile))))

(setq print-region-function 'w32-print-region)

;;; フォントの設定
(w32-add-font "tt-font-9px"
              '((strict-spec
                 ((:char-spec ascii :height any)
                  (w32-logfont "ＭＳ ゴシック" 0 -12 400 0 nil nil nil 0 1 3 49))
                 ((:char-spec ascii :height any :weight bold)
                  (w32-logfont "ＭＳ ゴシック" 0 -12 700 0 nil nil nil 0 1 3 49) ((spacing . -1)))
                 ((:char-spec ascii :height any :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -12 400 0 nil nil nil 0 1 3 17))
                 ((:char-spec ascii :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -12 700 0 nil nil nil 0 1 3 17) ((spacing . -1)))
                 ((:char-spec japanese-jisx0208 :height any)
                  (w32-logfont "ＭＳ ゴシック" 0 -12 400 0 nil nil nil 128 1 3 49))
                 ((:char-spec japanese-jisx0208 :height any :weight bold)
                  (w32-logfont "ＭＳ ゴシック" 0 -12 700 0 nil nil nil 128 1 3 49) ((spacing . -1)))
                 ((:char-spec japanese-jisx0208 :height any :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -12 400 0 nil nil nil 128 1 3 17))
                 ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -12 700 0 nil nil nil 128 1 3 17) ((spacing . -1)))
                 ((:char-spec katakana-jisx0201 :height any)
                  (w32-logfont "ＭＳ ゴシック" 0 -12 400 0 nil nil nil 128 1 3 49))
                 ((:char-spec katakana-jisx0201 :height any :weight bold)
                  (w32-logfont "ＭＳ ゴシック" 0 -12 700 0 nil nil nil 128 1 3 49) ((spacing . -1)))
                 ((:char-spec katakana-jisx0201 :height any :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -12 400 0 nil nil nil 128 1 3 17))
                 ((:char-spec katakana-jisx0201 :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -12 700 0 nil nil nil 128 1 3 17) ((spacing . -1))))))

(w32-add-font "tt-font-12px"
              '((strict-spec
                 ((:char-spec ascii :height any)
                  (w32-logfont "ＭＳ ゴシック" 0 -16 400 0 nil nil nil 0 1 3 49))
                 ((:char-spec ascii :height any :weight bold)
                  (w32-logfont "ＭＳ ゴシック" 0 -16 700 0 nil nil nil 0 1 3 49) ((spacing . -1)))
                 ((:char-spec ascii :height any :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -16 400 0 nil nil nil 0 1 3 17))
                 ((:char-spec ascii :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -16 700 0 nil nil nil 0 1 3 17) ((spacing . -1)))
                 ((:char-spec japanese-jisx0208 :height any)
                  (w32-logfont "ＭＳ ゴシック" 0 -16 400 0 nil nil nil 128 1 3 49))
                 ((:char-spec japanese-jisx0208 :height any :weight bold)
                  (w32-logfont "ＭＳ ゴシック" 0 -16 700 0 nil nil nil 128 1 3 49) ((spacing . -1)))
                 ((:char-spec japanese-jisx0208 :height any :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -16 400 0 nil nil nil 128 1 3 17))
                 ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -16 700 0 nil nil nil 128 1 3 17) ((spacing . -1)))
                 ((:char-spec katakana-jisx0201 :height any)
                  (w32-logfont "ＭＳ ゴシック" 0 -16 400 0 nil nil nil 128 1 3 49))
                 ((:char-spec katakana-jisx0201 :height any :weight bold)
                  (w32-logfont "ＭＳ ゴシック" 0 -16 700 0 nil nil nil 128 1 3 49) ((spacing . -1)))
                 ((:char-spec katakana-jisx0201 :height any :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -16 400 0 nil nil nil 128 1 3 17))
                 ((:char-spec katakana-jisx0201 :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ 明朝"     0 -16 700 0 nil nil nil 128 1 3 17) ((spacing . -1))))))

(w32-add-font "tt-font-pgothic"
              '((strict-spec
                 ((:char-spec ascii :height any)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 400 0 nil nil nil 0 1 0 49))
                 ((:char-spec ascii :height any :slant italic)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 400 0   t nil nil 0 1 0 49))
                 ((:char-spec ascii :height any :weight bold)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 700 0 nil nil nil 0 1 0 49) ((spacing . -1)))
                 ((:char-spec ascii :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 700 0   t nil nil 0 1 0 49) ((spacing . -1)))
                 ((:char-spec japanese-jisx0208 :height any)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 400 0 nil nil nil 128 1 0 49))
                 ((:char-spec japanese-jisx0208 :height any :slant italic)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 400 0   t nil nil 128 1 0 49))
                 ((:char-spec japanese-jisx0208 :height any :weight bold)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 700 0 nil nil nil 128 1 0 49) ((spacing . -1)))
                 ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 700 0   t nil nil 128 1 0 49) ((spacing . -1)))
                 ((:char-spec katakana-jisx0201 :height any)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 400 0 nil nil nil 128 1 0 49))
                 ((:char-spec katakana-jisx0201 :height any :slant italic)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 400 0   t nil nil 128 1 0 49))
                 ((:char-spec katakana-jisx0201 :height any :weight bold)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 700 0 nil nil nil 128 1 0 49) ((spacing . -1)))
                 ((:char-spec katakana-jisx0201 :height any :weight bold :slant italic)
                  (w32-logfont "ＭＳ Ｐゴシック" 0 16 700 0   t nil nil 128 1 0 49) ((spacing . -1))))))

(w32-add-font "BDF M+"
              '((strict-spec
                 ((:char-spec ascii :height any :slant any)
                  (bdf-font "c:/meadow/bdf/mplus/mplus_f12r.bdf"))
                 ((:char-spec ascii :height any :weight bold :slant any)
                  (bdf-font "c:/meadow/bdf/mplus/mplus_f12b.bdf"))
                 ((:char-spec japanese-jisx0208 :height any :slant any)
                  (bdf-font "c:/meadow/bdf/mplus/mplus_j12r.bdf"))
                 ((:char-spec japanese-jisx0208 :height any :weight bold :slant any)
                  (bdf-font "c:/meadow/bdf/mplus/mplus_j12b.bdf")))))

(set-face-attribute 'variable-pitch nil :family "Courier New" :height 0.9)

;; 初期フレーム設定（フォント）
(setq default-frame-alist
      (append (list
               '(ime-font . (w32-logfont "ＭＳ ゴシック" 0 -12 400 0 nil nil nil 128 1 3 17))
               '(font . "BDF M+"))
              default-frame-alist))

;;; coding-system の設定
;(modify-coding-system-alist 'process ".*sh\\.exe" 'undecided-dos)
(add-hook 'shell-mode-hook
          (lambda ()
           (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))

;;; ^M をとる
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;;; shell-modeでの補完 (for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")
