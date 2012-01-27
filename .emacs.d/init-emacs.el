;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-

;; 規定の文字コードを UTF-8 に
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(if (not window-system)
    (set-terminal-coding-system 'utf-8-unix))
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(if (eq window-system 'w32)
    (setq default-file-name-coding-system 'japanese-shift-jis-dos))

;; フォントの設定
(cond ((eq window-system 'x)
       ;; (create-fontset-from-fontset-spec
       ;;  "-mplus-fixed-*-*-*--10-*-*-*-*-*-fontset-10,
       ;;   ascii:-mplus-gothic-*--10-*-iso8859-1,
       ;;   japanese-jisx0208:-mplus-gothic-*--10-*-jisx0208.1983-0,
       ;;   katakana-jisx0201:-mplus-gothic-*--10-*-jisx0201.1976-0")
       ;; (create-fontset-from-fontset-spec
       ;;  "-mplus-fixed-*-*-*--12-*-*-*-*-*-fontset-12,
       ;;   ascii:-mplus-gothic-*--12-*-iso8859-1,
       ;;   japanese-jisx0208:-mplus-gothic-*--12-*-jisx0208.1983-0,
       ;;   katakana-jisx0201:-mplus-gothic-*--12-*-jisx0201.1976-0")
       ;; (create-fontset-from-fontset-spec
       ;;  "-misc-fixed-*-*-*--14-*-*-*-*-*-fontset-14,
       ;;   ascii:-misc-fixed-*--14-*-iso8859-1,
       ;;   japanese-jisx0208:-misc-fixed-*--14-*-jisx0208.1983-0,
       ;;   katakana-jisx0201:-misc-fixed-*--14-*-jisx0201.1976-0")
       ;; (create-fontset-from-fontset-spec
       ;;  "-misc-fixed-*-*-*--16-*-*-*-*-*-fontset-16,
       ;;   ascii:-*-fixed-*--16-*-iso8859-1,
       ;;   japanese-jisx0208:-*-fixed-*--16-*-jisx0208.1983-0,
       ;;   katakana-jisx0201:-*-fixed-*--16-*-jisx0201.1976-0")
       ;; (create-fontset-from-fontset-spec
       ;;  "-misc-fixed-*-*-*--24-*-*-*-*-*-fontset-24,
       ;;   ascii:-*-fixed-*--24-*-iso8859-1,
       ;;   japanese-jisx0208:-*-fixed-*--24-*-jisx0208.1983-0,
       ;;   katakana-jisx0201:-*-fixed-*--24-*-jisx0201.1976-0")
       ;; (set-default-font "fontset-12"))
       (add-to-list 'initial-frame-alist '(font . "Ricty-12")))
      ((eq window-system 'w32)
       ;; (add-to-list 'initial-frame-alist '(font . "BDF UM+-9"))
       ;; (add-to-list 'initial-frame-alist '(font . "Ricty-12"))
       (create-fontset-from-ascii-font "Consolas-11:weight=normal:slant=normal" nil "consolasmeiryo")
       (set-fontset-font "fontset-consolasmeiryo" 'japanese-jisx0208 '("MeiryoKe_Console" . "jisx0208-sjis"))
       (set-fontset-font "fontset-consolasmeiryo" 'katakana-jisx0201 '("MeiryoKe_Console" . "jisx0201-katakana"))
       (add-to-list 'initial-frame-alist '(font . "fontset-consolasmeiryo"))
       ))
(setq default-frame-alist initial-frame-alist)
