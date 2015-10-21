;;; -*- mode: lisp-interaction; coding: utf-8-unix -*-
(deftheme mytheme
  "Color theme by YAMAGUCHI Seiji, Created 2015-05-01.")

(custom-theme-set-faces
 'mytheme
 '(default ((t (:foreground "white" :background "gray10"))))
 '(cursor ((t (:background "green"))))
 '(fixed-pitch ((t (:family "courier"))))
 '(variable-pitch ((t (:family "helv"))))
 '(escape-glyph ((t (:foreground "cyan"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(region ((t (:background "DeepSkyBlue4"))))
 '(shadow ((t (:foreground "grey70"))))
 '(secondary-selection ((t (:background "darkslateblue"))))
 '(trailing-whitespace ((t (:background "darkred"))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "OrangeRed"))))
 '(font-lock-comment-face ((t (:foreground "OrangeRed"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "Pink"))))
 '(button ((t (:underline (:color foreground-color :style line)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "cyan1"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "violet"))))
 '(fringe ((t (:foreground "SlateGray" :background "gray20"))))
 '(header-line ((t (:box nil :foreground "grey20" :background "grey90"))))
 '(tooltip ((t (:foreground "systeminfotext" :background "systeminfowindow"))))
 '(mode-line ((t (:foreground "black" :background "white"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "black" :background "white"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:weight light :box (:line-width -1 :color "grey40" :style nil) :foreground "grey80" :background "grey30"))))
 '(isearch ((t (:background "blue"))))
 '(isearch-fail ((t (:background "red4"))))
 '(lazy-highlight ((t (:background "paleturquoise4"))))
 '(match ((t (:background "RoyalBlue3"))))
 '(next-error ((t (:background "blue"))))
 '(query-replace ((t (:background "SteelBlue"))))
 '(mmm-cleanup-submode-face ((t (:background "Wheat"))))
 '(mmm-code-submode-face ((t (:background "LightGray"))))
 '(mmm-comment-submode-face ((t (:background "navy"))))
 '(mmm-declaration-submode-face ((t (:background "Aquamarine"))))
 '(mmm-default-submode-face ((t (:background "gray15"))))
 '(mmm-delimiter-face ((t (nil))))
 '(mmm-init-submode-face ((t (:background "Pink"))))
 '(mmm-output-submode-face ((t (:background "Plum"))))
 '(mmm-special-submode-face ((t (:background "MediumSpringGreen"))))
 '(hl-line ((t (:background "gray15"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mytheme)