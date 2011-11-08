;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-
(defun my-color-theme ()
  "Color theme by YAMAGUCHI, Seiji, created 2010-02-24."
  (interactive)
  (color-theme-install
   '(my-color-theme
     (;(background-color . "rgb:00/40/40")
      (background-color . "gray10")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "green")
      (foreground-color . "white")
      (mouse-color . "sienna1"))
     ((Man-overstrike-face . bold)
      (Man-reverse-face . highlight)
      (Man-underline-face . underline)
      (align-highlight-change-face . highlight)
      (align-highlight-nochange-face . secondary-selection)
      (bm-face . bm-face)
      (bm-persistent-face . bm-persistent-face)
      (browse-kill-ring-separator-face . browse-kill-ring-separator-face)
      (compilation-message-face . underline)
      (dabbrev-highlight-face . highlight)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face)
      (ispell-highlight-face . isearch)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . bold)
      (migemo-dabbrev-ol-face . highlight)
      (migemo-message-prefix-face . highlight)
      (moccur-maximum-displayed-with-color . 500)
      (snippet-bound-face . bold)
      (snippet-field-face . highlight)
      (tags-tag-face . default)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "gray10" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "BDF UM+"))))
     (CUA-global-mark-face ((t (:background "cyan" :foreground "black"))))
     (CUA-rectangle-face ((t (:background "maroon" :foreground "white"))))
     (CUA-rectangle-noselect-face ((t (:background "dimgray" :foreground "white"))))
     (ac-candidate-face ((t (:background "darkgray" :foreground "black" :underline nil))))
     (ac-completion-face ((t (:background "darkblue" :foreground "yellow" :underline t))))
     (ac-selection-face ((t (:background "steelblue" :foreground "white"))))
     (anything-bookmarks-su-face ((t (:foreground "red"))))
     (anything-dir-heading ((t (:background "Pink" :foreground "Blue"))))
     (anything-dir-priv ((t (:foreground "LightSkyBlue"))))
     (anything-file-name ((t (:foreground "white"))))
     (anything-gentoo-match-face ((t (:foreground "red"))))
     (anything-header ((t (:box nil :foreground "grey20" :background "grey90"))))
     (anything-isearch-match ((t (:background "Yellow"))))
     (anything-overlay-line-face ((t (:background "IndianRed4" :underline t))))
     (anything-visible-mark ((t (:background "green1" :foreground "black"))))
     (anything-w3m-bookmarks-face ((t (:foreground "cyan1" :underline t))))
     (bm-face ((t (:background "DarkOrange1" :foreground "Black"))))
     (bm-persistent-face ((t (:background "DarkBlue" :foreground "White"))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (bookmark-menu-heading ((t (:foreground "PaleGreen"))))
     (border ((t (:background "black"))))
     (browse-kill-ring-separator-face ((t (:bold t :foreground "light steel blue" :weight bold))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (change-log-acknowledgement ((t (:foreground "OrangeRed"))))
     (change-log-conditionals ((t (:foreground "LightGoldenrod"))))
     (change-log-date ((t (:foreground "LightSalmon"))))
     (change-log-email ((t (:foreground "LightGoldenrod"))))
     (change-log-file ((t (:foreground "LightSkyBlue"))))
     (change-log-function ((t (:foreground "LightGoldenrod"))))
     (change-log-list ((t (:foreground "Cyan"))))
     (change-log-name ((t (:foreground "Aquamarine"))))
     (clearcase-dired-checkedout-face ((t (:foreground "red"))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (compilation-column-number ((t (:foreground "PaleGreen"))))
     (compilation-error ((t (:bold t :weight bold :foreground "Pink"))))
     (compilation-info ((t (:bold t :foreground "Green1" :weight bold))))
     (compilation-line-number ((t (:foreground "LightGoldenrod"))))
     (compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
     (completions-common-part ((t (:family "BDF UM+" :foundry "outline" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "gray10" :stipple nil :height 90))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (cursor ((t (:background "green"))))
     (custom-button-face ((t (nil))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:foreground "light blue" :underline t))))
     (custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "blue"))))
     (custom-state-face ((t (:foreground "lime green"))))
     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag-face ((t (:foreground "light blue" :underline t))))
     (diff-added ((t (nil))))
     (diff-changed ((t (nil))))
     (diff-context ((t (:foreground "grey70"))))
     (diff-file-header ((t (:bold t :background "grey60" :weight bold))))
     (diff-function ((t (:background "grey45"))))
     (diff-header ((t (:background "grey45"))))
     (diff-hunk-header ((t (:background "grey45"))))
     (diff-index ((t (:bold t :weight bold :background "grey60"))))
     (diff-indicator-added ((t (nil))))
     (diff-indicator-changed ((t (nil))))
     (diff-indicator-removed ((t (nil))))
     (diff-nonexistent ((t (:bold t :weight bold :background "grey60"))))
     (diff-refine-change ((t (:background "grey60"))))
     (diff-removed ((t (nil))))
     (dired-directory ((t (:foreground "LightSkyBlue"))))
     (dired-flagged ((t (:bold t :weight bold :foreground "Pink"))))
     (dired-header ((t (:foreground "PaleGreen"))))
     (dired-ignored ((t (:foreground "grey70"))))
     (dired-mark ((t (:foreground "Aquamarine"))))
     (dired-marked ((t (:bold t :weight bold :foreground "Pink"))))
     (dired-perm-write ((t (:family "BDF UM+" :foundry "outline" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "gray10" :stipple nil :height 90))))
     (dired-symlink ((t (:foreground "Cyan"))))
     (dired-warning ((t (:bold t :weight bold :foreground "Pink"))))
     (ee-face-bookmarked-face ((t (:foreground "Aquamarine"))))
     (ee-face-category-face ((t (:foreground "LightSkyBlue"))))
     (ee-face-faded-face ((t (:foreground "grey40"))))
     (ee-face-link-face ((t (:foreground "LightSkyBlue"))))
     (ee-face-marked-face ((t (:foreground "Red"))))
     (ee-face-omitted-face ((t (:foreground "LightSalmon"))))
     (ee-face-visited-link-face ((t (:foreground "DarkMagenta"))))
     (elscreen-tab-background-face ((t (:background "Gray50"))))
     (elscreen-tab-current-screen-face ((t (:background "white" :foreground "black"))))
     (elscreen-tab-other-screen-face ((t (:background "Gray85" :foreground "Gray50"))))
     (escape-glyph ((t (:foreground "cyan"))))
     (expander-face ((t (:background "darkblue" :foreground "white"))))
     (file-name-shadow ((t (:foreground "grey70"))))
     (fixed-pitch ((t (:family "courier"))))
     (fl-comment-face ((t (:foreground "pink"))))
     (fl-doc-string-face ((t (:foreground "purple"))))
     (fl-function-name-face ((t (:foreground "red"))))
     (fl-keyword-face ((t (:foreground "cyan"))))
     (fl-string-face ((t (:foreground "green"))))
     (fl-type-face ((t (:foreground "yellow"))))
     (flash-paren-face-off ((t (nil))))
     (flash-paren-face-on ((t (nil))))
     (flash-paren-face-region ((t (nil))))
     (flymake-errline ((t (:background "red4"))))
     (flymake-warnline ((t (:background "dark slate blue"))))
     (flyspell-duplicate ((t (:bold t :foreground "Gold3" :underline t :weight bold))))
     (flyspell-incorrect ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-comment-delimiter-face ((t (:foreground "OrangeRed"))))
     (font-lock-comment-face ((t (:foreground "OrangeRed"))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-doc-face ((t (:foreground "LightSalmon"))))
     (font-lock-doc-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
     (font-lock-keyword-face ((t (:foreground "Cyan"))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-type-face ((t (:foreground "PaleGreen"))))
     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
     (fringe ((t (:background "gray20" :foreground "SlateGray"))))
     (header-line ((t (:background "grey90" :foreground "grey20" :box nil))))
     (help-argument-name ((t (:italic t :slant italic))))
     (highlight ((t (:background "darkolivegreen"))))
     (ibuffer-deletion-face ((t (:foreground "red"))))
     (ibuffer-marked-face ((t (:foreground "green"))))
     (isearch ((t (:background "blue"))))
     (isearch-fail ((t (:background "red4"))))
     (iswitchb-current-match ((t (:foreground "LightSkyBlue"))))
     (iswitchb-invalid-regexp ((t (:bold t :weight bold :foreground "Pink"))))
     (iswitchb-single-match ((t (:foreground "OrangeRed"))))
     (iswitchb-virtual-matches ((t (:foreground "LightSteelBlue"))))
     (italic ((t (:italic t :slant italic))))
     (jaspace-highlight-eol-face ((t (:foreground "darkcyan"))))
     (jaspace-highlight-jaspace-face ((t (:foreground "pink4"))))
     (jaspace-highlight-tab-face ((t (:foreground "gray20" :strike-through t))))
     (lazy-highlight ((t (:background "paleturquoise4"))))
     (link ((t (:foreground "cyan1" :underline t))))
     (link-visited ((t (:underline t :foreground "violet"))))
     (makefile-space-face ((t (:background "hotpink"))))
     (match ((t (:background "RoyalBlue3"))))
     (menu ((t (nil))))
     (message-cited-text-face ((t (:bold t :foreground "red" :weight bold))))
     (message-header-cc-face ((t (:bold t :foreground "green4" :weight bold))))
     (message-header-name-face ((t (:bold t :foreground "orange" :weight bold))))
     (message-header-newsgroups-face ((t (:bold t :foreground "violet" :weight bold))))
     (message-header-other-face ((t (:bold t :foreground "chocolate" :weight bold))))
     (message-header-subject-face ((t (:bold t :foreground "yellow" :weight bold))))
     (message-header-to-face ((t (:bold t :foreground "cyan" :weight bold))))
     (message-header-xheader-face ((t (:bold t :foreground "light blue" :weight bold))))
     (message-mml-face ((t (:bold t :background "Green3" :weight bold))))
     (message-separator-face ((t (:foreground "blue3"))))
     (minibuf-isearch-comp-face ((t (:background "navy" :underline t))))
     (minibuf-isearch-face ((t (:bold t :background "blue" :underline t :weight bold))))
     (minibuffer-prompt ((t (:foreground "cyan"))))
     (mmm-cleanup-submode-face ((t (:background "Wheat"))))
     (mmm-code-submode-face ((t (:background "LightGray"))))
     (mmm-comment-submode-face ((t (:background "navy"))))
     (mmm-declaration-submode-face ((t (:background "Aquamarine"))))
     (mmm-default-submode-face ((t (:background "gray15"))))
     (mmm-delimiter-face ((t (nil))))
     (mmm-init-submode-face ((t (:background "Pink"))))
     (mmm-output-submode-face ((t (:background "Plum"))))
     (mmm-special-submode-face ((t (:background "MediumSpringGreen"))))
     (moccur-current-line-face ((t (:underline t))))
     (moccur-edit-done-face ((t (:bold t :foreground "gray30" :weight bold))))
     (moccur-edit-face ((t (:bold t :background "Pink" :foreground "Black" :weight bold))))
     (moccur-edit-file-face ((t (:bold t :background "gray30" :weight bold))))
     (moccur-edit-reject-face ((t (:bold t :foreground "hot pink" :weight bold))))
     (moccur-face ((t (:bold t :background "light grey" :foreground "Black" :weight bold))))
     (mode-line ((t (:background "white" :foreground "black"))))
     (mode-line-buffer-id ((t (:bold t :background "white" :foreground "black" :weight bold))))
     (mode-line-emphasis ((t (:bold t :weight bold))))
     (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
     (mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
     (modeline-mousable ((t (:background "white" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "white" :foreground "black"))))
     (mouse ((t (:background "sienna1"))))
     (navi2ch-article-auto-decode-face ((t (:foreground "gray10"))))
     (navi2ch-article-citation-face ((t (:foreground "HotPink1"))))
     (navi2ch-article-face ((t (nil))))
     (navi2ch-article-header-contents-face ((t (:foreground "yellow"))))
     (navi2ch-article-header-face ((t (:bold t :foreground "gray" :weight bold))))
     (navi2ch-article-header-fusianasan-face ((t (:foreground "yellow" :underline t))))
     (navi2ch-article-link-face ((t (:bold t :weight bold))))
     (navi2ch-article-url-face ((t (:bold t :weight bold))))
     (navi2ch-bm-cache-face ((t (:foreground "SkyBlue"))))
     (navi2ch-bm-mark-face ((t (:foreground "tomato"))))
     (navi2ch-bm-new-cache-face ((t (:bold t :foreground "SkyBlue" :weight bold))))
     (navi2ch-bm-new-mark-face ((t (:bold t :foreground "tomato" :weight bold))))
     (navi2ch-bm-new-unread-face ((t (:bold t :foreground "GreenYellow" :weight bold))))
     (navi2ch-bm-new-update-face ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
     (navi2ch-bm-new-view-face ((t (:bold t :foreground "PaleGreen" :weight bold))))
     (navi2ch-bm-seen-cache-face ((t (:foreground "SkyBlue" :underline t))))
     (navi2ch-bm-seen-mark-face ((t (:foreground "tomato" :underline t))))
     (navi2ch-bm-seen-unread-face ((t (:foreground "GreenYellow" :underline t))))
     (navi2ch-bm-seen-update-face ((t (:foreground "LightSkyBlue" :underline t))))
     (navi2ch-bm-seen-view-face ((t (:foreground "PaleGreen" :underline t))))
     (navi2ch-bm-unread-face ((t (:foreground "GreenYellow"))))
     (navi2ch-bm-update-face ((t (:foreground "LightSkyBlue"))))
     (navi2ch-bm-updated-cache-face ((t (:bold t :foreground "SkyBlue" :weight bold))))
     (navi2ch-bm-updated-mark-face ((t (:bold t :foreground "tomato" :weight bold))))
     (navi2ch-bm-updated-unread-face ((t (:bold t :foreground "GreenYellow" :weight bold))))
     (navi2ch-bm-updated-update-face ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
     (navi2ch-bm-updated-view-face ((t (:bold t :foreground "PaleGreen" :weight bold))))
     (navi2ch-bm-view-face ((t (:foreground "cyan"))))
     (navi2ch-list-add-board-name-face ((t (:bold t :foreground "cyan" :weight bold))))
     (navi2ch-list-board-name-face ((t (:foreground "SkyBlue"))))
     (navi2ch-list-category-face ((t (:bold t :foreground "SkyBlue" :weight bold))))
     (navi2ch-list-change-board-name-face ((t (:bold t :foreground "GreenYellow" :weight bold))))
     (navi2ch-mona-face ((t (nil))))
     (navi2ch-mona12-face ((t (nil))))
     (navi2ch-mona14-face ((t (nil))))
     (navi2ch-mona16-face ((t (nil))))
     (navi2ch-splash-screen-face ((t (:foreground "SkyBlue"))))
     (next-error ((t (:background "blue"))))
     (nobreak-space ((t (:foreground "cyan" :underline t))))
     (primary-selection ((t (:background "blue"))))
     (pulldown-default-face ((t (:background "lightgray" :foreground "black" :underline "darkgray"))))
     (pulldown-default-selection-face ((t (:background "steelblue" :foreground "white"))))
     (query-replace ((t (:background "blue"))))
     (region ((t (:background "blue"))))
     (scroll-bar ((t (nil))))
     (search-buffers-face ((t (:bold t :background "SkyBlue" :foreground "Black" :weight bold))))
     (search-buffers-header-face ((t (:bold t :background "gray20" :foreground "azure3" :weight bold))))
     (secondary-selection ((t (:background "darkslateblue"))))
     (shadow ((t (:foreground "grey70"))))
     (show-block-face1 ((t (:background "gray10"))))
     (show-block-face2 ((t (:background "gray15"))))
     (show-block-face3 ((t (:background "gray20"))))
     (show-block-face4 ((t (:background "gray25"))))
     (show-block-face5 ((t (:background "gray30"))))
     (show-block-face6 ((t (:background "gray35"))))
     (show-block-face7 ((t (:background "gray40"))))
     (show-block-face8 ((t (:background "gray45"))))
     (show-block-face9 ((t (:background "gray50"))))
     (show-paren-match ((t (:background "turquoise"))))
     (show-paren-mismatch ((t (:background "purple" :foreground "white"))))
     (speedbar-button-face ((t (:foreground "green3"))))
     (speedbar-directory-face ((t (:foreground "light blue"))))
     (speedbar-file-face ((t (:foreground "cyan"))))
     (speedbar-highlight-face ((t (:background "sea green"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
     (speedbar-tag-face ((t (:foreground "yellow"))))
     (svn-log-partner-highlight-face ((t (:bold t :weight bold))))
     (svn-status-blame-highlight-face ((t (:background "darkolivegreen"))))
     (svn-status-blame-rev-number-face ((t (:foreground "LightGoldenrod"))))
     (svn-status-directory-face ((t (:foreground "lightskyblue1"))))
     (svn-status-filename-face ((t (:foreground "beige"))))
     (svn-status-locked-face ((t (:bold t :foreground "Red" :weight bold))))
     (svn-status-marked-face ((t (:foreground "palegreen2"))))
     (svn-status-marked-popup-face ((t (:foreground "palegreen2"))))
     (svn-status-switched-face ((t (:foreground "Aquamarine"))))
     (svn-status-symlink-face ((t (:foreground "cyan"))))
     (svn-status-update-available-face ((t (:foreground "yellow"))))
     (tool-bar ((t (:background "systembuttonface" :foreground "systembuttontext" :box (:line-width 1 :style released-button)))))
     (tooltip ((t (:background "systeminfowindow" :foreground "systeminfotext"))))
     (trailing-whitespace ((t (:background "darkred"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "helv"))))
     (vertical-border ((t (nil))))
     (whitespace-empty ((t (:background "yellow" :foreground "firebrick"))))
     (whitespace-hspace ((t (:background "grey24" :foreground "aquamarine3"))))
     (whitespace-indentation ((t (:background "yellow" :foreground "firebrick"))))
     (whitespace-line ((t (:background "gray20" :foreground "violet"))))
     (whitespace-newline ((t (:foreground "darkgray" :weight normal))))
     (whitespace-space ((t (:background "grey20" :foreground "aquamarine3"))))
     (whitespace-space-after-tab ((t (:background "yellow" :foreground "firebrick"))))
     (whitespace-space-before-tab ((t (:background "DarkOrange" :foreground "firebrick"))))
     (whitespace-tab ((t (:background "grey22" :foreground "aquamarine3"))))
     (whitespace-trailing ((t (:bold t :background "red1" :foreground "yellow" :weight bold))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:foreground "red"))))
     (widget-documentation ((t (:foreground "lime green"))))
     (widget-field ((t (:background "dim gray"))))
     (widget-inactive ((t (:foreground "light gray"))))
     (widget-single-line-field ((t (:background "dim gray"))))
     (woman-addition ((t (:foreground "orange"))))
     (woman-bold ((t (:bold t :foreground "green2" :weight bold))))
     (woman-italic ((t (:italic t :underline t :slant italic))))
     (woman-symbol ((t (nil))))
     (woman-unknown ((t (:foreground "cyan1"))))
     (zmacs-region ((t (:background "blue")))))))
