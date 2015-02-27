;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-
(defun my-color-theme ()
  "Color theme by YAMAGUCHI, Seiji, created 2012-01-27."
  (interactive)
  (color-theme-install
   '(my-color-theme
     ((background-color . "gray10")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "green")
      (foreground-color . "white")
      (mouse-color . "sienna1"))
     ((ac-fuzzy-cursor-color . "red")
      (align-highlight-change-face . highlight)
      (align-highlight-nochange-face . secondary-selection)
      (bm-face . bm-face)
      (bm-persistent-face . bm-persistent-face)
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
      (org-goto-interface . outline)
      (org-remember-interactive-interface . refile)
      (snippet-bound-face . bold)
      (snippet-field-face . highlight)
      (tags-tag-face . default)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "gray10" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "Consolas"))))
     (CUA-global-mark-face ((t (:background "cyan" :foreground "black"))))
     (CUA-rectangle-face ((t (:background "maroon" :foreground "white"))))
     (CUA-rectangle-noselect-face ((t (:background "dimgray" :foreground "white"))))
     (ac-candidate-face ((t (:background "darkgray" :foreground "black" :underline nil))))
     (ac-completion-face ((t (:background "darkblue" :foreground "yellow" :underline t))))
     (ac-gtags-candidate-face ((t (:background "lightgray" :foreground "navy"))))
     (ac-gtags-selection-face ((t (:background "navy" :foreground "white"))))
     (ac-selection-face ((t (:background "steelblue" :foreground "white"))))
     (ac-yasnippet-candidate-face ((t (:background "sandybrown" :foreground "black"))))
     (ac-yasnippet-selection-face ((t (:background "coral3" :foreground "white"))))
     (anything-M-x-key-face ((t (:foreground "orange" :underline t))))
     (anything-apt-installed ((t (:foreground "green"))))
     (anything-bmkext-file ((t (:foreground "Deepskyblue2"))))
     (anything-bmkext-gnus ((t (:foreground "magenta"))))
     (anything-bmkext-info ((t (:foreground "green"))))
     (anything-bmkext-man ((t (:foreground "Orange4"))))
     (anything-bmkext-no--file ((t (:foreground "grey"))))
     (anything-bmkext-w3m ((t (:foreground "yellow"))))
     (anything-bookmarks-su-face ((t (:foreground "red"))))
     (anything-dir-heading ((t (:background "Pink" :foreground "Blue"))))
     (anything-dir-priv ((t (:foreground "LightSkyBlue"))))
     (anything-dired-symlink-face ((t (:foreground "DarkOrange"))))
     (anything-emms-playlist ((t (:foreground "Springgreen4" :underline t))))
     (anything-ffiles-prefix-face ((t (:background "yellow" :foreground "black"))))
     (anything-file-name ((t (:foreground "white"))))
     (anything-gentoo-match-face ((t (:foreground "red"))))
     (anything-grep-file ((t (:foreground "BlueViolet" :underline t))))
     (anything-grep-lineno ((t (:foreground "Darkorange1"))))
     (anything-grep-match ((t (:background "RoyalBlue3"))))
     (anything-header ((t (:background "grey90" :foreground "grey20" :box nil))))
     (anything-isearch-match ((t (:background "Yellow"))))
     (anything-match ((t (:background "RoyalBlue3"))))
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
     (compilation-error ((t (:bold t :foreground "Pink" :weight bold))))
     (compilation-info ((t (:bold t :foreground "Green1" :weight bold))))
     (compilation-line-number ((t (:foreground "LightGoldenrod"))))
     (compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
     (completions-annotations ((t (:italic t :slant italic))))
     (completions-common-part ((t (:background "gray10" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "BDF UM+"))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (css-property ((t (:foreground "LightGoldenrod"))))
     (css-selector ((t (:foreground "LightSkyBlue"))))
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
     (diff-added ((t (:background nil :foreground "green"))))
     (diff-changed ((t (:background nil :foreground "green yellow"))))
     (diff-context ((t (:foreground "grey70"))))
     (diff-file-header ((t (:bold t :background nil :weight extra-bold))))
     (diff-function ((t (:background "grey45"))))
     (diff-header ((t (:foreground "yellow2" :background nil))))
     (diff-hunk-header ((t (:foreground "violet" :background nil :weight extra-bold))))
     (diff-index ((t (:bold t :background "grey60" :weight bold))))
     (diff-indicator-added ((t (:background nil :foreground "green"))))
     (diff-indicator-changed ((t (:background nil :foreground "green yellow"))))
     (diff-indicator-removed ((t (:background nil :foreground "firebrick1"))))
     (diff-nonexistent ((t (:bold t :background "grey60" :weight bold))))
     (diff-refine-change ((t (:foreground nil :background nil :inverse-video t))))
     (diff-removed ((t (:background nil :foreground "firebrick1"))))
     (dired-directory ((t (:foreground "LightSkyBlue"))))
     (dired-flagged ((t (:bold t :foreground "Pink" :weight bold))))
     (dired-header ((t (:foreground "PaleGreen"))))
     (dired-ignored ((t (:foreground "grey70"))))
     (dired-mark ((t (:foreground "Aquamarine"))))
     (dired-marked ((t (:bold t :foreground "Pink" :weight bold))))
     (dired-perm-write ((t (:background "gray10" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "BDF UM+"))))
     (dired-symlink ((t (:foreground "Cyan"))))
     (dired-warning ((t (:bold t :foreground "Pink" :weight bold))))
     (ediff-current-diff-A ((t (:background "pale green" :foreground "firebrick"))))
     (ediff-current-diff-Ancestor ((t (:background "VioletRed" :foreground "Black"))))
     (ediff-current-diff-B ((t (:background "Yellow" :foreground "DarkOrchid"))))
     (ediff-current-diff-C ((t (:background "Pink" :foreground "Navy"))))
     (ediff-even-diff-A ((t (:background "light grey" :foreground "Black"))))
     (ediff-even-diff-Ancestor ((t (:background "Grey" :foreground "White"))))
     (ediff-even-diff-B ((t (:background "Grey" :foreground "White"))))
     (ediff-even-diff-C ((t (:background "light grey" :foreground "Black"))))
     (ediff-fine-diff-A ((t (:background "sky blue" :foreground "Navy"))))
     (ediff-fine-diff-Ancestor ((t (:background "Green" :foreground "Black"))))
     (ediff-fine-diff-B ((t (:background "cyan" :foreground "Black"))))
     (ediff-fine-diff-C ((t (:background "Turquoise" :foreground "Black"))))
     (ediff-odd-diff-A ((t (:background "Grey" :foreground "White"))))
     (ediff-odd-diff-Ancestor ((t (:background "gray40" :foreground "cyan3"))))
     (ediff-odd-diff-B ((t (:background "light grey" :foreground "Black"))))
     (ediff-odd-diff-C ((t (:background "Grey" :foreground "White"))))
     (ee-face-bookmarked-face ((t (:foreground "Aquamarine"))))
     (ee-face-category-face ((t (:foreground "LightSkyBlue"))))
     (ee-face-faded-face ((t (:foreground "grey40"))))
     (ee-face-link-face ((t (:foreground "LightSkyBlue"))))
     (ee-face-marked-face ((t (:foreground "Red"))))
     (ee-face-omitted-face ((t (:foreground "LightSalmon"))))
     (ee-face-visited-link-face ((t (:foreground "DarkMagenta"))))
     (elscreen-tab-background-face ((t (:background "Gray50"))))
     (elscreen-tab-control-face ((t (:background "white" :foreground "black" :underline "Gray50"))))
     (elscreen-tab-current-screen-face ((t (:background "white" :foreground "black"))))
     (elscreen-tab-other-screen-face ((t (:background "Gray85" :foreground "Gray50"))))
     (escape-glyph ((t (:foreground "cyan"))))
     (expander-face ((t (:background "darkblue" :foreground "white"))))
     (ffap ((t (:background "darkolivegreen"))))
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
     (git-commit-branch-face ((t (:foreground "LightSalmon"))))
     (git-commit-comment-action-face ((t (:foreground "LightSalmon"))))
     (git-commit-comment-face ((t (:foreground "chocolate1"))))
     (git-commit-comment-file-face ((t (:foreground "Cyan1"))))
     (git-commit-comment-heading-face ((t (:foreground "PaleGreen"))))
     (git-commit-known-pseudo-header-face ((t (:foreground "PaleGreen"))))
     (git-commit-no-branch-face ((t (:foreground "LightSalmon"))))
     (git-commit-nonempty-second-line-face ((t (:bold t :foreground "Pink" :weight bold))))
     (git-commit-note-address-face ((t (:foreground "Cyan1"))))
     (git-commit-note-brace-face ((t (:foreground "Aquamarine"))))
     (git-commit-note-face ((t (:foreground "LightSalmon"))))
     (git-commit-overlong-summary-face ((t (:bold t :foreground "Pink" :weight bold))))
     (git-commit-pseudo-header-face ((t (:foreground "Cyan1"))))
     (git-commit-summary-face ((t (:bold t :foreground "LightSalmon" :weight bold))))
     (git-commit-text-face ((t (:family "Consolas" :foundry "outline" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "gray10" :stipple nil :height 113))))
     (header-line ((t (:background "grey90" :foreground "grey20" :box nil))))
     (help-argument-name ((t (:italic t :slant italic))))
     (highlight ((t (:background "darkolivegreen"))))
     (ibuffer-deletion-face ((t (:foreground "red"))))
     (ibuffer-marked-face ((t (:foreground "green"))))
     (isearch ((t (:background "blue"))))
     (isearch-fail ((t (:background "red4"))))
     (iswitchb-current-match ((t (:foreground "LightSkyBlue"))))
     (iswitchb-invalid-regexp ((t (:bold t :foreground "Pink" :weight bold))))
     (iswitchb-single-match ((t (:foreground "OrangeRed"))))
     (iswitchb-virtual-matches ((t (:foreground "LightSteelBlue"))))
     (italic ((t (:italic t :slant italic))))
     (jaspace-highlight-eol-face ((t (:foreground "darkcyan"))))
     (jaspace-highlight-jaspace-face ((t (:foreground "pink4"))))
     (jaspace-highlight-tab-face ((t (:foreground "gray20" :strike-through t))))
     (js2-builtin-face ((t (:foreground "LightSteelBlue"))))
     (js2-comment-face ((t (:foreground "OrangeRed"))))
     (js2-constant-face ((t (:foreground "Aquamarine"))))
     (js2-error-face ((t (:background "red4"))))
     (js2-function-name-face ((t (:foreground "LightSkyBlue"))))
     (js2-function-param-face ((t (:foreground "SeaGreen"))))
     (js2-instance-member-face ((t (:foreground "DarkOrchid"))))
     (js2-jsdoc-html-tag-delimiter-face ((t (:foreground "green"))))
     (js2-jsdoc-html-tag-name-face ((t (:foreground "yellow"))))
     (js2-jsdoc-tag-face ((t (:foreground "SlateGray"))))
     (js2-jsdoc-type-face ((t (:foreground "SteelBlue"))))
     (js2-jsdoc-value-face ((t (:foreground "PeachPuff3"))))
     (js2-keyword-face ((t (:foreground "Cyan"))))
     (js2-private-function-call-face ((t (:foreground "goldenrod"))))
     (js2-private-member-face ((t (:foreground "PeachPuff3"))))
     (js2-regexp-face ((t (:foreground "LightSalmon"))))
     (js2-string-face ((t (:foreground "LightSalmon"))))
     (js2-type-face ((t (:foreground "PaleGreen"))))
     (js2-variable-name-face ((t (:foreground "LightGoldenrod"))))
     (js2-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
     (lazy-highlight ((t (:background "paleturquoise4"))))
     (link ((t (:foreground "cyan1" :underline t))))
     (link-visited ((t (:foreground "violet" :underline t))))
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
     (nxml-attribute-colon ((t (nil))))
     (nxml-attribute-local-name ((t (:foreground "LightGoldenrod"))))
     (nxml-attribute-prefix ((t (:foreground "LightSteelBlue"))))
     (nxml-attribute-value ((t (:foreground "LightSalmon"))))
     (nxml-attribute-value-delimiter ((t (:foreground "LightSalmon"))))
     (nxml-cdata-section-CDATA ((t (:foreground "LightSteelBlue"))))
     (nxml-cdata-section-content ((t (nil))))
     (nxml-cdata-section-delimiter ((t (nil))))
     (nxml-char-ref-delimiter ((t (:foreground "Aquamarine"))))
     (nxml-char-ref-number ((t (:foreground "Aquamarine"))))
     (nxml-comment-content ((t (:foreground "OrangeRed"))))
     (nxml-comment-delimiter ((t (:foreground "OrangeRed"))))
     (nxml-delimited-data ((t (:foreground "LightSalmon"))))
     (nxml-delimiter ((t (nil))))
     (nxml-element-colon ((t (nil))))
     (nxml-element-local-name ((t (:foreground "LightSkyBlue"))))
     (nxml-element-prefix ((t (:foreground "LightSteelBlue"))))
     (nxml-entity-ref-delimiter ((t (:foreground "Aquamarine"))))
     (nxml-entity-ref-name ((t (:foreground "Aquamarine"))))
     (nxml-glyph ((t (:background "light grey" :foreground "black" :slant normal :weight normal))))
     (nxml-hash ((t (:foreground "LightSteelBlue"))))
     (nxml-heading ((t (:bold t :weight bold))))
     (nxml-markup-declaration-delimiter ((t (nil))))
     (nxml-name ((t (:foreground "LightSteelBlue"))))
     (nxml-namespace-attribute-colon ((t (nil))))
     (nxml-namespace-attribute-prefix ((t (:foreground "LightGoldenrod"))))
     (nxml-namespace-attribute-value ((t (:foreground "LightSalmon"))))
     (nxml-namespace-attribute-value-delimiter ((t (:foreground "LightSalmon"))))
     (nxml-namespace-attribute-xmlns ((t (:foreground "LightSteelBlue"))))
     (nxml-outline-active-indicator ((t (:stipple nil :background "gray10" :foreground "white" :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "outline" :family "Consolas" :box 1 :height 113))))
     (nxml-outline-ellipsis ((t (:bold t :family "Consolas" :foundry "outline" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "gray10" :stipple nil :weight bold :height 113))))
     (nxml-outline-indicator ((t (:family "Consolas" :foundry "outline" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "gray10" :stipple nil :height 113))))
     (nxml-processing-instruction-content ((t (:foreground "LightSalmon"))))
     (nxml-processing-instruction-delimiter ((t (nil))))
     (nxml-processing-instruction-target ((t (:foreground "Cyan"))))
     (nxml-prolog-keyword ((t (:foreground "Cyan"))))
     (nxml-prolog-literal-content ((t (:foreground "LightSalmon"))))
     (nxml-prolog-literal-delimiter ((t (:foreground "LightSalmon"))))
     (nxml-ref ((t (:foreground "Aquamarine"))))
     (nxml-tag-delimiter ((t (nil))))
     (nxml-tag-slash ((t (nil))))
     (nxml-text ((t (nil))))
     (org-agenda-clocking ((t (:background "darkslateblue"))))
     (org-agenda-column-dateline ((t (:family "Consolas" :weight normal :slant normal :underline nil :strike-through nil :background "grey30" :height 113))))
     (org-agenda-date ((t (:foreground "LightSkyBlue"))))
     (org-agenda-date-today ((t (:italic t :bold t :foreground "LightSkyBlue" :slant italic :weight bold))))
     (org-agenda-date-weekend ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
     (org-agenda-dimmed-todo-face ((t (:foreground "grey50"))))
     (org-agenda-done ((t (:foreground "PaleGreen"))))
     (org-agenda-restriction-lock ((t (:background "skyblue4"))))
     (org-agenda-structure ((t (:foreground "LightSkyBlue"))))
     (org-archived ((t (:foreground "grey70"))))
     (org-block ((t (:foreground "grey70"))))
     (org-checkbox ((t (:bold t :weight bold))))
     (org-checkbox-statistics-done ((t (:bold t :weight bold :foreground "PaleGreen"))))
     (org-checkbox-statistics-todo ((t (:bold t :weight bold :foreground "Pink"))))
     (org-clock-overlay ((t (:background "SkyBlue4"))))
     (org-code ((t (:foreground "grey70"))))
     (org-column ((t (:background "grey30" :strike-through nil :underline nil :slant normal :weight normal :height 113 :family "Consolas"))))
     (org-column-title ((t (:bold t :background "grey30" :underline t :weight bold))))
     (org-date ((t (:foreground "Cyan" :underline t))))
     (org-done ((t (:bold t :foreground "PaleGreen" :weight bold))))
     (org-drawer ((t (:foreground "LightSkyBlue"))))
     (org-ellipsis ((t (:foreground "LightGoldenrod" :underline t))))
     (org-footnote ((t (:foreground "Cyan" :underline t))))
     (org-formula ((t (:foreground "chocolate1"))))
     (org-headline-done ((t (:foreground "LightSalmon"))))
     (org-hide ((t (:foreground "black"))))
     (org-latex-and-export-specials ((t (:foreground "burlywood"))))
     (org-level-1 ((t (:foreground "LightSkyBlue"))))
     (org-level-2 ((t (:foreground "LightGoldenrod"))))
     (org-level-3 ((t (:foreground "Cyan"))))
     (org-level-4 ((t (:foreground "OrangeRed"))))
     (org-level-5 ((t (:foreground "PaleGreen"))))
     (org-level-6 ((t (:foreground "Aquamarine"))))
     (org-level-7 ((t (:foreground "LightSteelBlue"))))
     (org-level-8 ((t (:foreground "LightSalmon"))))
     (org-link ((t (:foreground "Cyan" :underline t))))
     (org-meta-line ((t (:foreground "OrangeRed"))))
     (org-mode-line-clock ((t (:foreground "black" :background "white"))))
     (org-property-value ((t (nil))))
     (org-quote ((t (:foreground "grey70"))))
     (org-scheduled ((t (:foreground "PaleGreen"))))
     (org-scheduled-previously ((t (:foreground "chocolate1"))))
     (org-scheduled-today ((t (:foreground "PaleGreen"))))
     (org-sexp-date ((t (:foreground "Cyan"))))
     (org-special-keyword ((t (:foreground "LightSalmon"))))
     (org-table ((t (:foreground "LightSkyBlue"))))
     (org-tag ((t (:bold t :weight bold))))
     (org-target ((t (:underline t))))
     (org-time-grid ((t (:foreground "LightGoldenrod"))))
     (org-todo ((t (:bold t :foreground "Pink" :weight bold))))
     (org-upcoming-deadline ((t (:foreground "chocolate1"))))
     (org-verbatim ((t (:foreground "grey70"))))
     (org-verse ((t (:foreground "grey70"))))
     (org-warning ((t (:bold t :weight bold :foreground "Pink"))))
     (outline-1 ((t (:foreground "LightSkyBlue"))))
     (outline-2 ((t (:foreground "LightGoldenrod"))))
     (outline-3 ((t (:foreground "Cyan"))))
     (outline-4 ((t (:foreground "OrangeRed"))))
     (outline-5 ((t (:foreground "PaleGreen"))))
     (outline-6 ((t (:foreground "Aquamarine"))))
     (outline-7 ((t (:foreground "LightSteelBlue"))))
     (outline-8 ((t (:foreground "LightSalmon"))))
     (popup-face ((t (:background "lightgray" :foreground "black"))))
     (popup-isearch-match ((t (:background "sky blue"))))
     (popup-menu-face ((t (:background "lightgray" :foreground "black"))))
     (popup-menu-selection-face ((t (:background "steelblue" :foreground "white"))))
     (popup-scroll-bar-background-face ((t (:background "gray"))))
     (popup-scroll-bar-foreground-face ((t (:background "black"))))
     (popup-tip-face ((t (:background "khaki1" :foreground "black"))))
     (primary-selection ((t (:background "blue"))))
     (pulldown-default-face ((t (:background "lightgray" :foreground "black" :underline "darkgray"))))
     (pulldown-default-selection-face ((t (:background "steelblue" :foreground "white"))))
     (query-replace ((t (:background "blue"))))
     (region ((t (:background "blue"))))
     (rng-error ((t (:bold t :weight bold :foreground "Pink"))))
     (scroll-bar ((t (nil))))
     (search-buffers-face ((t (:bold t :background "SkyBlue" :foreground "Black" :weight bold))))
     (search-buffers-header-face ((t (:bold t :background "gray20" :foreground "azure3" :weight bold))))
     (secondary-selection ((t (:background "darkslateblue"))))
     (sgml-namespace ((t (:foreground "LightSteelBlue"))))
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
     (whitespace-trailing ((t (:background "darkred" :foreground "yellow"))))
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
     (yaml-tab-face ((t (:bold t :background "red" :foreground "red" :weight bold))))
     (zmacs-region ((t (:background "blue")))))))
