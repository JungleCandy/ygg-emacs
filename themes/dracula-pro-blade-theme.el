;;; dracula-pro-dracula-pro-blade-theme.el --- Dracula Pro

;; Copyright (C) 2020-Today Dracula Theme.

;; Author: Dracula Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://draculatheme.com/pro

;;; Commentary:
;; Dracula PRO is a color scheme and UI theme tailored for programming. Made for terminal emulators, code editors, and syntax highlighters.

;;; Code:

(require 'cl-lib)
(deftheme dracula-pro-blade
  "Dracula PRO - Blade Variant")


;;;; Configuration options:

(defgroup dracula-pro-blade nil
  "Dracula theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dracula-pro-blade-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'dracula-pro-blade)

(defcustom dracula-pro-blade-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'dracula-pro-blade)

(defcustom dracula-pro-blade-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'dracula-pro-blade)

(defcustom dracula-pro-blade-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'dracula-pro-blade)

(defcustom dracula-pro-blade-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'dracula-pro-blade)

(defcustom dracula-pro-blade-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula-pro-blade)


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [TTY-COLOR]
(let ((colors '(;; Upstream theme color
                (dracula-pro-blade-bg       "#212C2A" "#1F2E2B" nil)             ; Background
                (dracula-pro-blade-fg       "#F8F8F2" "#F9F9F1" "brightwhite")   ; Foreground
                (dracula-pro-blade-current  "#415854" "#3D5C56" "brightblack")   ; Current-line/selection
                (dracula-pro-blade-comment  "#70A99F" "#6AAFA2" "blue")          ; Comment
                (dracula-pro-blade-cyan     "#80FFEA" "#86F9E6" "brightcyan")    ; Cyan
                (dracula-pro-blade-green    "#8AFF80" "#8FF986" "green")         ; Green
                (dracula-pro-blade-orange   "#FFCA80" "#F9C986" "brightred")     ; Orange
                (dracula-pro-blade-pink     "#FF80BF" "#F986BF" "magenta")       ; Pink
                (dracula-pro-blade-purple   "#9580FF" "#9986F9" "brightmagenta") ; Purple
                (dracula-pro-blade-red      "#FF9580" "#F99986" "red")           ; Red
                (dracula-pro-blade-yellow   "#FFFF80" "#F9F986" "yellow")        ; Yellow
                ;; Other colors
                (dracula-pro-blade-bg2      "#1F2E2B" "#293D39" "brightblack")
                (dracula-pro-blade-bg3      "#293D39" "#334D48" "brightblack")
                (dracula-pro-blade-bg4      "#334C47" "#3D5C56" "brightblack")
                (dracula-pro-blade-fg2      "#EDEDDE" "#EBEBE0" "brightwhite")
                (dracula-pro-blade-fg3      "#D6D6C2" "#D1D1C7" "white")
                (dracula-pro-blade-fg4      "#BABAAB" "#B3B3B3" "white")
                (dracula-pro-blade-alt-blue "#8A75F0" "#846EF7" "brightblue")))
      (faces '(;; default
               (cursor :background ,dracula-pro-blade-fg3)
               (completions-first-difference :foreground ,dracula-pro-blade-pink :weight bold)
               (default :background ,dracula-pro-blade-bg :foreground ,dracula-pro-blade-fg)
               (default-italic :slant italic)
               (ffap :foreground ,dracula-pro-blade-fg4)
               (fringe :background ,dracula-pro-blade-bg :foreground ,dracula-pro-blade-fg4)
               (highlight :foreground ,dracula-pro-blade-fg3 :background ,dracula-pro-blade-bg3)
               (hl-line :background ,dracula-pro-blade-current :extend t)
               (info-quoted-name :foreground ,dracula-pro-blade-orange)
               (info-string :foreground ,dracula-pro-blade-yellow)
               (lazy-highlight :foreground ,dracula-pro-blade-fg2 :background ,dracula-pro-blade-bg2)
               (link :foreground ,dracula-pro-blade-cyan :underline t)
               (linum :slant italic :foreground ,dracula-pro-blade-bg4 :background ,dracula-pro-blade-bg)
               (line-number :slant italic :foreground ,dracula-pro-blade-bg4 :background ,dracula-pro-blade-bg)
               (match :background ,dracula-pro-blade-yellow :foreground ,dracula-pro-blade-bg)
               (minibuffer-prompt
                ,@(if dracula-pro-blade-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-blade-fg)
                    (list :weight 'bold :foreground dracula-pro-blade-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground nil :background ,dracula-pro-blade-orange)
               (vertical-border :foreground ,dracula-pro-blade-bg2)
               (success :foreground ,dracula-pro-blade-green)
               (warning :foreground ,dracula-pro-blade-orange)
               (error :foreground ,dracula-pro-blade-red)
               (header-line :background ,dracula-pro-blade-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,dracula-pro-blade-orange)
               (font-lock-comment-face :foreground ,dracula-pro-blade-comment)
               (font-lock-comment-delimiter-face :foreground ,dracula-pro-blade-comment)
               (font-lock-constant-face :foreground ,dracula-pro-blade-cyan)
               (font-lock-doc-face :foreground ,dracula-pro-blade-comment)
               (font-lock-function-name-face :foreground ,dracula-pro-blade-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,dracula-pro-blade-pink)
               (font-lock-negation-char-face :foreground ,dracula-pro-blade-cyan)
               (font-lock-preprocessor-face :foreground ,dracula-pro-blade-orange)
               (font-lock-reference-face :foreground ,dracula-pro-blade-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,dracula-pro-blade-cyan)
               (font-lock-regexp-grouping-construct :foreground ,dracula-pro-blade-purple)
               (font-lock-string-face :foreground ,dracula-pro-blade-yellow)
               (font-lock-type-face :foreground ,dracula-pro-blade-purple)
               (font-lock-variable-name-face :foreground ,dracula-pro-blade-fg
                                             :weight bold)
               (font-lock-warning-face :foreground ,dracula-pro-blade-orange :background ,dracula-pro-blade-bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,dracula-pro-blade-pink)
               ;; company
               (company-echo-common :foreground ,dracula-pro-blade-bg :background ,dracula-pro-blade-fg)
               (company-preview :background ,dracula-pro-blade-bg :foreground ,dracula-pro-blade-alt-blue)
               (company-preview-common :foreground ,dracula-pro-blade-bg2 :foreground ,dracula-pro-blade-fg3)
               (company-preview-search :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-bg)
               (company-scrollbar-bg :background ,dracula-pro-blade-bg3)
               (company-scrollbar-fg :foreground ,dracula-pro-blade-pink)
               (company-template-field :inherit match)
               (company-tooltip :foreground ,dracula-pro-blade-fg2 :background ,dracula-pro-blade-bg :weight bold)
               (company-tooltip-annotation :foreground ,dracula-pro-blade-cyan)
               (company-tooltip-common :foreground ,dracula-pro-blade-fg3)
               (company-tooltip-common-selection :foreground ,dracula-pro-blade-yellow)
               (company-tooltip-mouse :inherit highlight)
               (company-tooltip-selection :background ,dracula-pro-blade-bg3 :foreground ,dracula-pro-blade-fg3)
               ;; diff-hl
               (diff-hl-change :foreground ,dracula-pro-blade-orange :background ,dracula-pro-blade-orange)
               (diff-hl-delete :foreground ,dracula-pro-blade-red :background ,dracula-pro-blade-red)
               (diff-hl-insert :foreground ,dracula-pro-blade-green :background ,dracula-pro-blade-green)
               ;; dired
               (dired-directory :foreground ,dracula-pro-blade-green :weight normal)
               (dired-flagged :foreground ,dracula-pro-blade-pink)
               (dired-header :foreground ,dracula-pro-blade-fg3 :background ,dracula-pro-blade-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,dracula-pro-blade-fg :weight bold)
               (dired-marked :foreground ,dracula-pro-blade-orange :weight bold)
               (dired-perm-write :foreground ,dracula-pro-blade-fg3 :underline t)
               (dired-symlink :foreground ,dracula-pro-blade-yellow :weight normal :slant italic)
               (dired-warning :foreground ,dracula-pro-blade-orange :underline t)
               (diredp-compressed-file-name :foreground ,dracula-pro-blade-fg3)
               (diredp-compressed-file-suffix :foreground ,dracula-pro-blade-fg4)
               (diredp-date-time :foreground ,dracula-pro-blade-fg)
               (diredp-deletion-file-name :foreground ,dracula-pro-blade-pink :background ,dracula-pro-blade-current)
               (diredp-deletion :foreground ,dracula-pro-blade-pink :weight bold)
               (diredp-dir-heading :foreground ,dracula-pro-blade-fg2 :background ,dracula-pro-blade-bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,dracula-pro-blade-orange)
               (diredp-file-name :foreground ,dracula-pro-blade-fg)
               (diredp-file-suffix :foreground ,dracula-pro-blade-fg4)
               (diredp-flag-mark-line :foreground ,dracula-pro-blade-fg2 :slant italic :background ,dracula-pro-blade-current)
               (diredp-flag-mark :foreground ,dracula-pro-blade-fg2 :weight bold :background ,dracula-pro-blade-current)
               (diredp-ignored-file-name :foreground ,dracula-pro-blade-fg)
               (diredp-mode-line-flagged :foreground ,dracula-pro-blade-orange)
               (diredp-mode-line-marked :foreground ,dracula-pro-blade-orange)
               (diredp-no-priv :foreground ,dracula-pro-blade-fg)
               (diredp-number :foreground ,dracula-pro-blade-cyan)
               (diredp-other-priv :foreground ,dracula-pro-blade-orange)
               (diredp-rare-priv :foreground ,dracula-pro-blade-orange)
               (diredp-read-priv :foreground ,dracula-pro-blade-purple)
               (diredp-write-priv :foreground ,dracula-pro-blade-pink)
               (diredp-exec-priv :foreground ,dracula-pro-blade-yellow)
               (diredp-symlink :foreground ,dracula-pro-blade-orange)
               (diredp-link-priv :foreground ,dracula-pro-blade-orange)
               (diredp-autofile-name :foreground ,dracula-pro-blade-yellow)
               (diredp-tagged-autofile-name :foreground ,dracula-pro-blade-yellow)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,dracula-pro-blade-yellow)
               (enh-ruby-op-face :foreground ,dracula-pro-blade-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,dracula-pro-blade-yellow)
               (enh-ruby-string-delimiter-face :foreground ,dracula-pro-blade-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,dracula-pro-blade-orange))
               (flyspell-incorrect :underline (:style wave :color ,dracula-pro-blade-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,dracula-pro-blade-purple)
               (font-latex-italic-face :foreground ,dracula-pro-blade-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,dracula-pro-blade-cyan)
               (font-latex-match-variable-keywords :foreground ,dracula-pro-blade-fg)
               (font-latex-string-face :foreground ,dracula-pro-blade-yellow)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,dracula-pro-blade-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,dracula-pro-blade-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,dracula-pro-blade-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,dracula-pro-blade-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,dracula-pro-blade-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,dracula-pro-blade-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,dracula-pro-blade-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,dracula-pro-blade-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,dracula-pro-blade-pink)
               (gnus-header-from :foreground ,dracula-pro-blade-fg)
               (gnus-header-name :foreground ,dracula-pro-blade-purple)
               (gnus-header-subject :foreground ,dracula-pro-blade-green :weight bold)
               (gnus-summary-markup-face :foreground ,dracula-pro-blade-cyan)
               (gnus-summary-high-unread :foreground ,dracula-pro-blade-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,dracula-pro-blade-alt-blue :weight bold)
               (gnus-summary-normal-read :foreground ,dracula-pro-blade-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,dracula-pro-blade-pink :weight bold)
               (gnus-summary-low-unread :foreground ,dracula-pro-blade-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,dracula-pro-blade-pink)
               (haskell-constructor-face :foreground ,dracula-pro-blade-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,dracula-pro-blade-purple)
               (helm-buffer-not-saved :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-bg)
               (helm-buffer-process :foreground ,dracula-pro-blade-orange :background ,dracula-pro-blade-bg)
               (helm-buffer-saved-out :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-bg)
               (helm-buffer-size :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-bg)
               (helm-candidate-number :foreground ,dracula-pro-blade-bg :background ,dracula-pro-blade-fg)
               (helm-ff-directory :foreground ,dracula-pro-blade-green :background ,dracula-pro-blade-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,dracula-pro-blade-green :background ,dracula-pro-blade-bg :weight normal)
               (helm-ff-executable :foreground ,dracula-pro-blade-alt-blue :background ,dracula-pro-blade-bg :weight normal)
               (helm-ff-file :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,dracula-pro-blade-pink :background ,dracula-pro-blade-bg :weight bold)
               (helm-ff-prefix :foreground ,dracula-pro-blade-bg :background ,dracula-pro-blade-pink :weight normal)
               (helm-ff-symlink :foreground ,dracula-pro-blade-pink :background ,dracula-pro-blade-bg :weight bold)
               (helm-grep-cmd-line :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-bg)
               (helm-grep-file :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-bg)
               (helm-grep-finish :foreground ,dracula-pro-blade-fg2 :background ,dracula-pro-blade-bg)
               (helm-grep-lineno :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-bg)
               (helm-grep-match :foreground nil :background nil :inherit helm-match)
               (helm-grep-running :foreground ,dracula-pro-blade-green :background ,dracula-pro-blade-bg)
               (helm-header :foreground ,dracula-pro-blade-fg2 :background ,dracula-pro-blade-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,dracula-pro-blade-green :background ,dracula-pro-blade-bg)
               (helm-selection :background ,dracula-pro-blade-bg2 :underline nil)
               (helm-selection-line :background ,dracula-pro-blade-bg2)
               (helm-separator :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-bg)
               (helm-source-go-package-godoc-description :foreground ,dracula-pro-blade-yellow)
               (helm-source-header :foreground ,dracula-pro-blade-pink :background ,dracula-pro-blade-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,dracula-pro-blade-orange :background ,dracula-pro-blade-bg)
               (helm-time-zone-home :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-bg)
               (helm-visible-mark :foreground ,dracula-pro-blade-bg :background ,dracula-pro-blade-bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,dracula-pro-blade-bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,dracula-pro-blade-fg)
               (icicle-special-candidate :foreground ,dracula-pro-blade-fg2)
               (icicle-extra-candidate :foreground ,dracula-pro-blade-fg2)
               (icicle-search-main-regexp-others :foreground ,dracula-pro-blade-fg)
               (icicle-search-current-input :foreground ,dracula-pro-blade-pink)
               (icicle-search-context-level-8 :foreground ,dracula-pro-blade-orange)
               (icicle-search-context-level-7 :foreground ,dracula-pro-blade-orange)
               (icicle-search-context-level-6 :foreground ,dracula-pro-blade-orange)
               (icicle-search-context-level-5 :foreground ,dracula-pro-blade-orange)
               (icicle-search-context-level-4 :foreground ,dracula-pro-blade-orange)
               (icicle-search-context-level-3 :foreground ,dracula-pro-blade-orange)
               (icicle-search-context-level-2 :foreground ,dracula-pro-blade-orange)
               (icicle-search-context-level-1 :foreground ,dracula-pro-blade-orange)
               (icicle-search-main-regexp-current :foreground ,dracula-pro-blade-fg)
               (icicle-saved-candidate :foreground ,dracula-pro-blade-fg)
               (icicle-proxy-candidate :foreground ,dracula-pro-blade-fg)
               (icicle-mustmatch-completion :foreground ,dracula-pro-blade-purple)
               (icicle-multi-command-completion :foreground ,dracula-pro-blade-fg2 :background ,dracula-pro-blade-bg2)
               (icicle-msg-emphasis :foreground ,dracula-pro-blade-green)
               (icicle-mode-line-help :foreground ,dracula-pro-blade-fg4)
               (icicle-match-highlight-minibuffer :foreground ,dracula-pro-blade-orange)
               (icicle-match-highlight-Completions :foreground ,dracula-pro-blade-green)
               (icicle-key-complete-menu-local :foreground ,dracula-pro-blade-fg)
               (icicle-key-complete-menu :foreground ,dracula-pro-blade-fg)
               (icicle-input-completion-fail-lax :foreground ,dracula-pro-blade-pink)
               (icicle-input-completion-fail :foreground ,dracula-pro-blade-pink)
               (icicle-historical-candidate-other :foreground ,dracula-pro-blade-fg)
               (icicle-historical-candidate :foreground ,dracula-pro-blade-fg)
               (icicle-current-candidate-highlight :foreground ,dracula-pro-blade-orange :background ,dracula-pro-blade-bg3)
               (icicle-Completions-instruction-2 :foreground ,dracula-pro-blade-fg4)
               (icicle-Completions-instruction-1 :foreground ,dracula-pro-blade-fg4)
               (icicle-completion :foreground ,dracula-pro-blade-fg)
               (icicle-complete-input :foreground ,dracula-pro-blade-orange)
               (icicle-common-match-highlight-Completions :foreground ,dracula-pro-blade-purple)
               (icicle-candidate-part :foreground ,dracula-pro-blade-fg)
               (icicle-annotation :foreground ,dracula-pro-blade-fg4)
               ;; icomplete
               (icompletep-determined :foreground ,dracula-pro-blade-orange)
               ;; ido
               (ido-first-match
                ,@(if dracula-pro-blade-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-blade-green)
                    (list :weight 'bold :foreground dracula-pro-blade-pink)))
               (ido-only-match :foreground ,dracula-pro-blade-orange)
               (ido-subdir :foreground ,dracula-pro-blade-yellow)
               (ido-virtual :foreground ,dracula-pro-blade-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,dracula-pro-blade-bg :background ,dracula-pro-blade-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,dracula-pro-blade-cyan)
               (jde-java-font-lock-modifier-face :foreground ,dracula-pro-blade-pink)
               (jde-java-font-lock-number-face :foreground ,dracula-pro-blade-fg)
               (jde-java-font-lock-package-face :foreground ,dracula-pro-blade-fg)
               (jde-java-font-lock-private-face :foreground ,dracula-pro-blade-pink)
               (jde-java-font-lock-public-face :foreground ,dracula-pro-blade-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,dracula-pro-blade-purple)
               (js2-function-param :foreground ,dracula-pro-blade-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,dracula-pro-blade-yellow)
               (js2-jsdoc-html-tag-name :foreground ,dracula-pro-blade-alt-blue)
               (js2-jsdoc-value :foreground ,dracula-pro-blade-yellow)
               (js2-private-function-call :foreground ,dracula-pro-blade-cyan)
               (js2-private-member :foreground ,dracula-pro-blade-fg3)
               ;; js3-mode
               (js3-error-face :underline ,dracula-pro-blade-orange)
               (js3-external-variable-face :foreground ,dracula-pro-blade-fg)
               (js3-function-param-face :foreground ,dracula-pro-blade-pink)
               (js3-instance-member-face :foreground ,dracula-pro-blade-cyan)
               (js3-jsdoc-tag-face :foreground ,dracula-pro-blade-pink)
               (js3-warning-face :underline ,dracula-pro-blade-pink)
               ;; magit
               (magit-branch-local :foreground ,dracula-pro-blade-cyan)
               (magit-branch-remote :foreground ,dracula-pro-blade-green)
               (magit-tag :foreground ,dracula-pro-blade-orange)
               (magit-section-heading :foreground ,dracula-pro-blade-pink :weight bold)
               (magit-section-highlight :background ,dracula-pro-blade-bg3 :extend t)
               (magit-diff-context-highlight :background ,dracula-pro-blade-bg3
                                             :foreground ,dracula-pro-blade-fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,dracula-pro-blade-orange
                                            :background ,dracula-pro-blade-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,dracula-pro-blade-orange
                                                      :background ,dracula-pro-blade-bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,dracula-pro-blade-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,dracula-pro-blade-green)
               (magit-diffstat-removed :foreground ,dracula-pro-blade-red)
               (magit-hash :foreground ,dracula-pro-blade-fg2)
               (magit-hunk-heading :background ,dracula-pro-blade-bg3)
               (magit-hunk-heading-highlight :background ,dracula-pro-blade-bg3)
               (magit-item-highlight :background ,dracula-pro-blade-bg3)
               (magit-log-author :foreground ,dracula-pro-blade-fg3)
               (magit-process-ng :foreground ,dracula-pro-blade-orange :weight bold)
               (magit-process-ok :foreground ,dracula-pro-blade-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,dracula-pro-blade-orange)
               (markdown-code-face :foreground ,dracula-pro-blade-orange)
               (markdown-footnote-face :foreground ,dracula-pro-blade-alt-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,dracula-pro-blade-pink
                ,@(when dracula-pro-blade-enlarge-headings
                    (list :height dracula-pro-blade-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,dracula-pro-blade-purple
                ,@(when dracula-pro-blade-enlarge-headings
                    (list :height dracula-pro-blade-height-title-2)))
               (markdown-header-face-3
                :foreground ,dracula-pro-blade-green
                ,@(when dracula-pro-blade-enlarge-headings
                    (list :height dracula-pro-blade-height-title-3)))
               (markdown-header-face-4 :foreground ,dracula-pro-blade-yellow)
               (markdown-header-face-5 :foreground ,dracula-pro-blade-cyan)
               (markdown-header-face-6 :foreground ,dracula-pro-blade-orange)
               (markdown-header-face-7 :foreground ,dracula-pro-blade-alt-blue)
               (markdown-header-face-8 :foreground ,dracula-pro-blade-fg)
               (markdown-inline-code-face :foreground ,dracula-pro-blade-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,dracula-pro-blade-orange)
               (markdown-table-face :foreground ,dracula-pro-blade-purple)
               ;; message
               (message-mml :foreground ,dracula-pro-blade-green :weight normal)
               (message-header-xheader :foreground ,dracula-pro-blade-cyan :weight normal)
               ;; mode-line
               (mode-line :background ,dracula-pro-blade-current
                          :box ,dracula-pro-blade-current :inverse-video nil
                          ,@(if dracula-pro-blade-alternate-mode-line-and-minibuffer
                                (list :foreground dracula-pro-blade-fg3)
                              (list :foreground nil)))
               (mode-line-inactive
                :inverse-video nil
                ,@(if dracula-pro-blade-alternate-mode-line-and-minibuffer
                      (list :foreground dracula-pro-blade-comment :background dracula-pro-blade-bg
                            :box dracula-pro-blade-bg)
                    (list :foreground dracula-pro-blade-fg :background dracula-pro-blade-bg2 :box dracula-pro-blade-bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,dracula-pro-blade-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,dracula-pro-blade-purple)
               (mu4e-highlight-face :background ,dracula-pro-blade-bg
                                    :foreground ,dracula-pro-blade-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,dracula-pro-blade-current
                                           :foreground ,dracula-pro-blade-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,dracula-pro-blade-purple)
               (mu4e-cited-1-face :foreground ,dracula-pro-blade-purple)
               (mu4e-cited-2-face :foreground ,dracula-pro-blade-orange)
               (mu4e-cited-3-face :foreground ,dracula-pro-blade-comment)
               (mu4e-cited-4-face :foreground ,dracula-pro-blade-fg2)
               (mu4e-cited-5-face :foreground ,dracula-pro-blade-fg3)
               ;; org
               (org-agenda-date :foreground ,dracula-pro-blade-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,dracula-pro-blade-comment)
               (org-agenda-done :foreground ,dracula-pro-blade-green)
               (org-agenda-structure :foreground ,dracula-pro-blade-purple)
               (org-block :foreground ,dracula-pro-blade-orange)
               (org-code :foreground ,dracula-pro-blade-yellow)
               (org-column :background ,dracula-pro-blade-bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,dracula-pro-blade-cyan :underline t)
               (org-document-info :foreground ,dracula-pro-blade-alt-blue)
               (org-document-info-keyword :foreground ,dracula-pro-blade-comment)
               (org-document-title :weight bold :foreground ,dracula-pro-blade-orange
                                   ,@(when dracula-pro-blade-enlarge-headings
                                       (list :height dracula-pro-blade-height-doc-title)))
               (org-done :foreground ,dracula-pro-blade-green)
               (org-ellipsis :foreground ,dracula-pro-blade-comment)
               (org-footnote :foreground ,dracula-pro-blade-alt-blue)
               (org-formula :foreground ,dracula-pro-blade-pink)
               (org-headline-done :foreground ,dracula-pro-blade-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,dracula-pro-blade-bg :background ,dracula-pro-blade-bg)
               (org-level-1 :inherit bold :foreground ,dracula-pro-blade-pink
                            ,@(when dracula-pro-blade-enlarge-headings
                                (list :height dracula-pro-blade-height-title-1)))
               (org-level-2 :inherit bold :foreground ,dracula-pro-blade-purple
                            ,@(when dracula-pro-blade-enlarge-headings
                                (list :height dracula-pro-blade-height-title-2)))
               (org-level-3 :weight normal :foreground ,dracula-pro-blade-green
                            ,@(when dracula-pro-blade-enlarge-headings
                                (list :height dracula-pro-blade-height-title-3)))
               (org-level-4 :weight normal :foreground ,dracula-pro-blade-yellow)
               (org-level-5 :weight normal :foreground ,dracula-pro-blade-cyan)
               (org-level-6 :weight normal :foreground ,dracula-pro-blade-orange)
               (org-level-7 :weight normal :foreground ,dracula-pro-blade-alt-blue)
               (org-level-8 :weight normal :foreground ,dracula-pro-blade-fg)
               (org-link :foreground ,dracula-pro-blade-cyan :underline t)
               (org-priority :foreground ,dracula-pro-blade-cyan)
               (org-scheduled :foreground ,dracula-pro-blade-green)
               (org-scheduled-previously :foreground ,dracula-pro-blade-yellow)
               (org-scheduled-today :foreground ,dracula-pro-blade-green)
               (org-sexp-date :foreground ,dracula-pro-blade-fg4)
               (org-special-keyword :foreground ,dracula-pro-blade-yellow)
               (org-table :foreground ,dracula-pro-blade-purple)
               (org-tag :foreground ,dracula-pro-blade-pink :weight bold :background ,dracula-pro-blade-bg2)
               (org-todo :foreground ,dracula-pro-blade-orange :weight bold :background ,dracula-pro-blade-bg2)
               (org-upcoming-deadline :foreground ,dracula-pro-blade-yellow)
               (org-warning :weight bold :foreground ,dracula-pro-blade-pink)
               ;; outline
               (outline-1 :foreground ,dracula-pro-blade-pink)
               (outline-2 :foreground ,dracula-pro-blade-purple)
               (outline-3 :foreground ,dracula-pro-blade-green)
               (outline-4 :foreground ,dracula-pro-blade-yellow)
               (outline-5 :foreground ,dracula-pro-blade-cyan)
               (outline-6 :foreground ,dracula-pro-blade-orange)
               ;; powerline
               (powerline-evil-base-face :foreground ,dracula-pro-blade-bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,dracula-pro-blade-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,dracula-pro-blade-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,dracula-pro-blade-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,dracula-pro-blade-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,dracula-pro-blade-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,dracula-pro-blade-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,dracula-pro-blade-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,dracula-pro-blade-fg)
               (rainbow-delimiters-depth-2-face :foreground ,dracula-pro-blade-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,dracula-pro-blade-purple)
               (rainbow-delimiters-depth-4-face :foreground ,dracula-pro-blade-pink)
               (rainbow-delimiters-depth-5-face :foreground ,dracula-pro-blade-orange)
               (rainbow-delimiters-depth-6-face :foreground ,dracula-pro-blade-green)
               (rainbow-delimiters-depth-7-face :foreground ,dracula-pro-blade-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,dracula-pro-blade-alt-blue)
               (rainbow-delimiters-unmatched-face :foreground ,dracula-pro-blade-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,dracula-pro-blade-green)
               (rpm-spec-doc-face :foreground ,dracula-pro-blade-pink)
               (rpm-spec-ghost-face :foreground ,dracula-pro-blade-purple)
               (rpm-spec-macro-face :foreground ,dracula-pro-blade-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,dracula-pro-blade-purple)
               (rpm-spec-section-face :foreground ,dracula-pro-blade-yellow)
               (rpm-spec-tag-face :foreground ,dracula-pro-blade-cyan)
               (rpm-spec-var-face :foreground ,dracula-pro-blade-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,dracula-pro-blade-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,dracula-pro-blade-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,dracula-pro-blade-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,dracula-pro-blade-orange
                     :strike-through t :slant oblique)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,dracula-pro-blade-pink :background ,dracula-pro-blade-bg
                            :box (:line-width 2 :color ,dracula-pro-blade-bg :style nil))
               (tab-bar-tab-inactive :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-bg2
                                     :box (:line-width 2 :color ,dracula-pro-blade-bg2 :style nil))
               (tab-line :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,dracula-pro-blade-pink :background ,dracula-pro-blade-bg
                             :box (:line-width 2 :color ,dracula-pro-blade-bg :style nil))
               (tab-line-tab-inactive :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-bg2
                                      :box (:line-width 2 :color ,dracula-pro-blade-bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,dracula-pro-blade-red)
               ;; term
               (term :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-bg)
               (term-color-black :foreground ,dracula-pro-blade-bg :background ,dracula-pro-blade-bg)
               (term-color-blue :foreground ,dracula-pro-blade-purple :background ,dracula-pro-blade-purple)
               (term-color-cyan :foreground ,dracula-pro-blade-cyan :background ,dracula-pro-blade-cyan)
               (term-color-green :foreground ,dracula-pro-blade-green :background ,dracula-pro-blade-green)
               (term-color-magenta :foreground ,dracula-pro-blade-pink :background ,dracula-pro-blade-pink)
               (term-color-red :foreground ,dracula-pro-blade-red :background ,dracula-pro-blade-red)
               (term-color-white :foreground ,dracula-pro-blade-fg :background ,dracula-pro-blade-fg)
               (term-color-yellow :foreground ,dracula-pro-blade-yellow :background ,dracula-pro-blade-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,dracula-pro-blade-orange)
               (undo-tree-visualizer-default-face :foreground ,dracula-pro-blade-fg2)
               (undo-tree-visualizer-register-face :foreground ,dracula-pro-blade-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,dracula-pro-blade-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,dracula-pro-blade-purple)
               (web-mode-html-attr-value-face :foreground ,dracula-pro-blade-green)
               (web-mode-html-tag-face :foreground ,dracula-pro-blade-pink :weight bold)
               (web-mode-keyword-face :foreground ,dracula-pro-blade-pink)
               (web-mode-string-face :foreground ,dracula-pro-blade-yellow)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,dracula-pro-blade-red :foreground ,dracula-pro-blade-red)
               (whitespace-empty :background ,dracula-pro-blade-orange :foreground ,dracula-pro-blade-red)
               (whitespace-hspace :background ,dracula-pro-blade-bg3 :foreground ,dracula-pro-blade-comment)
               (whitespace-indentation :background ,dracula-pro-blade-orange :foreground ,dracula-pro-blade-red)
               (whitespace-line :background ,dracula-pro-blade-bg :foreground ,dracula-pro-blade-pink)
               (whitespace-newline :foreground ,dracula-pro-blade-comment)
               (whitespace-space :background ,dracula-pro-blade-bg :foreground ,dracula-pro-blade-comment)
               (whitespace-space-after-tab :background ,dracula-pro-blade-orange :foreground ,dracula-pro-blade-red)
               (whitespace-space-before-tab :background ,dracula-pro-blade-orange :foreground ,dracula-pro-blade-red)
               (whitespace-tab :background ,dracula-pro-blade-bg2 :foreground ,dracula-pro-blade-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'dracula-pro-blade
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (term-colors (mapcar #'car (mapcar #'cddr colors)))
               (tty-colors (mapcar #'car (mapcar #'last colors)))
               (expand-for-kind (lambda (kind spec)
                                  (cl-progv color-names kind
                                    (eval `(backquote ,spec))))))
           (cl-loop for (face . spec) in faces
                    collect `(,face
                              ((((min-colors 16777216)) ; fully graphical envs
                                ,(funcall expand-for-kind graphic-colors spec))
                               (((min-colors 256))      ; terminal withs 256 colors
                                ,(funcall expand-for-kind term-colors spec))
                               (t                       ; should be only tty-like envs
                                ,(funcall expand-for-kind tty-colors spec))))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula-pro-blade)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; dracula-pro-blade-theme.el ends here