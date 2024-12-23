;;; dracula-pro-lincoln-theme.el --- Dracula Pro

;; Copyright (C) 2020-Today Dracula Theme.

;; Author: Dracula Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://draculatheme.com/pro

;;; Commentary:
;; Dracula PRO is a color scheme and UI theme tailored for programming. Made for terminal emulators, code editors, and syntax highlighters.

;;; Code:

(require 'cl-lib)
(deftheme dracula-pro-lincoln
  "Dracula PRO - Lincoln Variant")


;;;; Configuration options:

(defgroup dracula-pro-lincoln nil
  "Dracula theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dracula-pro-lincoln-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'dracula-pro-lincoln)

(defcustom dracula-pro-lincoln-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'dracula-pro-lincoln)

(defcustom dracula-pro-lincoln-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'dracula-pro-lincoln)

(defcustom dracula-pro-lincoln-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'dracula-pro-lincoln)

(defcustom dracula-pro-lincoln-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'dracula-pro-lincoln)

(defcustom dracula-pro-lincoln-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula-pro-lincoln)


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [TTY-COLOR]
(let ((colors '(;; Upstream theme color
                (dracula-pro-lincoln-bg       "#2C2A21" "#2E2B1F" nil)             ; Background
                (dracula-pro-lincoln-fg       "#F8F8F2" "#F9F9F1" "brightwhite")   ; Foreground
                (dracula-pro-lincoln-current  "#585441" "#5C563D" "brightblack")   ; Current-line/selection
                (dracula-pro-lincoln-comment  "#A99F70" "#AFA26A" "blue")          ; Comment
                (dracula-pro-lincoln-cyan     "#80FFEA" "#86F9E6" "brightcyan")    ; Cyan
                (dracula-pro-lincoln-green    "#8AFF80" "#8FF986" "green")         ; Green
                (dracula-pro-lincoln-orange   "#FFCA80" "#F9C986" "brightred")     ; Orange
                (dracula-pro-lincoln-pink     "#FF80BF" "#F986BF" "magenta")       ; Pink
                (dracula-pro-lincoln-purple   "#9580FF" "#9986F9" "brightmagenta") ; Purple
                (dracula-pro-lincoln-red      "#FF9580" "#F99986" "red")           ; Red
                (dracula-pro-lincoln-yellow   "#FFFF80" "#F9F986" "yellow")        ; Yellow
                ;; Other colors
                (dracula-pro-lincoln-bg2      "#2E2B1F" "#3D3929" "brightblack")
                (dracula-pro-lincoln-bg3      "#3D3929" "#4D4833" "brightblack")
                (dracula-pro-lincoln-bg4      "#4C4733" "#5C563D" "brightblack")
                (dracula-pro-lincoln-fg2      "#EDEDDE" "#EBEBE0" "brightwhite")
                (dracula-pro-lincoln-fg3      "#D6D6C2" "#D1D1C7" "white")
                (dracula-pro-lincoln-fg4      "#BABAAB" "#B3B3B3" "white")
                (dracula-pro-lincoln-alt-blue "#8A75F0" "#846EF7" "brightblue")))
      (faces '(;; default
               (cursor :background ,dracula-pro-lincoln-fg3)
               (completions-first-difference :foreground ,dracula-pro-lincoln-pink :weight bold)
               (default :background ,dracula-pro-lincoln-bg :foreground ,dracula-pro-lincoln-fg)
               (default-italic :slant italic)
               (ffap :foreground ,dracula-pro-lincoln-fg4)
               (fringe :background ,dracula-pro-lincoln-bg :foreground ,dracula-pro-lincoln-fg4)
               (highlight :foreground ,dracula-pro-lincoln-fg3 :background ,dracula-pro-lincoln-bg3)
               (hl-line :background ,dracula-pro-lincoln-current :extend t)
               (info-quoted-name :foreground ,dracula-pro-lincoln-orange)
               (info-string :foreground ,dracula-pro-lincoln-yellow)
               (lazy-highlight :foreground ,dracula-pro-lincoln-fg2 :background ,dracula-pro-lincoln-bg2)
               (link :foreground ,dracula-pro-lincoln-cyan :underline t)
               (linum :slant italic :foreground ,dracula-pro-lincoln-bg4 :background ,dracula-pro-lincoln-bg)
               (line-number :slant italic :foreground ,dracula-pro-lincoln-bg4 :background ,dracula-pro-lincoln-bg)
               (match :background ,dracula-pro-lincoln-yellow :foreground ,dracula-pro-lincoln-bg)
               (minibuffer-prompt
                ,@(if dracula-pro-lincoln-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-lincoln-fg)
                    (list :weight 'bold :foreground dracula-pro-lincoln-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground nil :background ,dracula-pro-lincoln-orange)
               (vertical-border :foreground ,dracula-pro-lincoln-bg2)
               (success :foreground ,dracula-pro-lincoln-green)
               (warning :foreground ,dracula-pro-lincoln-orange)
               (error :foreground ,dracula-pro-lincoln-red)
               (header-line :background ,dracula-pro-lincoln-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,dracula-pro-lincoln-orange)
               (font-lock-comment-face :foreground ,dracula-pro-lincoln-comment)
               (font-lock-comment-delimiter-face :foreground ,dracula-pro-lincoln-comment)
               (font-lock-constant-face :foreground ,dracula-pro-lincoln-cyan)
               (font-lock-doc-face :foreground ,dracula-pro-lincoln-comment)
               (font-lock-function-name-face :foreground ,dracula-pro-lincoln-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,dracula-pro-lincoln-pink)
               (font-lock-negation-char-face :foreground ,dracula-pro-lincoln-cyan)
               (font-lock-preprocessor-face :foreground ,dracula-pro-lincoln-orange)
               (font-lock-reference-face :foreground ,dracula-pro-lincoln-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,dracula-pro-lincoln-cyan)
               (font-lock-regexp-grouping-construct :foreground ,dracula-pro-lincoln-purple)
               (font-lock-string-face :foreground ,dracula-pro-lincoln-yellow)
               (font-lock-type-face :foreground ,dracula-pro-lincoln-purple)
               (font-lock-variable-name-face :foreground ,dracula-pro-lincoln-fg
                                             :weight bold)
               (font-lock-warning-face :foreground ,dracula-pro-lincoln-orange :background ,dracula-pro-lincoln-bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,dracula-pro-lincoln-pink)
               ;; company
               (company-echo-common :foreground ,dracula-pro-lincoln-bg :background ,dracula-pro-lincoln-fg)
               (company-preview :background ,dracula-pro-lincoln-bg :foreground ,dracula-pro-lincoln-alt-blue)
               (company-preview-common :foreground ,dracula-pro-lincoln-bg2 :foreground ,dracula-pro-lincoln-fg3)
               (company-preview-search :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-bg)
               (company-scrollbar-bg :background ,dracula-pro-lincoln-bg3)
               (company-scrollbar-fg :foreground ,dracula-pro-lincoln-pink)
               (company-template-field :inherit match)
               (company-tooltip :foreground ,dracula-pro-lincoln-fg2 :background ,dracula-pro-lincoln-bg :weight bold)
               (company-tooltip-annotation :foreground ,dracula-pro-lincoln-cyan)
               (company-tooltip-common :foreground ,dracula-pro-lincoln-fg3)
               (company-tooltip-common-selection :foreground ,dracula-pro-lincoln-yellow)
               (company-tooltip-mouse :inherit highlight)
               (company-tooltip-selection :background ,dracula-pro-lincoln-bg3 :foreground ,dracula-pro-lincoln-fg3)
               ;; diff-hl
               (diff-hl-change :foreground ,dracula-pro-lincoln-orange :background ,dracula-pro-lincoln-orange)
               (diff-hl-delete :foreground ,dracula-pro-lincoln-red :background ,dracula-pro-lincoln-red)
               (diff-hl-insert :foreground ,dracula-pro-lincoln-green :background ,dracula-pro-lincoln-green)
               ;; dired
               (dired-directory :foreground ,dracula-pro-lincoln-green :weight normal)
               (dired-flagged :foreground ,dracula-pro-lincoln-pink)
               (dired-header :foreground ,dracula-pro-lincoln-fg3 :background ,dracula-pro-lincoln-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,dracula-pro-lincoln-fg :weight bold)
               (dired-marked :foreground ,dracula-pro-lincoln-orange :weight bold)
               (dired-perm-write :foreground ,dracula-pro-lincoln-fg3 :underline t)
               (dired-symlink :foreground ,dracula-pro-lincoln-yellow :weight normal :slant italic)
               (dired-warning :foreground ,dracula-pro-lincoln-orange :underline t)
               (diredp-compressed-file-name :foreground ,dracula-pro-lincoln-fg3)
               (diredp-compressed-file-suffix :foreground ,dracula-pro-lincoln-fg4)
               (diredp-date-time :foreground ,dracula-pro-lincoln-fg)
               (diredp-deletion-file-name :foreground ,dracula-pro-lincoln-pink :background ,dracula-pro-lincoln-current)
               (diredp-deletion :foreground ,dracula-pro-lincoln-pink :weight bold)
               (diredp-dir-heading :foreground ,dracula-pro-lincoln-fg2 :background ,dracula-pro-lincoln-bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,dracula-pro-lincoln-orange)
               (diredp-file-name :foreground ,dracula-pro-lincoln-fg)
               (diredp-file-suffix :foreground ,dracula-pro-lincoln-fg4)
               (diredp-flag-mark-line :foreground ,dracula-pro-lincoln-fg2 :slant italic :background ,dracula-pro-lincoln-current)
               (diredp-flag-mark :foreground ,dracula-pro-lincoln-fg2 :weight bold :background ,dracula-pro-lincoln-current)
               (diredp-ignored-file-name :foreground ,dracula-pro-lincoln-fg)
               (diredp-mode-line-flagged :foreground ,dracula-pro-lincoln-orange)
               (diredp-mode-line-marked :foreground ,dracula-pro-lincoln-orange)
               (diredp-no-priv :foreground ,dracula-pro-lincoln-fg)
               (diredp-number :foreground ,dracula-pro-lincoln-cyan)
               (diredp-other-priv :foreground ,dracula-pro-lincoln-orange)
               (diredp-rare-priv :foreground ,dracula-pro-lincoln-orange)
               (diredp-read-priv :foreground ,dracula-pro-lincoln-purple)
               (diredp-write-priv :foreground ,dracula-pro-lincoln-pink)
               (diredp-exec-priv :foreground ,dracula-pro-lincoln-yellow)
               (diredp-symlink :foreground ,dracula-pro-lincoln-orange)
               (diredp-link-priv :foreground ,dracula-pro-lincoln-orange)
               (diredp-autofile-name :foreground ,dracula-pro-lincoln-yellow)
               (diredp-tagged-autofile-name :foreground ,dracula-pro-lincoln-yellow)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,dracula-pro-lincoln-yellow)
               (enh-ruby-op-face :foreground ,dracula-pro-lincoln-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,dracula-pro-lincoln-yellow)
               (enh-ruby-string-delimiter-face :foreground ,dracula-pro-lincoln-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,dracula-pro-lincoln-orange))
               (flyspell-incorrect :underline (:style wave :color ,dracula-pro-lincoln-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,dracula-pro-lincoln-purple)
               (font-latex-italic-face :foreground ,dracula-pro-lincoln-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,dracula-pro-lincoln-cyan)
               (font-latex-match-variable-keywords :foreground ,dracula-pro-lincoln-fg)
               (font-latex-string-face :foreground ,dracula-pro-lincoln-yellow)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,dracula-pro-lincoln-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,dracula-pro-lincoln-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,dracula-pro-lincoln-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,dracula-pro-lincoln-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,dracula-pro-lincoln-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,dracula-pro-lincoln-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,dracula-pro-lincoln-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,dracula-pro-lincoln-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,dracula-pro-lincoln-pink)
               (gnus-header-from :foreground ,dracula-pro-lincoln-fg)
               (gnus-header-name :foreground ,dracula-pro-lincoln-purple)
               (gnus-header-subject :foreground ,dracula-pro-lincoln-green :weight bold)
               (gnus-summary-markup-face :foreground ,dracula-pro-lincoln-cyan)
               (gnus-summary-high-unread :foreground ,dracula-pro-lincoln-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,dracula-pro-lincoln-alt-blue :weight bold)
               (gnus-summary-normal-read :foreground ,dracula-pro-lincoln-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,dracula-pro-lincoln-pink :weight bold)
               (gnus-summary-low-unread :foreground ,dracula-pro-lincoln-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,dracula-pro-lincoln-pink)
               (haskell-constructor-face :foreground ,dracula-pro-lincoln-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,dracula-pro-lincoln-purple)
               (helm-buffer-not-saved :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-bg)
               (helm-buffer-process :foreground ,dracula-pro-lincoln-orange :background ,dracula-pro-lincoln-bg)
               (helm-buffer-saved-out :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-bg)
               (helm-buffer-size :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-bg)
               (helm-candidate-number :foreground ,dracula-pro-lincoln-bg :background ,dracula-pro-lincoln-fg)
               (helm-ff-directory :foreground ,dracula-pro-lincoln-green :background ,dracula-pro-lincoln-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,dracula-pro-lincoln-green :background ,dracula-pro-lincoln-bg :weight normal)
               (helm-ff-executable :foreground ,dracula-pro-lincoln-alt-blue :background ,dracula-pro-lincoln-bg :weight normal)
               (helm-ff-file :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,dracula-pro-lincoln-pink :background ,dracula-pro-lincoln-bg :weight bold)
               (helm-ff-prefix :foreground ,dracula-pro-lincoln-bg :background ,dracula-pro-lincoln-pink :weight normal)
               (helm-ff-symlink :foreground ,dracula-pro-lincoln-pink :background ,dracula-pro-lincoln-bg :weight bold)
               (helm-grep-cmd-line :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-bg)
               (helm-grep-file :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-bg)
               (helm-grep-finish :foreground ,dracula-pro-lincoln-fg2 :background ,dracula-pro-lincoln-bg)
               (helm-grep-lineno :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-bg)
               (helm-grep-match :foreground nil :background nil :inherit helm-match)
               (helm-grep-running :foreground ,dracula-pro-lincoln-green :background ,dracula-pro-lincoln-bg)
               (helm-header :foreground ,dracula-pro-lincoln-fg2 :background ,dracula-pro-lincoln-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,dracula-pro-lincoln-green :background ,dracula-pro-lincoln-bg)
               (helm-selection :background ,dracula-pro-lincoln-bg2 :underline nil)
               (helm-selection-line :background ,dracula-pro-lincoln-bg2)
               (helm-separator :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-bg)
               (helm-source-go-package-godoc-description :foreground ,dracula-pro-lincoln-yellow)
               (helm-source-header :foreground ,dracula-pro-lincoln-pink :background ,dracula-pro-lincoln-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,dracula-pro-lincoln-orange :background ,dracula-pro-lincoln-bg)
               (helm-time-zone-home :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-bg)
               (helm-visible-mark :foreground ,dracula-pro-lincoln-bg :background ,dracula-pro-lincoln-bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,dracula-pro-lincoln-bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,dracula-pro-lincoln-fg)
               (icicle-special-candidate :foreground ,dracula-pro-lincoln-fg2)
               (icicle-extra-candidate :foreground ,dracula-pro-lincoln-fg2)
               (icicle-search-main-regexp-others :foreground ,dracula-pro-lincoln-fg)
               (icicle-search-current-input :foreground ,dracula-pro-lincoln-pink)
               (icicle-search-context-level-8 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-context-level-7 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-context-level-6 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-context-level-5 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-context-level-4 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-context-level-3 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-context-level-2 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-context-level-1 :foreground ,dracula-pro-lincoln-orange)
               (icicle-search-main-regexp-current :foreground ,dracula-pro-lincoln-fg)
               (icicle-saved-candidate :foreground ,dracula-pro-lincoln-fg)
               (icicle-proxy-candidate :foreground ,dracula-pro-lincoln-fg)
               (icicle-mustmatch-completion :foreground ,dracula-pro-lincoln-purple)
               (icicle-multi-command-completion :foreground ,dracula-pro-lincoln-fg2 :background ,dracula-pro-lincoln-bg2)
               (icicle-msg-emphasis :foreground ,dracula-pro-lincoln-green)
               (icicle-mode-line-help :foreground ,dracula-pro-lincoln-fg4)
               (icicle-match-highlight-minibuffer :foreground ,dracula-pro-lincoln-orange)
               (icicle-match-highlight-Completions :foreground ,dracula-pro-lincoln-green)
               (icicle-key-complete-menu-local :foreground ,dracula-pro-lincoln-fg)
               (icicle-key-complete-menu :foreground ,dracula-pro-lincoln-fg)
               (icicle-input-completion-fail-lax :foreground ,dracula-pro-lincoln-pink)
               (icicle-input-completion-fail :foreground ,dracula-pro-lincoln-pink)
               (icicle-historical-candidate-other :foreground ,dracula-pro-lincoln-fg)
               (icicle-historical-candidate :foreground ,dracula-pro-lincoln-fg)
               (icicle-current-candidate-highlight :foreground ,dracula-pro-lincoln-orange :background ,dracula-pro-lincoln-bg3)
               (icicle-Completions-instruction-2 :foreground ,dracula-pro-lincoln-fg4)
               (icicle-Completions-instruction-1 :foreground ,dracula-pro-lincoln-fg4)
               (icicle-completion :foreground ,dracula-pro-lincoln-fg)
               (icicle-complete-input :foreground ,dracula-pro-lincoln-orange)
               (icicle-common-match-highlight-Completions :foreground ,dracula-pro-lincoln-purple)
               (icicle-candidate-part :foreground ,dracula-pro-lincoln-fg)
               (icicle-annotation :foreground ,dracula-pro-lincoln-fg4)
               ;; icomplete
               (icompletep-determined :foreground ,dracula-pro-lincoln-orange)
               ;; ido
               (ido-first-match
                ,@(if dracula-pro-lincoln-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-lincoln-green)
                    (list :weight 'bold :foreground dracula-pro-lincoln-pink)))
               (ido-only-match :foreground ,dracula-pro-lincoln-orange)
               (ido-subdir :foreground ,dracula-pro-lincoln-yellow)
               (ido-virtual :foreground ,dracula-pro-lincoln-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,dracula-pro-lincoln-bg :background ,dracula-pro-lincoln-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,dracula-pro-lincoln-cyan)
               (jde-java-font-lock-modifier-face :foreground ,dracula-pro-lincoln-pink)
               (jde-java-font-lock-number-face :foreground ,dracula-pro-lincoln-fg)
               (jde-java-font-lock-package-face :foreground ,dracula-pro-lincoln-fg)
               (jde-java-font-lock-private-face :foreground ,dracula-pro-lincoln-pink)
               (jde-java-font-lock-public-face :foreground ,dracula-pro-lincoln-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,dracula-pro-lincoln-purple)
               (js2-function-param :foreground ,dracula-pro-lincoln-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,dracula-pro-lincoln-yellow)
               (js2-jsdoc-html-tag-name :foreground ,dracula-pro-lincoln-alt-blue)
               (js2-jsdoc-value :foreground ,dracula-pro-lincoln-yellow)
               (js2-private-function-call :foreground ,dracula-pro-lincoln-cyan)
               (js2-private-member :foreground ,dracula-pro-lincoln-fg3)
               ;; js3-mode
               (js3-error-face :underline ,dracula-pro-lincoln-orange)
               (js3-external-variable-face :foreground ,dracula-pro-lincoln-fg)
               (js3-function-param-face :foreground ,dracula-pro-lincoln-pink)
               (js3-instance-member-face :foreground ,dracula-pro-lincoln-cyan)
               (js3-jsdoc-tag-face :foreground ,dracula-pro-lincoln-pink)
               (js3-warning-face :underline ,dracula-pro-lincoln-pink)
               ;; magit
               (magit-branch-local :foreground ,dracula-pro-lincoln-cyan)
               (magit-branch-remote :foreground ,dracula-pro-lincoln-green)
               (magit-tag :foreground ,dracula-pro-lincoln-orange)
               (magit-section-heading :foreground ,dracula-pro-lincoln-pink :weight bold)
               (magit-section-highlight :background ,dracula-pro-lincoln-bg3 :extend t)
               (magit-diff-context-highlight :background ,dracula-pro-lincoln-bg3
                                             :foreground ,dracula-pro-lincoln-fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,dracula-pro-lincoln-orange
                                            :background ,dracula-pro-lincoln-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,dracula-pro-lincoln-orange
                                                      :background ,dracula-pro-lincoln-bg3
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
               (magit-diff-file-heading :foreground ,dracula-pro-lincoln-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,dracula-pro-lincoln-green)
               (magit-diffstat-removed :foreground ,dracula-pro-lincoln-red)
               (magit-hash :foreground ,dracula-pro-lincoln-fg2)
               (magit-hunk-heading :background ,dracula-pro-lincoln-bg3)
               (magit-hunk-heading-highlight :background ,dracula-pro-lincoln-bg3)
               (magit-item-highlight :background ,dracula-pro-lincoln-bg3)
               (magit-log-author :foreground ,dracula-pro-lincoln-fg3)
               (magit-process-ng :foreground ,dracula-pro-lincoln-orange :weight bold)
               (magit-process-ok :foreground ,dracula-pro-lincoln-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,dracula-pro-lincoln-orange)
               (markdown-code-face :foreground ,dracula-pro-lincoln-orange)
               (markdown-footnote-face :foreground ,dracula-pro-lincoln-alt-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,dracula-pro-lincoln-pink
                ,@(when dracula-pro-lincoln-enlarge-headings
                    (list :height dracula-pro-lincoln-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,dracula-pro-lincoln-purple
                ,@(when dracula-pro-lincoln-enlarge-headings
                    (list :height dracula-pro-lincoln-height-title-2)))
               (markdown-header-face-3
                :foreground ,dracula-pro-lincoln-green
                ,@(when dracula-pro-lincoln-enlarge-headings
                    (list :height dracula-pro-lincoln-height-title-3)))
               (markdown-header-face-4 :foreground ,dracula-pro-lincoln-yellow)
               (markdown-header-face-5 :foreground ,dracula-pro-lincoln-cyan)
               (markdown-header-face-6 :foreground ,dracula-pro-lincoln-orange)
               (markdown-header-face-7 :foreground ,dracula-pro-lincoln-alt-blue)
               (markdown-header-face-8 :foreground ,dracula-pro-lincoln-fg)
               (markdown-inline-code-face :foreground ,dracula-pro-lincoln-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,dracula-pro-lincoln-orange)
               (markdown-table-face :foreground ,dracula-pro-lincoln-purple)
               ;; message
               (message-mml :foreground ,dracula-pro-lincoln-green :weight normal)
               (message-header-xheader :foreground ,dracula-pro-lincoln-cyan :weight normal)
               ;; mode-line
               (mode-line :background ,dracula-pro-lincoln-current
                          :box ,dracula-pro-lincoln-current :inverse-video nil
                          ,@(if dracula-pro-lincoln-alternate-mode-line-and-minibuffer
                                (list :foreground dracula-pro-lincoln-fg3)
                              (list :foreground nil)))
               (mode-line-inactive
                :inverse-video nil
                ,@(if dracula-pro-lincoln-alternate-mode-line-and-minibuffer
                      (list :foreground dracula-pro-lincoln-comment :background dracula-pro-lincoln-bg
                            :box dracula-pro-lincoln-bg)
                    (list :foreground dracula-pro-lincoln-fg :background dracula-pro-lincoln-bg2 :box dracula-pro-lincoln-bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,dracula-pro-lincoln-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,dracula-pro-lincoln-purple)
               (mu4e-highlight-face :background ,dracula-pro-lincoln-bg
                                    :foreground ,dracula-pro-lincoln-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,dracula-pro-lincoln-current
                                           :foreground ,dracula-pro-lincoln-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,dracula-pro-lincoln-purple)
               (mu4e-cited-1-face :foreground ,dracula-pro-lincoln-purple)
               (mu4e-cited-2-face :foreground ,dracula-pro-lincoln-orange)
               (mu4e-cited-3-face :foreground ,dracula-pro-lincoln-comment)
               (mu4e-cited-4-face :foreground ,dracula-pro-lincoln-fg2)
               (mu4e-cited-5-face :foreground ,dracula-pro-lincoln-fg3)
               ;; org
               (org-agenda-date :foreground ,dracula-pro-lincoln-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,dracula-pro-lincoln-comment)
               (org-agenda-done :foreground ,dracula-pro-lincoln-green)
               (org-agenda-structure :foreground ,dracula-pro-lincoln-purple)
               (org-block :foreground ,dracula-pro-lincoln-orange)
               (org-code :foreground ,dracula-pro-lincoln-yellow)
               (org-column :background ,dracula-pro-lincoln-bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,dracula-pro-lincoln-cyan :underline t)
               (org-document-info :foreground ,dracula-pro-lincoln-alt-blue)
               (org-document-info-keyword :foreground ,dracula-pro-lincoln-comment)
               (org-document-title :weight bold :foreground ,dracula-pro-lincoln-orange
                                   ,@(when dracula-pro-lincoln-enlarge-headings
                                       (list :height dracula-pro-lincoln-height-doc-title)))
               (org-done :foreground ,dracula-pro-lincoln-green)
               (org-ellipsis :foreground ,dracula-pro-lincoln-comment)
               (org-footnote :foreground ,dracula-pro-lincoln-alt-blue)
               (org-formula :foreground ,dracula-pro-lincoln-pink)
               (org-headline-done :foreground ,dracula-pro-lincoln-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,dracula-pro-lincoln-bg :background ,dracula-pro-lincoln-bg)
               (org-level-1 :inherit bold :foreground ,dracula-pro-lincoln-pink
                            ,@(when dracula-pro-lincoln-enlarge-headings
                                (list :height dracula-pro-lincoln-height-title-1)))
               (org-level-2 :inherit bold :foreground ,dracula-pro-lincoln-purple
                            ,@(when dracula-pro-lincoln-enlarge-headings
                                (list :height dracula-pro-lincoln-height-title-2)))
               (org-level-3 :weight normal :foreground ,dracula-pro-lincoln-green
                            ,@(when dracula-pro-lincoln-enlarge-headings
                                (list :height dracula-pro-lincoln-height-title-3)))
               (org-level-4 :weight normal :foreground ,dracula-pro-lincoln-yellow)
               (org-level-5 :weight normal :foreground ,dracula-pro-lincoln-cyan)
               (org-level-6 :weight normal :foreground ,dracula-pro-lincoln-orange)
               (org-level-7 :weight normal :foreground ,dracula-pro-lincoln-alt-blue)
               (org-level-8 :weight normal :foreground ,dracula-pro-lincoln-fg)
               (org-link :foreground ,dracula-pro-lincoln-cyan :underline t)
               (org-priority :foreground ,dracula-pro-lincoln-cyan)
               (org-scheduled :foreground ,dracula-pro-lincoln-green)
               (org-scheduled-previously :foreground ,dracula-pro-lincoln-yellow)
               (org-scheduled-today :foreground ,dracula-pro-lincoln-green)
               (org-sexp-date :foreground ,dracula-pro-lincoln-fg4)
               (org-special-keyword :foreground ,dracula-pro-lincoln-yellow)
               (org-table :foreground ,dracula-pro-lincoln-purple)
               (org-tag :foreground ,dracula-pro-lincoln-pink :weight bold :background ,dracula-pro-lincoln-bg2)
               (org-todo :foreground ,dracula-pro-lincoln-orange :weight bold :background ,dracula-pro-lincoln-bg2)
               (org-upcoming-deadline :foreground ,dracula-pro-lincoln-yellow)
               (org-warning :weight bold :foreground ,dracula-pro-lincoln-pink)
               ;; outline
               (outline-1 :foreground ,dracula-pro-lincoln-pink)
               (outline-2 :foreground ,dracula-pro-lincoln-purple)
               (outline-3 :foreground ,dracula-pro-lincoln-green)
               (outline-4 :foreground ,dracula-pro-lincoln-yellow)
               (outline-5 :foreground ,dracula-pro-lincoln-cyan)
               (outline-6 :foreground ,dracula-pro-lincoln-orange)
               ;; powerline
               (powerline-evil-base-face :foreground ,dracula-pro-lincoln-bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,dracula-pro-lincoln-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,dracula-pro-lincoln-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,dracula-pro-lincoln-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,dracula-pro-lincoln-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,dracula-pro-lincoln-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,dracula-pro-lincoln-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,dracula-pro-lincoln-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,dracula-pro-lincoln-fg)
               (rainbow-delimiters-depth-2-face :foreground ,dracula-pro-lincoln-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,dracula-pro-lincoln-purple)
               (rainbow-delimiters-depth-4-face :foreground ,dracula-pro-lincoln-pink)
               (rainbow-delimiters-depth-5-face :foreground ,dracula-pro-lincoln-orange)
               (rainbow-delimiters-depth-6-face :foreground ,dracula-pro-lincoln-green)
               (rainbow-delimiters-depth-7-face :foreground ,dracula-pro-lincoln-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,dracula-pro-lincoln-alt-blue)
               (rainbow-delimiters-unmatched-face :foreground ,dracula-pro-lincoln-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,dracula-pro-lincoln-green)
               (rpm-spec-doc-face :foreground ,dracula-pro-lincoln-pink)
               (rpm-spec-ghost-face :foreground ,dracula-pro-lincoln-purple)
               (rpm-spec-macro-face :foreground ,dracula-pro-lincoln-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,dracula-pro-lincoln-purple)
               (rpm-spec-section-face :foreground ,dracula-pro-lincoln-yellow)
               (rpm-spec-tag-face :foreground ,dracula-pro-lincoln-cyan)
               (rpm-spec-var-face :foreground ,dracula-pro-lincoln-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,dracula-pro-lincoln-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,dracula-pro-lincoln-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,dracula-pro-lincoln-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,dracula-pro-lincoln-orange
                     :strike-through t :slant oblique)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,dracula-pro-lincoln-pink :background ,dracula-pro-lincoln-bg
                            :box (:line-width 2 :color ,dracula-pro-lincoln-bg :style nil))
               (tab-bar-tab-inactive :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-bg2
                                     :box (:line-width 2 :color ,dracula-pro-lincoln-bg2 :style nil))
               (tab-line :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,dracula-pro-lincoln-pink :background ,dracula-pro-lincoln-bg
                             :box (:line-width 2 :color ,dracula-pro-lincoln-bg :style nil))
               (tab-line-tab-inactive :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-bg2
                                      :box (:line-width 2 :color ,dracula-pro-lincoln-bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,dracula-pro-lincoln-red)
               ;; term
               (term :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-bg)
               (term-color-black :foreground ,dracula-pro-lincoln-bg :background ,dracula-pro-lincoln-bg)
               (term-color-blue :foreground ,dracula-pro-lincoln-purple :background ,dracula-pro-lincoln-purple)
               (term-color-cyan :foreground ,dracula-pro-lincoln-cyan :background ,dracula-pro-lincoln-cyan)
               (term-color-green :foreground ,dracula-pro-lincoln-green :background ,dracula-pro-lincoln-green)
               (term-color-magenta :foreground ,dracula-pro-lincoln-pink :background ,dracula-pro-lincoln-pink)
               (term-color-red :foreground ,dracula-pro-lincoln-red :background ,dracula-pro-lincoln-red)
               (term-color-white :foreground ,dracula-pro-lincoln-fg :background ,dracula-pro-lincoln-fg)
               (term-color-yellow :foreground ,dracula-pro-lincoln-yellow :background ,dracula-pro-lincoln-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,dracula-pro-lincoln-orange)
               (undo-tree-visualizer-default-face :foreground ,dracula-pro-lincoln-fg2)
               (undo-tree-visualizer-register-face :foreground ,dracula-pro-lincoln-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,dracula-pro-lincoln-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,dracula-pro-lincoln-purple)
               (web-mode-html-attr-value-face :foreground ,dracula-pro-lincoln-green)
               (web-mode-html-tag-face :foreground ,dracula-pro-lincoln-pink :weight bold)
               (web-mode-keyword-face :foreground ,dracula-pro-lincoln-pink)
               (web-mode-string-face :foreground ,dracula-pro-lincoln-yellow)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,dracula-pro-lincoln-red :foreground ,dracula-pro-lincoln-red)
               (whitespace-empty :background ,dracula-pro-lincoln-orange :foreground ,dracula-pro-lincoln-red)
               (whitespace-hspace :background ,dracula-pro-lincoln-bg3 :foreground ,dracula-pro-lincoln-comment)
               (whitespace-indentation :background ,dracula-pro-lincoln-orange :foreground ,dracula-pro-lincoln-red)
               (whitespace-line :background ,dracula-pro-lincoln-bg :foreground ,dracula-pro-lincoln-pink)
               (whitespace-newline :foreground ,dracula-pro-lincoln-comment)
               (whitespace-space :background ,dracula-pro-lincoln-bg :foreground ,dracula-pro-lincoln-comment)
               (whitespace-space-after-tab :background ,dracula-pro-lincoln-orange :foreground ,dracula-pro-lincoln-red)
               (whitespace-space-before-tab :background ,dracula-pro-lincoln-orange :foreground ,dracula-pro-lincoln-red)
               (whitespace-tab :background ,dracula-pro-lincoln-bg2 :foreground ,dracula-pro-lincoln-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'dracula-pro-lincoln
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

(provide-theme 'dracula-pro-lincoln)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; dracula-pro-lincoln-theme.el ends here