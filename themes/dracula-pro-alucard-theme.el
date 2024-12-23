;;; dracula-pro-alucard-theme.el --- Dracula Pro

;; Copyright (C) 2020-Today Dracula Theme.

;; Author: Dracula Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://draculatheme.com/pro

;;; Commentary:
;; Dracula PRO is a color scheme and UI theme tailored for programming. Made for terminal emulators, code editors, and syntax highlighters.

;;; Code:

(require 'cl-lib)
(deftheme dracula-pro-alucard
  "Dracula PRO - Alucard Variant")


;;;; Configuration options:

(defgroup dracula-pro-alucard nil
  "Dracula theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dracula-pro-alucard-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'dracula-pro-alucard)

(defcustom dracula-pro-alucard-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'dracula-pro-alucard)

(defcustom dracula-pro-alucard-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'dracula-pro-alucard)

(defcustom dracula-pro-alucard-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'dracula-pro-alucard)

(defcustom dracula-pro-alucard-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'dracula-pro-alucard)

(defcustom dracula-pro-alucard-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula-pro-alucard)


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [TTY-COLOR]
(let ((colors '(;; Upstream theme color
                (dracula-pro-alucard-bg       "#F5F5F5" "#F5F5F5" nil)             ; Background
                (dracula-pro-alucard-fg       "#1F1F1F" "#1F1F1F" "brightwhite")   ; Foreground
                (dracula-pro-alucard-current  "#CFCFDE" "#CFCFDE" "brightblack")   ; Current-line/selection
                (dracula-pro-alucard-comment  "#635D97" "#635D97" "blue")          ; Comment
                (dracula-pro-alucard-cyan     "#036A96" "#036A96" "brightcyan")    ; Cyan
                (dracula-pro-alucard-green    "#14710A" "#14710A" "green")         ; Green
                (dracula-pro-alucard-orange   "#A34D14" "#A34D14" "brightred")     ; Orange
                (dracula-pro-alucard-pink     "#A3144D" "#A3144D" "magenta")       ; Pink
                (dracula-pro-alucard-purple   "#644AC9" "#644AC9" "brightmagenta") ; Purple
                (dracula-pro-alucard-red      "#CB3A2A" "#CB3A2A" "red")           ; Red
                (dracula-pro-alucard-yellow   "#846E15" "#846E15" "yellow")        ; Yellow
                ;; Other colors
                (dracula-pro-alucard-bg2      "#E5E5E5" "#E5E5E5" "brightblack")
                (dracula-pro-alucard-bg3      "#D5D5D5" "#D5D5D5" "brightblack")
                (dracula-pro-alucard-bg4      "#C5C5C5" "#C5C5C5" "brightblack")
                (dracula-pro-alucard-fg2      "#2F2F2F" "#2F2F2F" "brightwhite")
                (dracula-pro-alucard-fg3      "#3F3F3F" "#3F3F3F" "white")
                (dracula-pro-alucard-fg4      "#4F4F4F" "#4F4F4F" "white")
                (dracula-pro-alucard-alt-blue "#336699" "#336699" "brightblue")))
      (faces '(;; default
               (cursor :background ,dracula-pro-alucard-fg3)
               (completions-first-difference :foreground ,dracula-pro-alucard-pink :weight bold)
               (default :background ,dracula-pro-alucard-bg :foreground ,dracula-pro-alucard-fg)
               (default-italic :slant italic)
               (ffap :foreground ,dracula-pro-alucard-fg4)
               (fringe :background ,dracula-pro-alucard-bg :foreground ,dracula-pro-alucard-fg4)
               (highlight :foreground ,dracula-pro-alucard-fg3 :background ,dracula-pro-alucard-bg3)
               (hl-line :background ,dracula-pro-alucard-current :extend t)
               (info-quoted-name :foreground ,dracula-pro-alucard-orange)
               (info-string :foreground ,dracula-pro-alucard-yellow)
               (lazy-highlight :foreground ,dracula-pro-alucard-fg2 :background ,dracula-pro-alucard-bg2)
               (link :foreground ,dracula-pro-alucard-cyan :underline t)
               (linum :slant italic :foreground ,dracula-pro-alucard-bg4 :background ,dracula-pro-alucard-bg)
               (line-number :slant italic :foreground ,dracula-pro-alucard-bg4 :background ,dracula-pro-alucard-bg)
               (match :background ,dracula-pro-alucard-yellow :foreground ,dracula-pro-alucard-bg)
               (minibuffer-prompt
                ,@(if dracula-pro-alucard-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-alucard-fg)
                    (list :weight 'bold :foreground dracula-pro-alucard-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground nil :background ,dracula-pro-alucard-orange)
               (vertical-border :foreground ,dracula-pro-alucard-bg2)
               (success :foreground ,dracula-pro-alucard-green)
               (warning :foreground ,dracula-pro-alucard-orange)
               (error :foreground ,dracula-pro-alucard-red)
               (header-line :background ,dracula-pro-alucard-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,dracula-pro-alucard-orange)
               (font-lock-comment-face :foreground ,dracula-pro-alucard-comment)
               (font-lock-comment-delimiter-face :foreground ,dracula-pro-alucard-comment)
               (font-lock-constant-face :foreground ,dracula-pro-alucard-cyan)
               (font-lock-doc-face :foreground ,dracula-pro-alucard-comment)
               (font-lock-function-name-face :foreground ,dracula-pro-alucard-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,dracula-pro-alucard-pink)
               (font-lock-negation-char-face :foreground ,dracula-pro-alucard-cyan)
               (font-lock-preprocessor-face :foreground ,dracula-pro-alucard-orange)
               (font-lock-reference-face :foreground ,dracula-pro-alucard-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,dracula-pro-alucard-cyan)
               (font-lock-regexp-grouping-construct :foreground ,dracula-pro-alucard-purple)
               (font-lock-string-face :foreground ,dracula-pro-alucard-yellow)
               (font-lock-type-face :foreground ,dracula-pro-alucard-purple)
               (font-lock-variable-name-face :foreground ,dracula-pro-alucard-fg
                                             :weight bold)
               (font-lock-warning-face :foreground ,dracula-pro-alucard-orange :background ,dracula-pro-alucard-bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,dracula-pro-alucard-pink)
               ;; company
               (company-echo-common :foreground ,dracula-pro-alucard-bg :background ,dracula-pro-alucard-fg)
               (company-preview :background ,dracula-pro-alucard-bg :foreground ,dracula-pro-alucard-alt-blue)
               (company-preview-common :foreground ,dracula-pro-alucard-bg2 :foreground ,dracula-pro-alucard-fg3)
               (company-preview-search :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-bg)
               (company-scrollbar-bg :background ,dracula-pro-alucard-bg3)
               (company-scrollbar-fg :foreground ,dracula-pro-alucard-pink)
               (company-template-field :inherit match)
               (company-tooltip :foreground ,dracula-pro-alucard-fg2 :background ,dracula-pro-alucard-bg :weight bold)
               (company-tooltip-annotation :foreground ,dracula-pro-alucard-cyan)
               (company-tooltip-common :foreground ,dracula-pro-alucard-fg3)
               (company-tooltip-common-selection :foreground ,dracula-pro-alucard-yellow)
               (company-tooltip-mouse :inherit highlight)
               (company-tooltip-selection :background ,dracula-pro-alucard-bg3 :foreground ,dracula-pro-alucard-fg3)
               ;; diff-hl
               (diff-hl-change :foreground ,dracula-pro-alucard-orange :background ,dracula-pro-alucard-orange)
               (diff-hl-delete :foreground ,dracula-pro-alucard-red :background ,dracula-pro-alucard-red)
               (diff-hl-insert :foreground ,dracula-pro-alucard-green :background ,dracula-pro-alucard-green)
               ;; dired
               (dired-directory :foreground ,dracula-pro-alucard-green :weight normal)
               (dired-flagged :foreground ,dracula-pro-alucard-pink)
               (dired-header :foreground ,dracula-pro-alucard-fg3 :background ,dracula-pro-alucard-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,dracula-pro-alucard-fg :weight bold)
               (dired-marked :foreground ,dracula-pro-alucard-orange :weight bold)
               (dired-perm-write :foreground ,dracula-pro-alucard-fg3 :underline t)
               (dired-symlink :foreground ,dracula-pro-alucard-yellow :weight normal :slant italic)
               (dired-warning :foreground ,dracula-pro-alucard-orange :underline t)
               (diredp-compressed-file-name :foreground ,dracula-pro-alucard-fg3)
               (diredp-compressed-file-suffix :foreground ,dracula-pro-alucard-fg4)
               (diredp-date-time :foreground ,dracula-pro-alucard-fg)
               (diredp-deletion-file-name :foreground ,dracula-pro-alucard-pink :background ,dracula-pro-alucard-current)
               (diredp-deletion :foreground ,dracula-pro-alucard-pink :weight bold)
               (diredp-dir-heading :foreground ,dracula-pro-alucard-fg2 :background ,dracula-pro-alucard-bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,dracula-pro-alucard-orange)
               (diredp-file-name :foreground ,dracula-pro-alucard-fg)
               (diredp-file-suffix :foreground ,dracula-pro-alucard-fg4)
               (diredp-flag-mark-line :foreground ,dracula-pro-alucard-fg2 :slant italic :background ,dracula-pro-alucard-current)
               (diredp-flag-mark :foreground ,dracula-pro-alucard-fg2 :weight bold :background ,dracula-pro-alucard-current)
               (diredp-ignored-file-name :foreground ,dracula-pro-alucard-fg)
               (diredp-mode-line-flagged :foreground ,dracula-pro-alucard-orange)
               (diredp-mode-line-marked :foreground ,dracula-pro-alucard-orange)
               (diredp-no-priv :foreground ,dracula-pro-alucard-fg)
               (diredp-number :foreground ,dracula-pro-alucard-cyan)
               (diredp-other-priv :foreground ,dracula-pro-alucard-orange)
               (diredp-rare-priv :foreground ,dracula-pro-alucard-orange)
               (diredp-read-priv :foreground ,dracula-pro-alucard-purple)
               (diredp-write-priv :foreground ,dracula-pro-alucard-pink)
               (diredp-exec-priv :foreground ,dracula-pro-alucard-yellow)
               (diredp-symlink :foreground ,dracula-pro-alucard-orange)
               (diredp-link-priv :foreground ,dracula-pro-alucard-orange)
               (diredp-autofile-name :foreground ,dracula-pro-alucard-yellow)
               (diredp-tagged-autofile-name :foreground ,dracula-pro-alucard-yellow)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,dracula-pro-alucard-yellow)
               (enh-ruby-op-face :foreground ,dracula-pro-alucard-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,dracula-pro-alucard-yellow)
               (enh-ruby-string-delimiter-face :foreground ,dracula-pro-alucard-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,dracula-pro-alucard-orange))
               (flyspell-incorrect :underline (:style wave :color ,dracula-pro-alucard-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,dracula-pro-alucard-purple)
               (font-latex-italic-face :foreground ,dracula-pro-alucard-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,dracula-pro-alucard-cyan)
               (font-latex-match-variable-keywords :foreground ,dracula-pro-alucard-fg)
               (font-latex-string-face :foreground ,dracula-pro-alucard-yellow)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,dracula-pro-alucard-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,dracula-pro-alucard-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,dracula-pro-alucard-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,dracula-pro-alucard-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,dracula-pro-alucard-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,dracula-pro-alucard-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,dracula-pro-alucard-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,dracula-pro-alucard-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,dracula-pro-alucard-pink)
               (gnus-header-from :foreground ,dracula-pro-alucard-fg)
               (gnus-header-name :foreground ,dracula-pro-alucard-purple)
               (gnus-header-subject :foreground ,dracula-pro-alucard-green :weight bold)
               (gnus-summary-markup-face :foreground ,dracula-pro-alucard-cyan)
               (gnus-summary-high-unread :foreground ,dracula-pro-alucard-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,dracula-pro-alucard-alt-blue :weight bold)
               (gnus-summary-normal-read :foreground ,dracula-pro-alucard-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,dracula-pro-alucard-pink :weight bold)
               (gnus-summary-low-unread :foreground ,dracula-pro-alucard-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,dracula-pro-alucard-pink)
               (haskell-constructor-face :foreground ,dracula-pro-alucard-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,dracula-pro-alucard-purple)
               (helm-buffer-not-saved :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-bg)
               (helm-buffer-process :foreground ,dracula-pro-alucard-orange :background ,dracula-pro-alucard-bg)
               (helm-buffer-saved-out :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-bg)
               (helm-buffer-size :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-bg)
               (helm-candidate-number :foreground ,dracula-pro-alucard-bg :background ,dracula-pro-alucard-fg)
               (helm-ff-directory :foreground ,dracula-pro-alucard-green :background ,dracula-pro-alucard-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,dracula-pro-alucard-green :background ,dracula-pro-alucard-bg :weight normal)
               (helm-ff-executable :foreground ,dracula-pro-alucard-alt-blue :background ,dracula-pro-alucard-bg :weight normal)
               (helm-ff-file :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,dracula-pro-alucard-pink :background ,dracula-pro-alucard-bg :weight bold)
               (helm-ff-prefix :foreground ,dracula-pro-alucard-bg :background ,dracula-pro-alucard-pink :weight normal)
               (helm-ff-symlink :foreground ,dracula-pro-alucard-pink :background ,dracula-pro-alucard-bg :weight bold)
               (helm-grep-cmd-line :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-bg)
               (helm-grep-file :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-bg)
               (helm-grep-finish :foreground ,dracula-pro-alucard-fg2 :background ,dracula-pro-alucard-bg)
               (helm-grep-lineno :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-bg)
               (helm-grep-match :foreground nil :background nil :inherit helm-match)
               (helm-grep-running :foreground ,dracula-pro-alucard-green :background ,dracula-pro-alucard-bg)
               (helm-header :foreground ,dracula-pro-alucard-fg2 :background ,dracula-pro-alucard-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,dracula-pro-alucard-green :background ,dracula-pro-alucard-bg)
               (helm-selection :background ,dracula-pro-alucard-bg2 :underline nil)
               (helm-selection-line :background ,dracula-pro-alucard-bg2)
               (helm-separator :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-bg)
               (helm-source-go-package-godoc-description :foreground ,dracula-pro-alucard-yellow)
               (helm-source-header :foreground ,dracula-pro-alucard-pink :background ,dracula-pro-alucard-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,dracula-pro-alucard-orange :background ,dracula-pro-alucard-bg)
               (helm-time-zone-home :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-bg)
               (helm-visible-mark :foreground ,dracula-pro-alucard-bg :background ,dracula-pro-alucard-bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,dracula-pro-alucard-bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,dracula-pro-alucard-fg)
               (icicle-special-candidate :foreground ,dracula-pro-alucard-fg2)
               (icicle-extra-candidate :foreground ,dracula-pro-alucard-fg2)
               (icicle-search-main-regexp-others :foreground ,dracula-pro-alucard-fg)
               (icicle-search-current-input :foreground ,dracula-pro-alucard-pink)
               (icicle-search-context-level-8 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-context-level-7 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-context-level-6 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-context-level-5 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-context-level-4 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-context-level-3 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-context-level-2 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-context-level-1 :foreground ,dracula-pro-alucard-orange)
               (icicle-search-main-regexp-current :foreground ,dracula-pro-alucard-fg)
               (icicle-saved-candidate :foreground ,dracula-pro-alucard-fg)
               (icicle-proxy-candidate :foreground ,dracula-pro-alucard-fg)
               (icicle-mustmatch-completion :foreground ,dracula-pro-alucard-purple)
               (icicle-multi-command-completion :foreground ,dracula-pro-alucard-fg2 :background ,dracula-pro-alucard-bg2)
               (icicle-msg-emphasis :foreground ,dracula-pro-alucard-green)
               (icicle-mode-line-help :foreground ,dracula-pro-alucard-fg4)
               (icicle-match-highlight-minibuffer :foreground ,dracula-pro-alucard-orange)
               (icicle-match-highlight-Completions :foreground ,dracula-pro-alucard-green)
               (icicle-key-complete-menu-local :foreground ,dracula-pro-alucard-fg)
               (icicle-key-complete-menu :foreground ,dracula-pro-alucard-fg)
               (icicle-input-completion-fail-lax :foreground ,dracula-pro-alucard-pink)
               (icicle-input-completion-fail :foreground ,dracula-pro-alucard-pink)
               (icicle-historical-candidate-other :foreground ,dracula-pro-alucard-fg)
               (icicle-historical-candidate :foreground ,dracula-pro-alucard-fg)
               (icicle-current-candidate-highlight :foreground ,dracula-pro-alucard-orange :background ,dracula-pro-alucard-bg3)
               (icicle-Completions-instruction-2 :foreground ,dracula-pro-alucard-fg4)
               (icicle-Completions-instruction-1 :foreground ,dracula-pro-alucard-fg4)
               (icicle-completion :foreground ,dracula-pro-alucard-fg)
               (icicle-complete-input :foreground ,dracula-pro-alucard-orange)
               (icicle-common-match-highlight-Completions :foreground ,dracula-pro-alucard-purple)
               (icicle-candidate-part :foreground ,dracula-pro-alucard-fg)
               (icicle-annotation :foreground ,dracula-pro-alucard-fg4)
               ;; icomplete
               (icompletep-determined :foreground ,dracula-pro-alucard-orange)
               ;; ido
               (ido-first-match
                ,@(if dracula-pro-alucard-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-alucard-green)
                    (list :weight 'bold :foreground dracula-pro-alucard-pink)))
               (ido-only-match :foreground ,dracula-pro-alucard-orange)
               (ido-subdir :foreground ,dracula-pro-alucard-yellow)
               (ido-virtual :foreground ,dracula-pro-alucard-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,dracula-pro-alucard-bg :background ,dracula-pro-alucard-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,dracula-pro-alucard-cyan)
               (jde-java-font-lock-modifier-face :foreground ,dracula-pro-alucard-pink)
               (jde-java-font-lock-number-face :foreground ,dracula-pro-alucard-fg)
               (jde-java-font-lock-package-face :foreground ,dracula-pro-alucard-fg)
               (jde-java-font-lock-private-face :foreground ,dracula-pro-alucard-pink)
               (jde-java-font-lock-public-face :foreground ,dracula-pro-alucard-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,dracula-pro-alucard-purple)
               (js2-function-param :foreground ,dracula-pro-alucard-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,dracula-pro-alucard-yellow)
               (js2-jsdoc-html-tag-name :foreground ,dracula-pro-alucard-alt-blue)
               (js2-jsdoc-value :foreground ,dracula-pro-alucard-yellow)
               (js2-private-function-call :foreground ,dracula-pro-alucard-cyan)
               (js2-private-member :foreground ,dracula-pro-alucard-fg3)
               ;; js3-mode
               (js3-error-face :underline ,dracula-pro-alucard-orange)
               (js3-external-variable-face :foreground ,dracula-pro-alucard-fg)
               (js3-function-param-face :foreground ,dracula-pro-alucard-pink)
               (js3-instance-member-face :foreground ,dracula-pro-alucard-cyan)
               (js3-jsdoc-tag-face :foreground ,dracula-pro-alucard-pink)
               (js3-warning-face :underline ,dracula-pro-alucard-pink)
               ;; magit
               (magit-branch-local :foreground ,dracula-pro-alucard-cyan)
               (magit-branch-remote :foreground ,dracula-pro-alucard-green)
               (magit-tag :foreground ,dracula-pro-alucard-orange)
               (magit-section-heading :foreground ,dracula-pro-alucard-pink :weight bold)
               (magit-section-highlight :background ,dracula-pro-alucard-bg3 :extend t)
               (magit-diff-context-highlight :background ,dracula-pro-alucard-bg3
                                             :foreground ,dracula-pro-alucard-fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,dracula-pro-alucard-orange
                                            :background ,dracula-pro-alucard-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,dracula-pro-alucard-orange
                                                      :background ,dracula-pro-alucard-bg3
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
               (magit-diff-file-heading :foreground ,dracula-pro-alucard-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,dracula-pro-alucard-green)
               (magit-diffstat-removed :foreground ,dracula-pro-alucard-red)
               (magit-hash :foreground ,dracula-pro-alucard-fg2)
               (magit-hunk-heading :background ,dracula-pro-alucard-bg3)
               (magit-hunk-heading-highlight :background ,dracula-pro-alucard-bg3)
               (magit-item-highlight :background ,dracula-pro-alucard-bg3)
               (magit-log-author :foreground ,dracula-pro-alucard-fg3)
               (magit-process-ng :foreground ,dracula-pro-alucard-orange :weight bold)
               (magit-process-ok :foreground ,dracula-pro-alucard-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,dracula-pro-alucard-orange)
               (markdown-code-face :foreground ,dracula-pro-alucard-orange)
               (markdown-footnote-face :foreground ,dracula-pro-alucard-alt-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,dracula-pro-alucard-pink
                ,@(when dracula-pro-alucard-enlarge-headings
                    (list :height dracula-pro-alucard-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,dracula-pro-alucard-purple
                ,@(when dracula-pro-alucard-enlarge-headings
                    (list :height dracula-pro-alucard-height-title-2)))
               (markdown-header-face-3
                :foreground ,dracula-pro-alucard-green
                ,@(when dracula-pro-alucard-enlarge-headings
                    (list :height dracula-pro-alucard-height-title-3)))
               (markdown-header-face-4 :foreground ,dracula-pro-alucard-yellow)
               (markdown-header-face-5 :foreground ,dracula-pro-alucard-cyan)
               (markdown-header-face-6 :foreground ,dracula-pro-alucard-orange)
               (markdown-header-face-7 :foreground ,dracula-pro-alucard-alt-blue)
               (markdown-header-face-8 :foreground ,dracula-pro-alucard-fg)
               (markdown-inline-code-face :foreground ,dracula-pro-alucard-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,dracula-pro-alucard-orange)
               (markdown-table-face :foreground ,dracula-pro-alucard-purple)
               ;; message
               (message-mml :foreground ,dracula-pro-alucard-green :weight normal)
               (message-header-xheader :foreground ,dracula-pro-alucard-cyan :weight normal)
               ;; mode-line
               (mode-line :background ,dracula-pro-alucard-current
                          :box ,dracula-pro-alucard-current :inverse-video nil
                          ,@(if dracula-pro-alucard-alternate-mode-line-and-minibuffer
                                (list :foreground dracula-pro-alucard-fg3)
                              (list :foreground nil)))
               (mode-line-inactive
                :inverse-video nil
                ,@(if dracula-pro-alucard-alternate-mode-line-and-minibuffer
                      (list :foreground dracula-pro-alucard-comment :background dracula-pro-alucard-bg
                            :box dracula-pro-alucard-bg)
                    (list :foreground dracula-pro-alucard-fg :background dracula-pro-alucard-bg2 :box dracula-pro-alucard-bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,dracula-pro-alucard-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,dracula-pro-alucard-purple)
               (mu4e-highlight-face :background ,dracula-pro-alucard-bg
                                    :foreground ,dracula-pro-alucard-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,dracula-pro-alucard-current
                                           :foreground ,dracula-pro-alucard-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,dracula-pro-alucard-purple)
               (mu4e-cited-1-face :foreground ,dracula-pro-alucard-purple)
               (mu4e-cited-2-face :foreground ,dracula-pro-alucard-orange)
               (mu4e-cited-3-face :foreground ,dracula-pro-alucard-comment)
               (mu4e-cited-4-face :foreground ,dracula-pro-alucard-fg2)
               (mu4e-cited-5-face :foreground ,dracula-pro-alucard-fg3)
               ;; org
               (org-agenda-date :foreground ,dracula-pro-alucard-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,dracula-pro-alucard-comment)
               (org-agenda-done :foreground ,dracula-pro-alucard-green)
               (org-agenda-structure :foreground ,dracula-pro-alucard-purple)
               (org-block :foreground ,dracula-pro-alucard-orange)
               (org-code :foreground ,dracula-pro-alucard-yellow)
               (org-column :background ,dracula-pro-alucard-bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,dracula-pro-alucard-cyan :underline t)
               (org-document-info :foreground ,dracula-pro-alucard-alt-blue)
               (org-document-info-keyword :foreground ,dracula-pro-alucard-comment)
               (org-document-title :weight bold :foreground ,dracula-pro-alucard-orange
                                   ,@(when dracula-pro-alucard-enlarge-headings
                                       (list :height dracula-pro-alucard-height-doc-title)))
               (org-done :foreground ,dracula-pro-alucard-green)
               (org-ellipsis :foreground ,dracula-pro-alucard-comment)
               (org-footnote :foreground ,dracula-pro-alucard-alt-blue)
               (org-formula :foreground ,dracula-pro-alucard-pink)
               (org-headline-done :foreground ,dracula-pro-alucard-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,dracula-pro-alucard-bg :background ,dracula-pro-alucard-bg)
               (org-level-1 :inherit bold :foreground ,dracula-pro-alucard-pink
                            ,@(when dracula-pro-alucard-enlarge-headings
                                (list :height dracula-pro-alucard-height-title-1)))
               (org-level-2 :inherit bold :foreground ,dracula-pro-alucard-purple
                            ,@(when dracula-pro-alucard-enlarge-headings
                                (list :height dracula-pro-alucard-height-title-2)))
               (org-level-3 :weight normal :foreground ,dracula-pro-alucard-green
                            ,@(when dracula-pro-alucard-enlarge-headings
                                (list :height dracula-pro-alucard-height-title-3)))
               (org-level-4 :weight normal :foreground ,dracula-pro-alucard-yellow)
               (org-level-5 :weight normal :foreground ,dracula-pro-alucard-cyan)
               (org-level-6 :weight normal :foreground ,dracula-pro-alucard-orange)
               (org-level-7 :weight normal :foreground ,dracula-pro-alucard-alt-blue)
               (org-level-8 :weight normal :foreground ,dracula-pro-alucard-fg)
               (org-link :foreground ,dracula-pro-alucard-cyan :underline t)
               (org-priority :foreground ,dracula-pro-alucard-cyan)
               (org-scheduled :foreground ,dracula-pro-alucard-green)
               (org-scheduled-previously :foreground ,dracula-pro-alucard-yellow)
               (org-scheduled-today :foreground ,dracula-pro-alucard-green)
               (org-sexp-date :foreground ,dracula-pro-alucard-fg4)
               (org-special-keyword :foreground ,dracula-pro-alucard-yellow)
               (org-table :foreground ,dracula-pro-alucard-purple)
               (org-tag :foreground ,dracula-pro-alucard-pink :weight bold :background ,dracula-pro-alucard-bg2)
               (org-todo :foreground ,dracula-pro-alucard-orange :weight bold :background ,dracula-pro-alucard-bg2)
               (org-upcoming-deadline :foreground ,dracula-pro-alucard-yellow)
               (org-warning :weight bold :foreground ,dracula-pro-alucard-pink)
               ;; outline
               (outline-1 :foreground ,dracula-pro-alucard-pink)
               (outline-2 :foreground ,dracula-pro-alucard-purple)
               (outline-3 :foreground ,dracula-pro-alucard-green)
               (outline-4 :foreground ,dracula-pro-alucard-yellow)
               (outline-5 :foreground ,dracula-pro-alucard-cyan)
               (outline-6 :foreground ,dracula-pro-alucard-orange)
               ;; powerline
               (powerline-evil-base-face :foreground ,dracula-pro-alucard-bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,dracula-pro-alucard-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,dracula-pro-alucard-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,dracula-pro-alucard-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,dracula-pro-alucard-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,dracula-pro-alucard-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,dracula-pro-alucard-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,dracula-pro-alucard-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,dracula-pro-alucard-fg)
               (rainbow-delimiters-depth-2-face :foreground ,dracula-pro-alucard-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,dracula-pro-alucard-purple)
               (rainbow-delimiters-depth-4-face :foreground ,dracula-pro-alucard-pink)
               (rainbow-delimiters-depth-5-face :foreground ,dracula-pro-alucard-orange)
               (rainbow-delimiters-depth-6-face :foreground ,dracula-pro-alucard-green)
               (rainbow-delimiters-depth-7-face :foreground ,dracula-pro-alucard-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,dracula-pro-alucard-alt-blue)
               (rainbow-delimiters-unmatched-face :foreground ,dracula-pro-alucard-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,dracula-pro-alucard-green)
               (rpm-spec-doc-face :foreground ,dracula-pro-alucard-pink)
               (rpm-spec-ghost-face :foreground ,dracula-pro-alucard-purple)
               (rpm-spec-macro-face :foreground ,dracula-pro-alucard-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,dracula-pro-alucard-purple)
               (rpm-spec-section-face :foreground ,dracula-pro-alucard-yellow)
               (rpm-spec-tag-face :foreground ,dracula-pro-alucard-cyan)
               (rpm-spec-var-face :foreground ,dracula-pro-alucard-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,dracula-pro-alucard-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,dracula-pro-alucard-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,dracula-pro-alucard-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,dracula-pro-alucard-orange
                     :strike-through t :slant oblique)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,dracula-pro-alucard-pink :background ,dracula-pro-alucard-bg
                            :box (:line-width 2 :color ,dracula-pro-alucard-bg :style nil))
               (tab-bar-tab-inactive :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-bg2
                                     :box (:line-width 2 :color ,dracula-pro-alucard-bg2 :style nil))
               (tab-line :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,dracula-pro-alucard-pink :background ,dracula-pro-alucard-bg
                             :box (:line-width 2 :color ,dracula-pro-alucard-bg :style nil))
               (tab-line-tab-inactive :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-bg2
                                      :box (:line-width 2 :color ,dracula-pro-alucard-bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,dracula-pro-alucard-red)
               ;; term
               (term :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-bg)
               (term-color-black :foreground ,dracula-pro-alucard-bg :background ,dracula-pro-alucard-bg)
               (term-color-blue :foreground ,dracula-pro-alucard-purple :background ,dracula-pro-alucard-purple)
               (term-color-cyan :foreground ,dracula-pro-alucard-cyan :background ,dracula-pro-alucard-cyan)
               (term-color-green :foreground ,dracula-pro-alucard-green :background ,dracula-pro-alucard-green)
               (term-color-magenta :foreground ,dracula-pro-alucard-pink :background ,dracula-pro-alucard-pink)
               (term-color-red :foreground ,dracula-pro-alucard-red :background ,dracula-pro-alucard-red)
               (term-color-white :foreground ,dracula-pro-alucard-fg :background ,dracula-pro-alucard-fg)
               (term-color-yellow :foreground ,dracula-pro-alucard-yellow :background ,dracula-pro-alucard-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,dracula-pro-alucard-orange)
               (undo-tree-visualizer-default-face :foreground ,dracula-pro-alucard-fg2)
               (undo-tree-visualizer-register-face :foreground ,dracula-pro-alucard-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,dracula-pro-alucard-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,dracula-pro-alucard-purple)
               (web-mode-html-attr-value-face :foreground ,dracula-pro-alucard-green)
               (web-mode-html-tag-face :foreground ,dracula-pro-alucard-pink :weight bold)
               (web-mode-keyword-face :foreground ,dracula-pro-alucard-pink)
               (web-mode-string-face :foreground ,dracula-pro-alucard-yellow)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,dracula-pro-alucard-red :foreground ,dracula-pro-alucard-red)
               (whitespace-empty :background ,dracula-pro-alucard-orange :foreground ,dracula-pro-alucard-red)
               (whitespace-hspace :background ,dracula-pro-alucard-bg3 :foreground ,dracula-pro-alucard-comment)
               (whitespace-indentation :background ,dracula-pro-alucard-orange :foreground ,dracula-pro-alucard-red)
               (whitespace-line :background ,dracula-pro-alucard-bg :foreground ,dracula-pro-alucard-pink)
               (whitespace-newline :foreground ,dracula-pro-alucard-comment)
               (whitespace-space :background ,dracula-pro-alucard-bg :foreground ,dracula-pro-alucard-comment)
               (whitespace-space-after-tab :background ,dracula-pro-alucard-orange :foreground ,dracula-pro-alucard-red)
               (whitespace-space-before-tab :background ,dracula-pro-alucard-orange :foreground ,dracula-pro-alucard-red)
               (whitespace-tab :background ,dracula-pro-alucard-bg2 :foreground ,dracula-pro-alucard-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'dracula-pro-alucard
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

(provide-theme 'dracula-pro-alucard)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; dracula-pro-alucard-theme.el ends here