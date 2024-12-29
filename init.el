;;; -*- no-byte-compile: t -*-
;;; -*- lexical-binding: t -*-
;;; init.el --- My personal configuration file.

(require 'package)

;; Personal information
(setq user-full-name "Abizer Nasir"
      user-mail-address "abizern@mac.com"
      user-domain "abizern.dev")

;; Move files to trash
(setq delete-by-moving-to-trash t)

;; Save files before compiling
(setq compilation-ask-about-save nil)

;; Turn off defadvice warnings during startup
(setq ad-redefinition-action 'accept)

;; Get the current directory by looking for the directory that this file is in.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name)))) 

;; Directory for support files. Create if needed.
(defvar savefile-dir (expand-file-name "savefile" dotfiles-dir)
  "This directory stores support files.")
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; User lisp files. Create if needed.
(defvar ygg-lisp (expand-file-name "lisp" dotfiles-dir)
  "This directory stores the user lisp files.")
(unless (file-exists-p ygg-lisp)
  (make-directory ygg-lisp))
;; Add the user-lisp directory to the load path.
(add-to-list 'load-path ygg-lisp)

;; Keep downloaded packages organised.
(setq package-user-dir (expand-file-name "elpa" dotfiles-dir))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 63)
                                                      ("nongnu" . 10)
                                                      ("org" . 99)
                                                      ("stable" . 80)
                                                      ("melpa"  . 90)))

;; To get the package manager going, we invoke its initialise function.
(package-initialize)

;; Update package metadata if required
(unless package-archive-contents
  (package-refresh-contents))

;; The use-package module has made using packages so much better
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; For more verbose startup, uncomment the line below
;; (setq use-package-verbose t)

(use-package f :ensure t)

;; Reduces the frequency of garbage collection by making it happen on
;; each 25MB of allocated data (the default is on every 0.76MB).
(setq gc-cons-threshold 25000000)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; Turn off modes that look ugly.
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))

;; We don't need a startup message.
(setq inhibit-startup-message t)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; A suitably wide fill-column
(set-default 'fill-column 100)

;; Show column and line number in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Tab-bar-mode
(tab-bar-mode 1)
(setq tab-bar-show 1)
(setq tab-bar-new-tab-choice "*scratch*")


;; Themes

;; Use Tron-legacy theme
;; (defun setup-tron-theme (frame)
;;   "Load the Theme from a hook and then remove the hook"
;;   (progn
;;     (load-theme 'tron-legacy t)
;;     (remove-hook 'after-make-frame-functions 'setup-tron-theme)))

;; (use-package tron-legacy-theme
;;   :ensure t
;;   :config
;;   (add-hook 'after-make-frame-functions 'setup-tron-theme t)
;;   (add-hook 'after-make-frame-functions 'setup-gold-cursor t)
;;   (setq-default cursor-type 'bar))

(setq-default cursor-type 'bar)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(progn
  (load-theme 'dracula-pro-pro :no-confirm)
  (add-hook 'after-make-frame-functions 'setup-gold-cursor t))

;; Use Zenburn as a theme
;; Helper functions to load the theme in the correct way
;; (defun setup-zenburn-theme (frame)
;;   "Load the theme from a hook then remove the hook"
;;   (progn
;;     (load-theme 'zenburn t)
;;     (remove-hook 'after-make-frame-functions 'setup-zenburn-theme)))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (add-hook 'after-make-frame-functions 'setup-zenburn-theme t)
;;   (add-hook 'after-make-frame-functions 'setup-gold-cursor t)
;;   (setq-default cursor-type 'bar))

;; I just want to have a cold cursor
(defun setup-gold-cursor (frame)
  (progn
    (set-cursor-color "gold1")))

;; Make sure we always use UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Ask for y/n confirmation instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode t)

;; We aren't using monospace typewriters anymore
(setq sentence-end-double-space nil)

;; Since ethan-wspace takes care of this for us, we don't need it
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; Always load the newest version of a file, prevents stale compiled elisp code
(setq load-prefer-newer t)

;; Tab indentation is a curse, a historical pestilence.
;; Turn it off and let's never talk about this default again.
(set-default 'indent-tabs-mode nil)

;; Set default indentation for various languages. Maybe set up others as their mode is set.
(setq-default tab-width 4) ;; Objective-C was my first professional programming language.

;; Define where to keep the autoload declarations.
(setq autoload-file (expand-file-name "loaddefs.el" savefile-dir))

;; Define where to keep user-settings, and load them.
(setq custom-file (expand-file-name "custom.el" savefile-dir))
(load custom-file 'noerror)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (call-interactively 'goto-line))
    (display-line-numbers-mode -1)))

(defun json-format ()
  "Reformats the JSON in the region for humans."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; Save point position between sessions.
(use-package saveplace
  :ensure t
  :init (setq save-place-file (expand-file-name ".places" savefile-dir))
  :config
  (setq-default save-place t))

;; Save history
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

;; Easily switch between windows
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-swap-window))
  :config
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))) ;; Optimise for Dvorak

;; The package formerly known as Ace-Jump Mode
(use-package avy
  :ensure t
  :defer t
  :bind (("C-;" . avy-goto-word-1)
         ("C-:" . avy-goto-char)))

;; Setup flyspell with hunspell
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config (when (executable-find "hunspell")
            (setq ispell-program-name (executable-find "hunspell"))
            (setq ispell-really-hunspell t)
            (setenv "DICTIONARY" "en_GB")
            (setq ispell-hunspell-dictionary-alist '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8))))
  (setq ispell-dictionary "en_GB"))

;; Automatically save files
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

;; All good IDEs have some interactivity
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook #'global-company-mode)
  :commands company-mode
  :config
  ;; Enable company-mode globally.
  (global-company-mode)
  ;; Except when you're in term-mode.
  (setq company-global-modes '(not term-mode))
  ;; Give Company a decent default configuration.
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence))
  ;; Show documentation where available for selected completion
  ;; after a short delay.
  (use-package company-quickhelp
    :ensure t
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))
  ;; Use C-\ to activate the Company autocompleter.
  ;; We invoke company-try-hard to gather completion candidates from multiple
  ;; sources if the active source isn't being very forthcoming.
  (use-package company-try-hard
    :ensure t
    :commands company-try-hard
    :bind ("C-\\" . company-try-hard)
    :config
    (bind-keys :map company-active-map
               ("C-\\" . company-try-hard)))
  :diminish company-mode)

;; A lot of useful functions from Prelude
(use-package crux
  :ensure t
  :commands crux-switch-to-previous-buffer
  :bind
  ("C-c o" . crux-open-with)                                      ;; Open the currently visited file with an external program
  ("M-n" . crux-smart-open-line-above)                            ;; Insert an empty line above the current line and indent it properly
  ("M-p" . crux-smart-open-line)                                  ;; Insert empty line and indent it properly
  ("C-c n" . crux-cleanup-buffer-or-region)                       ;; Fix indentation and strip whitespace
  ("C-c e" . crux-eval-and-replace)                               ;; Evale a bit of elisp and replace it with it's result
  ("C-x p t" . crux-transpose-windows)                            ;; Transpose the buffers between two windows
  ("C-c D" . crux-delete-file-and-buffer)                         ;; Delete current file and buffer
  ("C-c d" . crux-duplicate-current-line-or-region)               ;; Duplicate current line (region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region) ;; Duplicate and comment current line (region)
  ("C-c r" . crux-rename-file-and-buffer)                         ;; Rename the current buffer and visited file if any
  ("C-c k" . crux-kill-other-buffers)                             ;; Kill all but the current buffer
  ("M-j" . crux-top-join-lines)                                   ;; Join lines
  ("s-k" . crux-kill-whole-line)                                  ;; Kill whole line
  ("C-<backspace>" . crux-kill-line-backwards)                    ;; Kill line backwards
  ("C-c i" . crux-ispell-word-then-abbrev)                        ;; Fix word using ispell and then save to abbrev.
  )

;; Strict whitespace with ethan-wspace: highlight bad habits,
;; and automatically clean up your code when saving.
;; Use C-c c to instantly clean up your file.
;; Read more about ethan-wspace: https://github.com/glasserc/ethan-wspace
(use-package ethan-wspace
  :ensure t
  :commands global-ethan-wspace-mode
  :config
  (global-ethan-wspace-mode 1)
  :bind ("C-c c" . ethan-wspace-clean-all)
  :diminish ethan-wspace-mode)

;; Use C-= to select the innermost logical unit your cursor is on.
;; Keep hitting C-= to expand it to the next logical unit.
;; Protip: this goes really well with multiple cursors.
(use-package expand-region
  :ensure t)

;; Use M-x gist-buffer or M-x gist-region to create a gist
;; directly from the current buffer or selection.
(use-package gist
  :ensure t)

;; Mark uncommitted changes in the fringe.
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

;; helm for better navigation
(use-package helm
  :ensure t
  :config
  (progn
    (helm-mode 1))
  :bind
  (("M-y" . helm-show-kill-ring)
   ("C-x C-m" . helm-M-x)
   ("s-r" . helm-recentf)
   ("C-x C-b" . helm-buffers-list)))

;; We have to deal with JSON sometimes
(use-package json-mode
  :ensure t
  :commands json-mode
  :config
  (bind-keys :map json-mode-map
             ("C-c <tab>" . json-mode-beautify)))

;; Move like a ninja
(use-package key-chord
  :ensure t
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'avy-goto-word-1)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jk" 'avy-goto-char)
    (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
    (key-chord-define-global "xx" 'helm-M-x)
    (key-chord-define-global "yy" 'browse-kill-ring)))

(defadvice magit-status (around magit-fullscreen activate)
  "Activate full screen when using Magit."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-quit-window (around magit-restore-screen activate)
  "Restore previously hidden windows."
  ad-do-it
  (jump-to-register :magit-fullscreen))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;; Use C-x g to open a magit status window for the current directory.
(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("q" . magit-quit-session)))

;; Just set mode hooks and add a couple of keybindings to the mode map.
(use-package markdown-mode
  :ensure t
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

;; Why edit one line when you can work on many
(use-package multiple-cursors
  :ensure t
  :commands multiple-cursors-mode
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-e" . mc/edit-ends-of-lines)
         ("C-S-c C-S-a" . mc/edit-beginnings-of-lines))
  :config
  (setq mc/list-file (expand-file-name ".mc-lists.el" savefile-dir)))

;; Recent files
(use-package recentf
  :ensure t
  :init
  (progn
    (setq recentf-save-file (expand-file-name "recentf" savefile-dir))
    (setq recentf-auto-cleanup 'never)
    (recentf-mode 1))
  :config (setq recentf-max-saved-items 100
                recentf-max-menu-items 15))

;; Parentheses are important
(use-package smartparens
  :ensure t
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t))
  :config
  (progn
    (add-hook 'prog-mode-hook (lambda () (smartparens-strict-mode t))) ;; If I don't do this, it doesn't turn on properly.
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
    (setq sp-highlight-pair-overlay nil)
    (setq sp-highlight-wrap-overlay nil)
    (setq sp-highlight-wrap-tag-overlay nil))
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(defun ygg-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

;; Use the simpler undo-fu package instead of undoo tree
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Make buffer titles unique by adding more information, not just another number.
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-separator "/"
                uniquify-after-kill-buffer-p t     ;; Rename after killing uniquified
                uniquify-ignore-buffers-re "^\\*")) ;; Don't futz with special buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General programming mode support
;;

;; .editorconfig file support
(use-package editorconfig
    :ensure t
    :config (editorconfig-mode +1))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swift-mode
;;

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :interpreter "swift")

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((swift-mode . lsp)))

(use-package lsp-ui
  :ensure t)

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :custom
  (lsp-sourcekit-executable "/home/abizern/swift-6.0.3/usr/bin/sourcekit-lsp"))

;; web-mode is a special mode for HTML which cops with embedded JS/CSS,
;; JSX, various templating systems, ect.
;; find out more at http://web-mode.org
(use-package web-mode
  :ensure t
  :mode (;; Want to use web-mode for HTML, not default html-mode.
         ("\\.html?\\'" . web-mode)
         ;; Add some extensions as per web-mode docs
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  ;; Highlight element under the cursor.
  (setq-default web-mode-enable-current-element-highlight t)
  ;; Key for renaming tags
  (bind-keys :map web-mode-map
             ("C-c C-r" . 'mc/mark-sgml-tag-pair)))

(defun my-web-mode-hook ()
  "Hooks for web-mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Colourise colour names in certain modes.
(use-package rainbow-mode
  :ensure t
  :config
  (dolist (mode '(css-mode less-css-mode html-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (rainbow-mode))))
  :diminish rainbow-mode)

;; Show available keybindings after starting to type.
(use-package which-key
  :ensure t
  :config (which-key-mode +1)
  :diminish which-key-mode)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml$ . yaml-mode"))

(use-package toml-mode
  :ensure t
  :mode ("\\.toml$ . toml-mode"))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (add-hook 'after-save-hook
              (lambda ()
                (when (eql major-mode 'snippet-mode)
                  (yas-reload-all))))
    (setq yas-snippet-dirs (list (f-expand "snippets" dotfiles-dir)))
    (setq yas-indent-line 'auto)
    (yas-global-mode 1))
  :mode ("\\.yasnippet" . snippet-mode))

(use-package helm-c-yasnippet
  :ensure t
  :init
  (setq helm-yas-space-match-any-greedy t)
  (global-set-key (kbd "C-c y") 'helm-yas-complete)
  (yas-global-mode 1))

;; Use the better version of zap-to-char
(use-package zop-to-char
  :ensure t
  :bind
  (("M-z" . zop-up-to-char)
   ("M-Z" . zop-to-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Common-Lisp support

(defun setup-slime()
  ;; Set up only if quicklisp has been installed
  (setq-local slime-helper (expand-file-name "~/.quicklisp/slime-helper.el"))
  (when (file-exists-p slime-helper)

    (load slime-helper)
    (setq inferior-lisp-program "/usr/bin/sbcl")

    (use-package slime
      :ensure t
      :init
      (slime-setup '(slime-fancy slime-company)))

    (use-package slime-company
      :ensure t
      :after (slime company)
      :config (setq slime-company-completion 'fuzzy))))

(defun setup-sly()
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (use-package sly
    :ensure t
    :config
    (with-eval-after-load 'sly
      `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))))

(setup-sly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org-mode

(use-package org
  :ensure t
  :config
  ;; Stop org-mode from hijacking shift-cursor keys.
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)
                             (define-key org-mode-map (kbd "C-c t") 'yas-next-field)))
  (bind-keys :map org-mode-map
             ("M-j" . org-metaup)
             ("M-k" . org-metadown))
  (setq org-directory "~/Documents/Org")
  (setq org-metadir (concat org-directory "_orgmata/"))
  (setq org-archive-location (concat org-metadir "archive.org::date-tree"))
  (setq org-default-notes-file (concat org-directory "refile.org"))
  (setq org-agenda-files (quote ("~/Documents/Org/")))
  (setq org-startup-indented t)
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "DRAFT(r)" "|" "PUBLISH(p)")))
  (setq org-use-fast-todo-selection t) ;; done with C-c C-t KEY
  (setq org-log-done 'time)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil) ;; Change state with S-left / right. Skip timestamp processing. Handy when just clearing up.
  
  
  ;; Fancy bullet rendering.
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  ;; Flashcards
  (use-package org-drill
    :ensure t
    :config (progn
              (add-to-list 'org-modules 'org-drill)
              (setq org-drill-add-random-noise-to-intervals-p t)
              (setq org-drill-learn-fraction 0.25)))
  ;; Insert links from clipboard.
  (use-package org-cliplink
    :ensure t
    :config
    (with-eval-after-load "org"
      (define-key org-mode-map (kbd "C-c M-l") 'org-cliplink)))
  (require 'ox-latex)
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  ;; Override standard article classes
  ;; Select this by adding #+LaTeX_CLASS: <class-name> to the org file preamble
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[a4paper]{scrartcl}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{fullpage}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("tufte-handout"
                 "\\documentclass[a4paper]{tufte-handout}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath}
\\usepackage{amssymb}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; ox-hugo
(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

;; Latex

(use-package tex
  :ensure auctex
  :config
  (setq-default TeX-master nil)
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  :ensure company-auctex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Major Mode support

;; Use Ruby syntax for Cartfiles
(add-to-list 'auto-mode-alist '("Cartfile\\'" . ruby-mode))

;; Use Ruby for Fastlane files
(add-to-list 'auto-mode-alist '("Fastfile\\'" . ruby-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projects with Projectile
;;
;; https://docs.projectile.mx/projectile/index.html
;;

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Convenience Functions

;; Insert Date
;; Usage
;; - `C-c C-d` -> 13/04/2024
;; - `C-u C-c C-d` -> 2024-04-13
;; - `C-u C-u C-d C-d` -> Tuesday, April 13, 2024

(defun ygg-insert-date (prefix)
  "Insert the current date. With prefix-argument use ISO format. With two
prefix arguments, write out the day and month name"
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d/%m/%Y")
                 ((equal prefix '(4)) "%F")
                 ((equal prefix '(16)) "%A, %B %d, %Y")))
        (system-time-locale "en_GB"))
    (insert (format-time-string format))))

(defun ygg-insert-iso-date-time ()
    "Insert the current date in ISO format for UTC"
    (interactive)
    (insert (format-time-string "%F%T%z" nil "UTC")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key Bindings


;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-C-=") 'er/contract-region)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;;;; Remaps goto-line so that line numbers are turned on only when needed.
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Always indent after a newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Move about more quickly
;; move about in steps of 5 with C-S insteard of just C-
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; Add some Xcode bindings
(defun move-line-up ()
  "Move the current line up"
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-s-]")
                (lambda ()
                  (interactive)
                  (move-line-up)))

(global-set-key (kbd "M-s-[")
                (lambda ()
                  (interactive)
                  (move-line-down)))

;; Org-mode global keys
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; ygg date functions
(global-set-key (kbd "C-c C-d") 'ygg-insert-date)
(global-set-key (kbd "C-c C-t") 'ygg-insert-iso-date-time)



;; If I'm running emacs, then I want it to be a server
(require 'server)
(unless (server-running-p)
  (server-start))
(put 'upcase-region 'disabled nil)


;; Trying to fix an error with not being able to update packages
;; https://emacs.stackexchange.com/questions/68288/error-retrieving-https-elpa-gnu-org-packages-archive-contents

(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
