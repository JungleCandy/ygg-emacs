;;; -*- lexical-binding: t -*-
;;; ygg-setup.el --- Rarely changed setup values.

(require 'ygg-package)

; Reduces the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB).
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Appearance

;; Turn off modes that look ugly.
(mapc
  (lambda (mode)
    (when (fboundp mode)
      (funcall mode -1)))
  '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))

;; We don't need a startup message.
(setq inhibit-startup-message t)

;; Turn off the annoying beep on errors
(setq visible-bell t)

;; A suitably wide fill-column
(set-default 'fill-column 100)

;; Show column and line number in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; There is something nice about this theme. https://draculatheme.com.
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)
  ;; Regardless of the theme, I'm particularly fond of my gold cursor.
  (setq-default cursor-type 'bar)
  (add-to-list 'default-frame-alist '(cursor-color . "gold1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing

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

;; JSON
(setq-default js-indent-level 2)

(provide 'ygg-setup)
;;; ygg-setup.el ends here
