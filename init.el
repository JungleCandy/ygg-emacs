;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t -*-

;;; init.el --- My personal configuration file.

;; Reduces the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB).
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

(require 'package)

;; Keep downloaded packages organised.
(setq package-user-dir
      (expand-file-name "elpa"
                        user-emacs-directory))


;; Set priorities for which archives are used.
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("stable" . "https://stable.melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("org"    . "http://orgmode.org/elpa/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 63)
                                                      ("nongnu" . 10)
                                                      ("org"    . 99)
                                                      ("stable" . 80)
                                                      ("melpa"  . 90)))

;; To get the package manager going, we invoke its initialise function.
(package-initialize)

;; Update package metadata if required
(unless package-archive-contents
  (package-refresh-contents))

;; This loads the org-file as the settings file.
(require 'org)
(org-babel-load-file
 (expand-file-name "setup.org"
                   user-emacs-directory))

