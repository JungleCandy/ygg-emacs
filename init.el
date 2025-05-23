;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t -*-

;;; init.el --- My personal configuration file.

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(require 'package)

(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))

(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("stable" . "https://stable.melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("org"    . "https://orgmode.org/elpa/")))

(setq package-archive-priorities
      '(("org"    . 99)
        ("melpa"  . 90)
        ("stable" . 80)
        ("gnu"    . 63)
        ("nongnu" . 10)))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Ensure org is installed before loading the .org config
(unless (package-installed-p 'org)
  (package-install 'org))

(require 'org)

;; Load and tangle the Org configuration
(org-babel-load-file
 (expand-file-name "Readme.org" user-emacs-directory))
