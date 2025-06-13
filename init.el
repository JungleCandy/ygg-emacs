;;; package --- Create my Readme.el file
;;; Commentary:
;;; Tangle the Readme.org file.

;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t -*-

;;; Code:


(require 'package)

(require 'server)
(unless (server-running-p)
  (server-start))

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

;; Make sure Super is not used as a modifier
(setq mac-command-modifier 'none
      ns-command-modifier 'none)

;; Ensure org is installed before loading the .org config
(unless (package-installed-p 'org)
  (package-install 'org))

(require 'org)
(require 'ob-tangle)

;; Load and tangle the Org configuration
(org-babel-load-file
(expand-file-name "Readme.org" user-emacs-directory))
