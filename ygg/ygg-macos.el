;;; -*- lexical-binding: t -*-
;;; ygg-macos.el --- Configurations particular to macOS.

(require 'ygg-package)

;; Set up modifier keys.
(setq ns-function-modifier 'hyper)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; Use the trash.
(setq delete-by-moving-to-trash t)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Make the browser the OS X default.
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Make the path available
(use-package exec-path-from-shell
  :ensure t
  :init (setq exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))

  (provide 'ygg-macos)
  ;;; ygg-macos.el ends here
