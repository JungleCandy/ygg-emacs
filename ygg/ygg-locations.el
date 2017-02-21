;;; -*- lexical-binding: t -*-
;;; ygg-locations.el --- Definitions for where to store support files.

(require 'ygg-package)

;; Define where to keep the autoload declarations.
(setq autoload-file (expand-file-name "loaddefs.el" savefile-dir))

;; Define where to keep user-settings, and load them.
(setq custom-file (expand-file-name "custom.el" savefile-dir))
(load custom-file 'noerror)

;; Emacs writes backup files to `filename~` by default. This is messy,
;; so let's tell it to write them to `~/.emacs.d/bak` instead.
;; If you have an accident, check this directory - you might get lucky.
(setq backup-directory-alist
      `(("." . ,(expand-file-name (expand-file-name "bak" savefile-dir)))))

;; Save point position between sessions.
(use-package saveplace
  :ensure t
  :init (setq-default save-place t)
  :config
  (setq save-place-file (expand-file-name ".places" savefile-dir)))

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

(provide 'ygg-locations)
;;; ygg-locations.el ends here
