;;; -*- lexical-binding: t -*-
;;; ygg-defuns.el --- Useful functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit

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

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun json-format ()
  "Reformats the JSON in the region for humans."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun ygg-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(provide 'ygg-defuns)
;;; ygg-defuns.el ends here
