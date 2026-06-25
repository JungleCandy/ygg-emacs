;;; early-init.el --- Early initialisation -*- lexical-binding: t -*-
;;; Commentary:
;;; Loaded before package initialisation and GUI setup.

;;; Code:

;; libgccjit linker errors on macOS (missing emutls_w); disable all native compilation early.
(setq native-comp-jit-compilation nil)
(setq native-comp-enable-subr-trampolines nil)

;;; early-init.el ends here
