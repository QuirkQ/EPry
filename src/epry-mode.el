;;; epry-mode.el --- Emacs Pry debugger interface -*- lexical-binding: t -*-
;;;
;;; Author: QuirkQ
;;; Version: 0.0.0
;;; URL: https://github.com/quirkq/epry
;;; Package-Requires: ((emacs "29.1"))
;;; License: MIT
;;;
;;; Commentary:
;;;
;;; This is the major mode of epry
;;;
;;; Code:

(define-derived-mode epry-mode fundamental-mode "Epry"
  "Major mode for the epry debugger interface."
  (defvar-local epry-current-ui nil "Local instance of the epry-ui class.")
  (setq buffer-read-only t)
  (setq-local buffer-save-without-query t)
  (display-line-numbers-mode -1))

(provide 'epry-mode)
;;; epry-mode.el ends here
