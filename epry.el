;;; epry.el --- Emacs Pry debugger interface -*- lexical-binding: t -*-
;;;
;;; Author: QuirkQ
;;; Version: 0.0.0
;;; URL: https://github.com/quirkq/epry
;;; Package-Requires: ((emacs "29.1"))
;;; Keywords: tools, ruby
;;; License: MIT
;;;
;;; Commentary:
;;;
;;; This is the root file to require everything necessary for epry
;;;
;;; Code:

;; Ensure eieio is loaded for OOP features
(require 'eieio)

;; epry root directory.
(defvar epry-dir (file-name-directory load-file-name))

(defun epry-load-files ()
  "Load all the necessary files for the Epry package."
  (interactive))

;; Call the function to load all files when Epry is started
(epry-load-files)

(provide 'epry)
;;; epry.el ends here
