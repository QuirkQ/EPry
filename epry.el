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

(require 'eieio)
(require 'cl-lib)
(require 'ruby-mode)
(require 'ansi-color)

;; epry root directory.
(defvar epry-dir (file-name-directory load-file-name))

;; Add src directory to load-path
(let ((src-dir (expand-file-name "src" epry-dir)))
  (add-to-list 'load-path src-dir))

(defun epry-load-files ()
  "Load all the necessary files for the Epry package."
  (interactive)
  (require 'epry-ui)
  (require 'epry-mode)
  (require 'epry-core))

;; Call the function to load all files when Epry is started
(epry-load-files)

(provide 'epry)
;;; epry.el ends here
