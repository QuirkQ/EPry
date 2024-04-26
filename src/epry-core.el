;;; epry-core.el --- Emacs Pry debugger interface -*- lexical-binding: t -*-
;;;
;;; Author: QuirkQ
;;; Version: 0.0.0
;;; URL: https://github.com/quirkq/epry
;;; Package-Requires: ((emacs "29.1"))
;;; License: MIT
;;;
;;; Commentary:
;;;
;;; This is the core of epry
;;;
;;; Code:

(defgroup epry nil
  "Customization group for EPry."
  :prefix "epry-"
  :group 'applications)

(defcustom epry-shell-path "/opt/homebrew/bin/fish" ; "/bin/bash"
  "Path to the shell used by EPry for executing commands."
  :type 'string
  :group 'epry)

(defcustom epry-prefix-command "mise x -- " ; ""
  "Prefix command because we run in a non-interactive shell."
  :type 'string
  :group 'epry)

(defun epry-project-root ()
  "Return the root directory of the project."
  (or
   ;; Try to find the Git root via VC first
   (vc-root-dir)
   ;; Fallback to projectile if vc-root-dir is not available or fails
   (and (featurep 'projectile) (projectile-project-root))
   ;; Default to the current directory if no method works
    default-directory))

(defvar epry-sessions (make-hash-table :test 'equal)
  "Hash table storing epry-ui instances keyed by their project root.")

(defun epry-start (&optional command)
  "Start EPry by initializing the UI and displaying it, optionally running a COMMAND."
  (interactive)
  (let* ((root (epry-project-root))
         (ui (or (gethash root epry-sessions)  ; Check for an existing session first
                 (let ((new-ui (epry-ui :project-root root)))  ; Create and initialize new UI
                   (epry-ui-init new-ui)
                   (puthash root new-ui epry-sessions)
                   new-ui))))
    (with-current-buffer (oref ui buffer)
      (setq-local epry-current-ui ui)
      (when command  ; If a command is provided, run it.
        (epry-run-command ui command)))
    (switch-to-buffer-other-window (oref ui buffer))
    ui)) ; Ensure that the UI instance is returned

(defun epry-execute-command (&optional command)
  "Prompt for a command, or use the provided COMMAND, and execute it in the context of the current or new EPry session."
  (interactive)
  ;; If no command is provided, prompt the user for one
  (unless command
    (setq command (read-from-minibuffer "Command: ")))

  (let ((ui (if (and (derived-mode-p 'epry-mode) (bound-and-true-p epry-current-ui))
                epry-current-ui  ; Use the existing UI instance if available
              (epry-start))))  ; Otherwise, start a new session
    ;; Only run the command if ui is correctly set and we're in the right mode
    (when (and ui (derived-mode-p 'epry-mode))
      (epry-run-command ui command))))

(defun epry-bundle-install ()
  "Run 'bundle install' in the context of the current or new EPry session."
  (interactive)
  (epry-execute-command "bundle install"))

(defun epry-test ()
  "Run 'bundle exec rspec' in the context of the current or new EPry session."
  (interactive)
  (epry-execute-command "bundle exec rspec"))

(defun epry-rails ()
  "Run 'bundle exec rails s' in the context of the current or new EPry session."
  (interactive)
  (epry-execute-command "bundle exec rails s"))

(defun pprint-epry-sessions ()
  "Pretty-print all entries in the `epry-sessions` hash table to a dedicated buffer."
  (interactive)
  (let ((output-buffer (get-buffer-create "*EPry-Sessions*")))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (maphash (lambda (key value)
                 (pp `(,key ,value) output-buffer))
               epry-sessions)
      (read-only-mode 1))
    (pop-to-buffer output-buffer)))

(provide 'epry-core)
;;; epry-core.el ends here
