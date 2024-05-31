;;; epry.el --- Emacs integration for Pry Ruby debugger -*- lexical-binding: t -*-
;;;
;;; Author: QuirkQ
;;; Version: 1.1.0
;;; URL: https://github.com/quirkq/epry
;;; Package-Requires: ((emacs "29.1"))
;;; Keywords: tools, ruby
;;; License: MIT
;;;
;;; Commentary:
;;;
;;; EPry is an Emacs interface for the Pry debugger, designed to enhance the Ruby
;;; development experience by integrating powerful debugging capabilities directly
;;; into your Emacs workflow.  This package provides seamless interaction with Pry
;;; sessions, allowing you to execute commands, manage sessions, and debug Ruby
;;; code more efficiently.
;;;
;;; This file contains the core definitions and functions necessary to set up and
;;; use EPry.  It includes the main mode definition, customizations, key bindings,
;;; and utility functions to interact with Pry sessions.  By loading this file,
;;; all essential components required for EPry to function are initialized.
;;;
;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'ruby-mode)
(require 'ansi-color)

(declare-function multi-vterm-project "ext:multi-vterm" ())
(declare-function vterm-send-string "ext:vterm" (string))
(declare-function vterm-send-return "ext:vterm" ())

(defgroup epry nil
  "Customization group for EPry."
  :prefix "epry-"
  :group 'applications)

(defcustom epry-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "The root directory of the epry package."
  :type 'directory
  :group 'epry)

(defcustom epry-shell-path "/opt/homebrew/bin/fish" ; "/bin/bash"
  "Path to the shell used by EPry for executing commands."
  :type 'string
  :group 'epry)

(defcustom epry-prefix-command "mise x -- " ; ""
  "Prefix command because we run in a non-interactive shell."
  :type 'string
  :group 'epry)

(defcustom epry-gemfile-name "Gemfile.epry" ; "Gemfile.local"
  "Name of the Gemfile to create and use for EPRy."
  :type 'string
  :group 'epry)

(defcustom epry-debug-statement "require 'debug/open'; DEBUGGER__.open #fixme"
  "Ruby debug statement to insert."
  :type 'string
  :group 'epry)

(defcustom epry-original-url ""
  "The original URL in the Gemfile.lock to be replaced."
  :type 'string
  :group 'epry)

(defcustom epry-new-url ""
  "The new URL to replace the original URL in the Gemfile.lock."
  :type 'string
  :group 'epry)

(defun epry-project-root ()
  "Return the root directory of the project."
  (expand-file-name
   (or (vc-root-dir)
       default-directory)))

(defvar epry-sessions (make-hash-table :test 'equal)
  "Hash table storing \='epry-ui\=' instances keyed by their project root.")

(defvar-local epry-current-ui nil
  "Local instance of the \='epry-ui\=' class.")

(defvar epry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'epry-kill-process)
    (define-key map (kbd "<f19> i") 'epry-bundle-install)
    (define-key map (kbd "<f19> o") 'epry-rails)
    (define-key map (kbd "<f19> p") 'epry-test)
    (define-key map (kbd "<f19> l") 'epry-execute-command)
    map)
  "Keymap for `epry-mode`.")

(define-derived-mode epry-mode fundamental-mode "Epry"
  "Major mode for the epry debugger interface."
  (setq buffer-read-only t)
  (display-line-numbers-mode -1)
  :keymap epry-mode-map)

(defclass epry-ui ()
  ((buffer :initarg :buffer
           :initform "*EPry*"
           :type t
           :documentation "The buffer associated with the EPry session.")
   (window :initarg :window
           :type t
           :documentation "The window displaying the EPry buffer.")
   (project-root :initarg :project-root
                 :initform nil
                 :type (or null string)
                 :documentation "The root directory of the current project.")
   (last-command :initarg :last-command
                 :initform nil
                 :type (or null string)
                 :documentation "The last executed command in this session."))
  "Class representing the UI for EPry.")

(defun epry-auto-scroll ()
  "Automatically scroll to the bottom of the buffer."
  (when (and (derived-mode-p 'epry-mode)
             (not (input-pending-p)))
    (let ((window (get-buffer-window (current-buffer))))
      (when window
        (with-selected-window window
          (goto-char (point-max))
          (recenter -1))))))

(defun epry-start (&optional command)
  "Start EPry by initializing the UI, optionally running a COMMAND."
  (interactive)
  (let* ((root (epry-project-root))
         (ui (or (gethash root epry-sessions)
                 (let ((new-ui (epry-ui :project-root root)))
                   (epry-ui-init new-ui)
                   (puthash root new-ui epry-sessions)
                   new-ui))))
    (with-current-buffer (oref ui buffer)
      (setq-local epry-current-ui ui)
      (when command
        (epry-run-command ui command)))
    (switch-to-buffer-other-window (oref ui buffer))
    ui))

(defun epry-cleanup-session (project-root)
  "Remove the session associated with PROJECT-ROOT from \='epry-sessions\='."
  (interactive "Project Root: ")
  (if (gethash project-root epry-sessions)
      (progn
        (remhash project-root epry-sessions)
        (message "Successfully removed session for project root: %s" project-root))
    (message "No session found for project root: %s" project-root)))

(defun epry-cleanup-sessions ()
  "Remove all \='epry-sessions\=' where the associated buffer has been killed."
  (interactive)
  (maphash (lambda (root ui)
             (let ((buffer (oref ui buffer)))
               (when (not (buffer-live-p buffer))
                 (remhash root epry-sessions))))
    epry-sessions))

(defun epry-get-or-create-ui ()
  "Get or create an `epry-ui` instance depending on the current context."
  (if (and (derived-mode-p 'epry-mode) (bound-and-true-p epry-current-ui))
      epry-current-ui
    (epry-start)))

(defun epry-execute-command (&optional command callback)
  "Run a COMMAND in a EPry session with optional CALLBACK function."
  (interactive)
  (let ((ui (epry-get-or-create-ui)))
    (unless command
      (setq command (read-from-minibuffer "Command: " (oref ui last-command))))

    (when (and ui (derived-mode-p 'epry-mode))
      (oset ui last-command command)
      (epry-run-command ui command callback))))

(defun epry-bundle-install ()
  "Run \='bundle install\=' in a EPry session."
  (interactive)
  (epry-setup-project-gemfile)
  (let ((ui (epry-get-or-create-ui)))
    (when (and ui (derived-mode-p 'epry-mode))
      (epry-run-command ui "bundle install" #'epry-update-gemfile-lock))))

(defun epry-test ()
  "Run \='bundle exec rspec\=' in a EPry session."
  (interactive)
  (epry-execute-command "bundle exec rspec"))

(defun epry-rails ()
  "Run \='bundle exec rails s\=' in a EPry session."
  (interactive)
  (epry-execute-command "bundle exec rails s"))

(defun epry-pprint-sessions ()
  "Pretty-print the `epry-sessions` hash table to a dedicated buffer."
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

(cl-defmethod epry-ui-init ((ui epry-ui))
  "Initialize the EPry UI: create the buffer based on the project root."
  (with-slots (buffer window project-root) ui
    (setf project-root (expand-file-name (epry-project-root)))
    (setq buffer (get-buffer-create (format "*EPry* - %s"
                                            (file-name-nondirectory
                                             (directory-file-name project-root)))))
    (with-current-buffer buffer
      (setq-local epry-current-ui ui)
      (add-hook 'kill-buffer-hook
                (lambda () (epry-cleanup-session project-root))
                nil t)
      (epry-mode))
    (setq window (display-buffer buffer))))

(cl-defmethod epry-create-command ((ui epry-ui) command)
  "Create a shell COMMAND string adjusted for the UI stored project directory."
  (with-slots (project-root) ui
    (let* ((expanded-root (expand-file-name project-root))
           (epry-gemfile (expand-file-name epry-gemfile-name project-root))
           (bundle-gemfile-setting (if (file-exists-p epry-gemfile)
                                       (concat "BUNDLE_GEMFILE="
                                               (shell-quote-argument epry-gemfile) " ")
                                     ""))
           (full-command (concat "cd " (shell-quote-argument expanded-root) " && "
                                 epry-prefix-command
                                 "ruby --version" " && "
                                 bundle-gemfile-setting
                                 epry-prefix-command
                                 command)))
      (message "Executing command: %s" full-command)
      full-command)))

(cl-defmethod epry-run-command ((ui epry-ui) command &optional callback)
  "Execute a COMMAND in UI and optionally, run CALLBACK after the command finishes."
  (with-slots (buffer) ui
    (let ((output-buffer (get-buffer-create buffer))
          (process-command (list epry-shell-path "-l" "-c"
                                 (epry-create-command ui command))))
      (with-current-buffer output-buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (make-process
         :name "epry-command-process"
         :buffer output-buffer
         :command process-command
         :filter (lambda (proc string)
                   (with-current-buffer (process-buffer proc)
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert (ansi-color-apply string))
                       (epry-auto-scroll)
                       (when (string-match "DEBUGGER: wait for debugger connection..."
                                           string)
                         (epry-debugger-attach ui)))))
         :sentinel (lambda (proc event)
                     (when (string-match-p "\\`finished" event)
                       (with-current-buffer (process-buffer proc)
                         (setq buffer-read-only t))
                       (when callback
                         (funcall callback ui)))))))))

(defun epry-get-project-gemfile-name ()
  "Return the name of the Gemfile based on BUNDLE_GEMFILE or default to 'Gemfile'."
  (or (getenv "BUNDLE_GEMFILE") "Gemfile"))

(cl-defmethod epry-create-project-gemfile ((ui epry-ui))
  "Create EPRy Gemfile and Gemfile.lock in the UI project root."
  (with-slots (project-root) ui
    (let ((original-gemfile (expand-file-name (epry-get-project-gemfile-name)
                                              project-root))
          (epry-gemfile (expand-file-name epry-gemfile-name project-root)))
      (if (not (file-exists-p original-gemfile))
          (message "No Gemfile found in the project root: %s" project-root)
        (progn
          (with-temp-file epry-gemfile
            (insert (format "# EPry generated Gemfile\n\n"))
            (insert (format "eval_gemfile '%s'\n\n"
                            (file-name-nondirectory original-gemfile)))
            (insert "gem 'debug', '>= 1.0.0'\n"))
          (message "Created EPRy Gemfile at: %s" epry-gemfile)
          (let ((original-gemfile-lock (expand-file-name (concat (epry-get-project-gemfile-name)
                                                                 ".lock")
                                                         project-root))
                (epry-gemfile-lock (expand-file-name (concat epry-gemfile-name ".lock")
                                                     project-root)))
            (when (file-exists-p original-gemfile-lock)
              (copy-file original-gemfile-lock epry-gemfile-lock t)
              (message "Created EPRy Gemfile.lock at: %s" epry-gemfile-lock))))))))

(cl-defmethod epry-update-gemfile-lock ((ui epry-ui))
  "Update the UI epry-gemfile-lock to replace the remote URL with the new URL."
  (with-slots (project-root) ui
    (let* ((epry-gemfile-lock (expand-file-name (concat epry-gemfile-name ".lock")
                                                project-root))
           (original-url (string-trim epry-original-url))
           (new-url (string-trim epry-new-url)))
      (message "Updating epry-gemfile-lock: %s" epry-gemfile-lock)
      (if (file-exists-p epry-gemfile-lock)
          (progn
            (with-temp-buffer
              (insert-file-contents epry-gemfile-lock)
              (goto-char (point-min))
              (let ((case-fold-search nil))
                (while (search-forward original-url nil t)
                  (replace-match new-url)))
              (write-region (point-min) (point-max) epry-gemfile-lock)))
        (message "epry-gemfile-lock file does not exist.")))))

(cl-defmethod epry-debugger-attach ((ui epry-ui))
  "Open a vterm and attach to the Ruby debugger of the UI project."
  (if (featurep 'multi-vterm)
      (with-slots (project-root) ui
        (multi-vterm-project)
        (vterm-send-string (concat "cd " project-root))
        (vterm-send-return)
        (vterm-send-string "rdbg --attach")
        (vterm-send-return))
    (message "multi-vterm is not available.")))

(defun epry-setup-project-gemfile ()
  "Setup project Gemfile and Gemfile.lock using the EPry UI."
  (interactive)
  (let ((ui (epry-get-or-create-ui)))
    (when ui
      (epry-create-project-gemfile ui))))

(defun epry-insert-ruby-debug-statement ()
  "Insert the `epry-debug-statement` at the current cursor position."
  (interactive)
  (insert epry-debug-statement))

(defun epry-kill-process ()
  "Send a SIGINT to the process running in the current EPry buffer."
  (interactive)
  (let ((ui (epry-get-or-create-ui)))
    (when ui
      (let ((buffer (oref ui buffer)))
        (with-current-buffer buffer
          (let ((process (get-buffer-process buffer)))
            (when process
              (interrupt-process process)
              (message "Sent SIGINT to process %s" process))))))))

(provide 'epry)
;;; epry.el ends here
