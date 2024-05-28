;;; epry.el --- Emacs Pry debugger interface -*- lexical-binding: t -*-
;;;
;;; Author: QuirkQ
;;; Version: 1.0.0
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

(defcustom epry-gemfile-name "Gemfile.epry" ; "Gemfile.local"
  "Name of the Gemfile to create and use for EPRy."
  :type 'string
  :group 'epry)

(defcustom epry-debug-statement "require 'debug/open'; DEBUGGER__.open #fixme"
  "Ruby debug statement to insert"
  :type 'string
  :group 'epry)

(defun epry-project-root ()
  "Return the root directory of the project."
  (expand-file-name (or
   ;; Try to find the Git root via VC first
   (vc-root-dir)
   ;; Fallback to projectile if vc-root-dir is not available or fails
   (and (featurep 'projectile) (projectile-project-root))
   ;; Default to the current directory if no method works
    default-directory)))

(defvar epry-sessions (make-hash-table :test 'equal)
  "Hash table storing epry-ui instances keyed by their project root.")

(define-derived-mode epry-mode fundamental-mode "Epry"
  "Major mode for the epry debugger interface."
  (defvar-local epry-current-ui nil "Local instance of the epry-ui class.")
  (setq buffer-read-only t)
  ;; (setq-local buffer-save-without-query t)
  (display-line-numbers-mode -1))

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

(defun epry-cleanup-session (project-root)
  "Remove the session associated with PROJECT-ROOT from the epry-sessions hash table."
  (interactive "Project Root: ")
  (if (gethash project-root epry-sessions)
      (progn
        (remhash project-root epry-sessions)
        (message "Successfully removed session for project root: %s" project-root))
    (message "No session found for project root: %s" project-root)))

(defun epry-cleanup-sessions ()
  "Remove all entries from epry-sessions where the associated buffer has been killed."
  (interactive)
  (maphash (lambda (root ui)
             (let ((buffer (oref ui buffer)))
               (when (not (buffer-live-p buffer))
                 (remhash root epry-sessions))))
    epry-sessions))

(defun epry-get-or-create-ui ()
  "Get or create an `epry-ui` instance depending on the current context."
  (if (and (derived-mode-p 'epry-mode) (bound-and-true-p epry-current-ui))
      epry-current-ui  ; Return the existing UI if already present and correct mode
    (epry-start)))     ; Start a new UI session if not present

(defun epry-execute-command (&optional command)
  "Prompt for a command, or use the provided COMMAND, and execute it in the context of the current or new EPry session."
  (interactive)
  ;; If no command is provided, prompt the user for one
  (unless command
    (setq command (read-from-minibuffer "Command: ")))

  ;; Setup project Gemfile before executing the command
  (epry-setup-project-gemfile)

  (let ((ui (epry-get-or-create-ui)))  ; Get or create the UI instance
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

(defun epry-pprint-sessions ()
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
                 :documentation "The root directory of the current project."))
  "Class representing the UI for EPry.")

(cl-defmethod epry-ui-init ((ui epry-ui))
  "Initialize the EPry UI: create the buffer based on the project root and set up the window."
  (with-slots (buffer window project-root) ui
    ;; Determine the project root and store it in the instance
    (setf project-root (expand-file-name (epry-project-root)))
    ;; Set the buffer name to include the project directory name
    (setq buffer (get-buffer-create (format "*EPry* - %s" (file-name-nondirectory (directory-file-name project-root)))))
    (with-current-buffer buffer
      (setq-local epry-current-ui ui)
      (add-hook 'kill-buffer-hook
                (lambda () (epry-cleanup-session project-root))
        nil t)
      (epry-mode))
    (setq window (display-buffer buffer))))

(cl-defmethod epry-create-command ((ui epry-ui) command)
  "Create a shell command string adjusted for the stored project directory."
  (with-slots (project-root) ui
    (let* ((expanded-root (expand-file-name project-root))  ; Ensure path is expanded.
           (epry-gemfile (expand-file-name epry-gemfile-name project-root))  ; Path to the epry Gemfile
           (bundle-gemfile-setting (if (file-exists-p epry-gemfile)
                                       (concat "BUNDLE_GEMFILE=" (shell-quote-argument epry-gemfile) " ")
                                     ""))  ; Set BUNDLE_GEMFILE if the file exists
           (full-command (concat "cd " (shell-quote-argument expanded-root) " && "
                                 epry-prefix-command
                                 "ruby --version" " && "
                                 bundle-gemfile-setting
                                 epry-prefix-command
                                 command)))
      (message "Executing command: %s" full-command)
      full-command)))

(cl-defmethod epry-run-command ((ui epry-ui) command)
  "Execute a COMMAND using the shell specified in `epry-shell-path` with output in the appropriate buffer."
  (with-slots (buffer) ui
    (let ((output-buffer (get-buffer-create buffer))
          (process-command (list epry-shell-path "-l" "-c" (epry-create-command ui command))))
      (with-current-buffer output-buffer
        ;; Ensure buffer is writable
        (let ((inhibit-read-only t))
          (erase-buffer))  ; Clear previous contents
        ;; Start the process
        (let ((process (make-process
                        :name "epry-command-process"
                        :buffer output-buffer
                        :command process-command
                        :filter (lambda (proc string)
                                  (with-current-buffer (process-buffer proc)
                                    (let ((inhibit-read-only t))
                                      (goto-char (point-max))
                                      (insert (ansi-color-apply string))
                                      (epry-auto-scroll)))))))
          (set-process-sentinel process
                                (lambda (proc event)
                                  (when (string-match-p "\\`finished" event)
                                    (with-current-buffer (process-buffer proc)
                                      (setq buffer-read-only t))))))))))

(cl-defmethod epry-get-project-gemfile-name ((ui epry-ui))
  "Return the name of the Gemfile based on the BUNDLE_GEMFILE environment variable or default to 'Gemfile'."
  (or (getenv "BUNDLE_GEMFILE") "Gemfile"))

(cl-defmethod epry-append-gemfile-extension ((ui epry-ui) target-gemfile)
  "Append contents of Gemfile_extension.rb to the TARGET-GEMFILE."
  (with-slots (project-root) ui
    (let ((extension-file (expand-file-name "Gemfile_extension.rb" epry-dir))) ;; Ensure epry-dir is correctly defined as the EPry root
      (message "Debug extension file: %s" extension-file)
      (message "Debug target gemfile: %s" target-gemfile)
      (if (and (file-exists-p extension-file) (file-exists-p target-gemfile))
          (with-temp-buffer
            (insert-file-contents extension-file)
            (append-to-file (point-min) (point-max) target-gemfile)
            (message "Appended Gemfile_extension.rb to %s." target-gemfile))))))

(cl-defmethod epry-create-project-gemfile ((ui epry-ui))
  "Create an EPRy Gemfile and its corresponding Gemfile.lock in the project root."
  (with-slots (buffer window project-root) ui
    (let ((original-gemfile (expand-file-name (epry-get-project-gemfile-name ui) project-root))
          (original-gemfile-lock (expand-file-name (concat (epry-get-project-gemfile-name ui) ".lock") project-root))
          (epry-gemfile (expand-file-name epry-gemfile-name project-root))
          (epry-gemfile-lock (expand-file-name (concat epry-gemfile-name ".lock") project-root)))
      ;; Check if the original Gemfile exists
      (if (not (file-exists-p original-gemfile))
          (message "No Gemfile found in the project root: %s" project-root)
        (progn
          ;; Copy the original Gemfile to the new epry-gemfile location
          (copy-file original-gemfile epry-gemfile t)
          (message "Created EPRy Gemfile at: %s" epry-gemfile)
          (epry-append-gemfile-extension ui epry-gemfile)
          ;; Check and copy Gemfile.lock if it exists
          (when (file-exists-p original-gemfile-lock)
            (copy-file original-gemfile-lock epry-gemfile-lock t)
            (message "Created EPRy Gemfile.lock at: %s" epry-gemfile-lock)))))))

(defun epry-setup-project-gemfile ()
  "Setup project Gemfile and Gemfile.lock using the EPry UI."
  (interactive)
  (let ((ui (epry-get-or-create-ui)))
    (when ui
      (epry-create-project-gemfile ui))))

(defun epry-insert-ruby-debug-statement ()
  "Inserts the Ruby debug statement defined by `epry-debug-statement` at the current cursor position."
  (interactive)
  (insert epry-debug-statement))

(provide 'epry)
;;; epry.el ends here
