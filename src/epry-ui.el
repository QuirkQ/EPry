;;; epry-ui.el --- Emacs Pry debugger interface -*- lexical-binding: t -*-
;;;
;;; Author: QuirkQ
;;; Version: 0.0.0
;;; URL: https://github.com/quirkq/epry
;;; Package-Requires: ((emacs "29.1"))
;;; License: MIT
;;;
;;; Commentary:
;;;
;;; This is the UI of epry
;;;
;;; Code:

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
      (epry-mode))
    (setq window (display-buffer buffer))))

(cl-defmethod epry-create-command ((ui epry-ui) command)
  "Create a shell command string adjusted for the stored project directory."
  (with-slots (project-root) ui
    (let* ((expanded-root (expand-file-name project-root))  ; Ensure path is expanded.
           (full-command (concat "cd " (shell-quote-argument expanded-root) " && " command)))
      full-command)))

(cl-defmethod epry-run-command ((ui epry-ui) command)
  "Execute a COMMAND using the shell specified in `epry-shell-path` with output in the appropriate buffer."
  (with-slots (buffer) ui
    (let ((output-buffer (get-buffer-create buffer))
          (process-command (list epry-shell-path "-c" (epry-create-command ui command))))
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
                                      (insert (ansi-color-apply string))))))))
          (set-process-sentinel process
                                (lambda (proc event)
                                  (when (string-match-p "\\`finished" event)
                                    (with-current-buffer (process-buffer proc)
                                      (setq buffer-read-only t))))))))))


(provide 'epry-ui)
;;; epry-ui.el ends here
