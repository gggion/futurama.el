;;; futur-shell.el --- Async shell commands via futur  -*- lexical-binding: t -*-

;; Author: Gino Cornejo <gggion123@gmail.com>
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; URL: https://github.com/gggion/futurama.el
;; Keywords: processes, async

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "30.1") (futur "1.2"))

;;; Commentary:

;; Async replacements for `shell-command' and `async-shell-command'
;; built on `futur'.  Output streams incrementally.
;;
;;     (futur-shell-async-command "make -j8")
;;     (futur-shell-command "ls -la")
;;     (futur-shell-command-to-string "hostname")

;;; Code:

(require 'futur)
(require 'shell)
(require 'dired)
(require 'ansi-color)

;;;; Output filtering

(defun futur-shell--output-filter (proc string)
  "Insert STRING from PROC into process buffer with ANSI color support.
Handles carriage returns for progress-bar style output.

Used by `futur-shell--async-start'."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (start (point-max)))
        (save-excursion
          (goto-char start)
          (insert string)
          ;; Handle carriage returns: each \r erases from line start to \r.
          ;; Process forward through the inserted region.
          (goto-char start)
          (while (search-forward "\r" nil t)
            (delete-region (line-beginning-position) (point)))
          ;; Apply ANSI color codes on the inserted region.
          (ansi-color-apply-on-region start (point-max)))
        (dolist (w (get-buffer-window-list (current-buffer) nil t))
          (set-window-point w (point-max)))))))

;;;; Async shell command

(defun futur-shell-async-command (command &optional output-buffer)
  "Run COMMAND in a shell asynchronously, streaming output.

COMMAND is a shell command string.
OUTPUT-BUFFER is a buffer or buffer name for the output;
defaults to `shell-command-buffer-name-async'.

Output arrives incrementally with ANSI color support.
Respects `async-shell-command-buffer' for conflict resolution and
`async-shell-command-display-buffer' for deferred display.

Returns a futur delivering the integer exit code.

Also see `futur-shell-command' for non-streaming output and
`futur-shell-command-to-string' for capturing output as a string."
  (interactive
   (list (read-shell-command
          (if shell-command-prompt-show-cwd
              (format-message "Async shell command in `%s': "
                              (abbreviate-file-name default-directory))
            "Async shell command: ")
          nil nil
          (let ((filename (or buffer-file-name
                              (and (eq major-mode 'dired-mode)
                                   (dired-get-filename nil t)))))
            (and filename (file-relative-name filename))))))
  (when (string-match "[ \t]*&[ \t]*\\'" command)
    (setq command (substring command 0 (match-beginning 0))))
  (let* ((buffer (get-buffer-create
                  (or output-buffer shell-command-buffer-name-async)))
         (bname (buffer-name buffer))
         (proc (get-buffer-process buffer))
         (directory default-directory))
    (when proc
      (pcase-exhaustive async-shell-command-buffer
        ('confirm-kill-process
         (shell-command--same-buffer-confirm "Kill it")
         (kill-process proc))
        ('confirm-new-buffer
         (shell-command--same-buffer-confirm "Use a new buffer")
         (setq buffer (generate-new-buffer bname)))
        ('new-buffer
         (setq buffer (generate-new-buffer bname)))
        ('confirm-rename-buffer
         (shell-command--same-buffer-confirm "Rename it")
         (with-current-buffer buffer (rename-uniquely))
         (setq buffer (get-buffer-create bname)))
        ('rename-buffer
         (with-current-buffer buffer (rename-uniquely))
         (setq buffer (get-buffer-create bname)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory directory)
      (setq mode-line-process '(":%s"))
      (setq-local revert-buffer-function
                  (lambda (&rest _)
                    (futur-shell-async-command command (current-buffer)))))
    (when async-shell-command-display-buffer
      (display-buffer buffer '(nil (allow-no-window . t))))
    (let ((fut (futur-shell--async-start command buffer)))
      (when (called-interactively-p 'any)
        (futur-bind fut #'ignore))
      fut)))

(defun futur-shell--async-start (command buffer)
  "Start COMMAND in BUFFER, return futur delivering exit code.
Installs `futur-shell--output-filter' and `futur-shell--async-sentinel'.
Unlocks process from creating thread via `set-process-thread'.

Used by `futur-shell-async-command'."
  (futur-new
   (lambda (f)
     (let ((proc (start-process-shell-command "Shell" buffer command)))
       (set-process-thread proc nil)
       (set-process-filter proc #'futur-shell--output-filter)
       (set-process-sentinel proc #'futur-shell--async-sentinel)
       (unless async-shell-command-display-buffer
         (let ((nonce (make-symbol "nonce")))
           (add-function
            :before (process-filter proc)
            (lambda (proc _string)
              (let ((buf (process-buffer proc)))
                (when (buffer-live-p buf)
                  (remove-function (process-filter proc) nonce)
                  (display-buffer buf '(nil (allow-no-window . t))))))
            `((name . ,nonce)))))
       (process-put proc 'futur--deliver-to f)
       proc))))

(defun futur-shell--async-sentinel (proc signal)
  "Sentinel for `futur-shell--async-start' processes.
Delivers exit code to the futur stored on PROC's property list.

SIGNAL is the sentinel string from Emacs."
  (when (memq (process-status proc) '(exit signal))
    (shell-command-set-point-after-cmd (process-buffer proc))
    (message "%s: %s."
             (car (cddr (or (process-get proc 'remote-command)
                            (process-command proc))))
             (substring signal 0 -1))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (setq mode-line-process nil)
        (force-mode-line-update)))
    (futur-deliver-value (process-get proc 'futur--deliver-to)
                         (process-exit-status proc))))

;;;; Shell command to buffer or echo area

(defun futur-shell-command (command &optional output-buffer)
  "Run COMMAND in a shell asynchronously, display output.

COMMAND is a shell command string.
OUTPUT-BUFFER is a buffer or buffer name; defaults to
`shell-command-buffer-name'.

Short output appears in the echo area via `display-message-or-buffer';
longer output in the buffer.

Returns a futur delivering the integer exit code.

Also see `futur-shell-async-command' for streaming output and
`futur-shell-command-to-string' for capturing output."
  (interactive
   (list (read-shell-command
          (if shell-command-prompt-show-cwd
              (format-message "Shell command in `%s': "
                              (abbreviate-file-name default-directory))
            "Shell command: ")
          nil nil
          (let ((filename (or buffer-file-name
                              (and (eq major-mode 'dired-mode)
                                   (dired-get-filename nil t)))))
            (and filename (file-relative-name filename))))))
  (let* ((buf (get-buffer-create
               (or output-buffer shell-command-buffer-name)))
         (directory default-directory))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory directory))
    (let ((fut
           (futur-let*
               ((exit-code
                 <- (futur-new
                     (lambda (f)
                       (let ((proc (start-process-shell-command
                                    "futur-shell-cmd" buf command)))
                         (set-process-thread proc nil)
                         (set-process-sentinel
                          proc
                          (lambda (p _signal)
                            (when (memq (process-status p) '(exit signal))
                              (futur-deliver-value
                               f (process-exit-status p)))))
                         proc)))))
             (with-current-buffer buf
               (setq-local revert-buffer-function
                           (lambda (&rest _)
                             (futur-shell-command command
                                                  (current-buffer))))
               (shell-command-set-point-after-cmd buf)
               (display-message-or-buffer buf))
             exit-code)))
      (when (called-interactively-p 'any)
        (futur-bind fut #'ignore))
      fut)))

;;;; Shell command to string

(defun futur-shell-command-to-string (command)
  "Run COMMAND in a shell asynchronously, return output as string.

COMMAND is a shell command string.

Unlike `shell-command-to-string', this does not block Emacs.
Returns a futur delivering the output string (not the exit code).

Also see `futur-shell-command' for displaying output and
`futur-shell-async-command' for streaming output."
  (let ((buf (generate-new-buffer " *futur-shell-to-string*")))
    (futur-let*
        ((_exit
          <- (futur-new
              (lambda (f)
                (let ((proc (start-process-shell-command
                             "futur-shell" buf command)))
                  (set-process-thread proc nil)
                  (set-process-sentinel
                   proc
                   (lambda (p _signal)
                     (when (memq (process-status p) '(exit signal))
                       (futur-deliver-value
                        f (process-exit-status p)))))
                  proc))))
         (output (with-current-buffer buf (buffer-string))))
      (kill-buffer buf)
      output)))

(provide 'futur-shell)
;;; futur-shell.el ends here
