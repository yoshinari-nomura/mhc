(defvar mhc-process nil)

(add-to-list 'process-coding-system-alist '("^mhc$" . utf-8))

(defun mhc-db-scan (b e &optional nosort category)
  (let ((command nil))
    (unless (and (processp mhc-process)
                 (eq (process-status mhc-process) 'run))
      (mhc-start-process))
    (setq command (format "scan --format=emacs %04d%02d%02d-%04d%02d%02d%s"
                          (mhc-date-yy b)
                          (mhc-date-mm b)
                          (mhc-date-dd b)
                          (mhc-date-yy e)
                          (mhc-date-mm e)
                          (mhc-date-dd e)
                          (if category  (format " --category=%s" category) "")))
    (message "COMMAND: %s" command)
    (mhc-process-send-command command)))

(defun mhc-process-send-command (command)
  (with-current-buffer (process-buffer mhc-process)
    (delete-region (point-min) (point-max))
    (process-send-string mhc-process (concat command "\n"))
    (while (not (and (> (point-max) 1)
                     (eq (char-after (1- (point-max))) ?\n)))
      (accept-process-output mhc-process 0.5))
    ;; (message (format "%s"  (buffer-substring (point-min) (1- (point-max)))))
    (read (buffer-substring (point-min) (1- (point-max))))))

(defun mhc-start-process ()
  (interactive)
  (let ((base-dir (mhc-summary-folder-to-path mhc-base-folder)))
    (if (and (processp mhc-process)
             (eq (process-status mhc-process) 'run))
        (kill-process mhc-process))
    (setq mhc-process (start-process
                       "mhc"
                       (get-buffer-create " *mhc-scan-process*")
                       "mhc"
                       "server"
                       (format "--repository=%s" base-dir)))))
