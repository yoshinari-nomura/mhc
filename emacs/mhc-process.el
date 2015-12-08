(require 'mhc-vars)

(defvar mhc-process nil)

(defun mhc-process-send-command (command)
  (unless (and (processp mhc-process)
               (eq (process-status mhc-process) 'run))
    (mhc-start-process))
  (message "COMMAND: %s" command)
  (with-current-buffer (process-buffer mhc-process)
    (delete-region (point-min) (point-max))
    (process-send-string mhc-process (concat command "\n"))
    (let ((i 1))
      (while (not (and (> (point-max) 1)
                       (eq (char-after (1- (point-max))) ?\n)))
        (message (format "Waiting mhc process...%d" i))
        (setq i (1+ i))
        (accept-process-output mhc-process 0.5)))
    (read (buffer-substring (point-min) (1- (point-max))))))

(defun mhc-start-process ()
  (interactive)
  (let ((process-connection-type nil)) ;; use PIPE not tty
    (if (and (processp mhc-process)
             (eq (process-status mhc-process) 'run))
        (kill-process mhc-process))
    (setq mhc-process
	  (apply 'start-process
		 (delq nil `("mhc"
			     ,(get-buffer-create " *mhc-scan-process*")
			     ,mhc-ruby-program-name
			     ,mhc-program-name
			     "server"))))
    (set-process-coding-system mhc-process 'utf-8 'utf-8)
    (set-process-query-on-exit-flag mhc-process nil)
    mhc-process))

(provide 'mhc-process)
