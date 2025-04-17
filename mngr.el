;; mngr

(defvar mngr--date-only-format "%Y%m%d"
  "The format for mngr Dates. Being `YYYYMMDD'.")

(defvar mngr--time-only-format "%H%M"
  "The format for mngr Times. Being `HHMM'.")

(defvar mngr--date-with-time-format "%Y%m%d-%H%M"
  "The format for mngr Dates with times. Being `YYYYMMDD-HHMM'.")

(defvar mngr-projects-dir "~/mngr-testing"
  "Location of mngr files.")

(defvar mngr--current-clock nil
  "Location of current clocked in task")

(defvar mngr--mode-line-clock-status nil
  "String to display in the mode-line when clocked in.")

(defun mngr--sanitise-path (path)
  "Ensure paths end with `/'."
  (if (string-suffix-p "/" path)
      path
    (concat path "/")))

(defun mngr-jump-to-current-clock ()
  "Jump to the buffer and position of the last clock-in marker."
  (interactive)
  (if (and mngr--current-clock (marker-buffer mngr--current-clock))
      (progn
        (switch-to-buffer (marker-buffer mngr--current-clock))
        (goto-char mngr--current-clock))
    (message "No clock currently active")))

(defun mngr-clock-in ()
  "Clock into task at point. If no empty line is found, insert a new line at end
of buffer."
  (interactive)
  (if mngr--current-clock
      (progn
        (mngr-jump-to-current-clock)
        (message "Already clocked in."))
    (let ((found (re-search-forward "^[ \t]*$" nil t))
	  (clock-in-time (format-time-string mngr--date-with-time-format)))
      (if found
          (beginning-of-line)
        ;; No empty line found — insert at end of buffer
        (goto-char (point-max))
        (unless (bolp) ;; If not already at beginning of line, insert newline
          (newline)))
      (insert (concat " ~" clock-in-time ">"))
      (setq mngr--current-clock (copy-marker (point)))
      (newline))))

(defun mngr-clock-out ()
  "Move to and clock out of current clock"
  (interactive)
  (if mngr--current-clock
      (progn
	(mngr-jump-to-current-clock)
	(insert (format-time-string mngr--date-with-time-format))
	(setq mngr--current-clock nil))
    (message "Not currently clocked in")))

(defun mngr--get-match-data-today ()
  "Return a list of snippets from .mngr files in `mngr-projects-dir` that contain today's scheduled date.
Each snippet includes client, project metadata, task, and other details for every match."
  (let* ((sanitised-dir (mngr--sanitise-path mngr-projects-dir))
         (date-str (format-time-string mngr--date-only-format))
         (schedule-regex (concat "@" date-str "\\(-[0-9]\\{4\\}\\)?\\b"))
         (mngr-files (directory-files-recursively sanitised-dir "\\.mngr\\'"))
         (snippets '()))
    (dolist (file mngr-files snippets)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward schedule-regex nil t)
          (let* ((match-line (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))
                 (client (file-name-sans-extension (car (last (split-string file "/")))))
                 ;; Grab the line above and metadata using save-excursion
                 (task (save-excursion
                         (forward-line -1)
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
                 (project (save-excursion
                            (when (re-search-backward "^/" nil t)
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))
                 (project-metadata (save-excursion
                                     (when (re-search-backward "^/" nil t)
                                       (forward-line 1)
                                       (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))))
                 (snippet (list client project-metadata project task match-line)))
            (push snippet snippets)))))))

(defun mngr--format-overview-string (data)
  "Formats and returns an overview string"
  (let ((client (first data))
	(contract-client (when (string-match "&[^ \t\n]+" (second data))
			   (match-string 0 (second data))))
	(project (third data))
	(task (fourth data))
	(estimate (when (string-match "<[^ \t\n]+" (car (last data)))
		    (match-string 0 (car (last data)))))
	(time (when (string-match "@[0-9]\\{8\\}-\\([0-9]\\{4\\}\\)\\b" (car (last data)))
		(match-string 1 (car (last data))))))
    (let ((parts (delq nil
                       (list project
                             task
                             "\n"
                             (when contract-client contract-client)
                             (when client (concat "&" client))
                             (when time (concat "@" time))
                             estimate))))
      (string-join parts " "))))

;; (let ((parts (delq nil
;;                    (list (propertize project 'face '(:weight bold :foreground "deep sky blue"))
;;                          (propertize task 'face '(:foreground "white"))
;;                          "\n"
;;                          (when contract-client
;;                            (propertize contract-client 'face '(:foreground "light green")))
;;                          (when client
;;                            (propertize (concat "&" client) 'face '(:foreground "orange")))
;;                          (when time
;;                            (propertize (concat "@" time) 'face '(:foreground "gold")))
;;                          (when estimate
;;                            (propertize estimate 'face '(:foreground "gray")))))))
;;   (string-join parts " "))

(defun mngr-overview-today ()
  "Format and display today's scheduled tasks in a buffer called *today*."
  (interactive)
  (let* ((raw-data (mngr--get-match-data-today)) ;; List of lists of strings
         (formatted-entries (mapcar #'mngr--format-overview-string raw-data))

         ;; Separate entries with and without @HHMM
         (with-time '())
         (without-time '()))

    ;; Categorize entries
    (dolist (entry formatted-entries)
      (if (string-match "@\\([0-9]\\{4\\}\\)\\b" entry)
          (push entry with-time)
        (push entry without-time)))

    ;; Sort entries with time by the numeric value of HHMM
    (setq with-time
          (sort with-time
                (lambda (a b)
                  (let ((time-a (string-to-number (match-string 1 (string-match "@\\([0-9]\\{4\\}\\)" a))))
                        (time-b (string-to-number (match-string 1 (string-match "@\\([0-9]\\{4\\}\\)" b)))))
                    (< time-a time-b)))))

    ;; Combine
    (let ((final-entries (append with-time (nreverse without-time))))
      (with-current-buffer (get-buffer-create "*today*")
        (erase-buffer)
	(insert "Overview for today\n\n\n")
        (dolist (entry final-entries)
          (insert entry "\n\n"))
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))))




