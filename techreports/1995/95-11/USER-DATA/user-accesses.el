(require 'ecs)

(defun www*www-ics*split-stats-by-domain ()

  "This function parses the www log files and records entries for specified
domains in separate files."

  (let ((logfiles '("0195.log" "0295.log" "0395.log" "0495.log"))
	(domains '("quark" "larry" "moe" "sonoma" "nada" 
		   "zilch" "nolo" "zip" "zero" "curly"))
	domain-name
	start-pos end-pos
	log-item)

    (dolist (logfile logfiles)
      (find-file (concat "~www/logs/" logfile))
      (beginning-of-buffer)
      (save-excursion
	(dolist (domain domains)
	  (setq domain-name (concat domain ".ics.hawaii.edu"))
	  (find-file (concat "~/WWW/USER-DATA/" domain))
	  (switch-to-buffer logfile)
	  (beginning-of-buffer)
	  (while (search-forward domain-name (point-max) t)
	      (progn
		(beginning-of-line)
		(setf start-pos (point))
		(end-of-line)
		(setf end-pos (point))
		(setf log-item (buffer-substring start-pos end-pos))
		(switch-to-buffer domain)
		(end-of-buffer)
		(insert (concat log-item "\n"))
		(switch-to-buffer logfile)))
	  (switch-to-buffer domain)
	  (save-buffer)
	  (kill-buffer domain)
	  (message (concat "Done processing " domain " in " logfile "."))))
      (kill-buffer logfile)
      (message (concat "Finished with " logfile ".")))))

(defun www*www-ics*separate-domain-sessions ()

  "This function parses the logfile for each domain name and separates sessions
based on 5 minutes of idle time."

  (let ((domains '("quark" "larry" "moe" "sonoma" "nada" 
		   "zilch" "nolo" "zip" "zero" "curly"))
	start-pos end-pos
	day month year hour minute seconds
	previous current)

    (dolist (domain domains)
      (save-excursion
	(find-file (concat "~/WWW/USER-DATA/" domain))
	(setq domain-name (concat domain ".ics.hawaii.edu"))
	(setq previous nil)
	(while (search-forward domain-name (point-max) t)
	  (progn
	    (setq current (www*www-ics*make-date))
	    (cond (previous
		   (cond ((>= (u*date*difference current previous) 300)
			  (beginning-of-line)
			  (insert "\n")
			  (end-of-line)))))
	    (setq previous current)))
	(save-buffer)))))

(defun www*www-ics*make-date ()

  "This function parses a logitem in a file and returns a u*date instance for
that file."

  (search-forward "[" (point-max) t)
  (setq start-pos (point))
  (setq end-pos (+ (point) 2))
  (setq day (buffer-substring start-pos end-pos))
  (setq start-pos (+ (point) 3))
  (setq end-pos (+ (point) 6))
  (setq month (buffer-substring start-pos end-pos))
  (setq start-pos (+ (point) 7))
  (setq end-pos (+ (point) 11))
  (setq year (buffer-substring start-pos end-pos))
  (setq start-pos (+ (point) 12))
  (setq end-pos (+ (point) 14))
  (setq hour (buffer-substring start-pos end-pos))
  (setq start-pos (+ (point) 15))
  (setq end-pos (+ (point) 17))
  (setq minute (buffer-substring start-pos end-pos))
  (setq start-pos (+ (point) 18))
  (setq end-pos (+ (point) 20))
  (setq seconds (buffer-substring start-pos end-pos))
  (u*date*make year month day hour minute seconds))
	    

