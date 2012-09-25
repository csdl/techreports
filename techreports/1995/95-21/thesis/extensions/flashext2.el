
(require 'flashmail)

(defun flash*exp*record (db-ID)
  (unless (itimerp (get-itimer (format flash*exp!format db-ID)))
    (start-itimer (format flash*exp!update-timer-name db-ID)
		  'flash*exp*write-data 
		  flash*exp*update-interval
		  flash*exp*update-interval)))


(defun* flash*user*make (&key db-ID host-name connect-socket user-ID password)
  "Attempts to connect USER-ID to Flashmail HBS server.
DB-ID, HOST-NAME, and CONNECT-SOCKET identify the Flashmail HBS server.
Sets up Flashmail menu and returns DB-ID if successful.
Returns DB-ID if connection to flashmail succeeds, nil if already 
connected, or Flashmail server is down, or some other problem.
PASSWORD is an optional string containing the password to the flashmail database.
If not supplied, it will be prompted for interactively.
USER-ID defaults to user-login-name."

  (cond ((equal db-ID (u*error*with-complete-trapping #'identity
		       (s*db*make :db-ID db-ID
				  :connect-socket connect-socket
				  :host-name host-name
				  :password password
				  :client-name (or user-ID (user-login-name)))))
	 (flash*exp*record db-ID)
	 (u*debug*hide-buffers)
	 (u*process*hide-buffers)
	 (s*db*set-property db-ID 'users (s*{user}*connected-IDs db-ID)) 
	 (pushnew 'flash*user*connected-users-hook s*db*generic-event-hooks)
	 (flash*user*set-host db-ID (s*db*current-client db-ID)
			      (funcall flash*user*get-host-fn))
	 (unless (itimerp (get-itimer (format flash*db!format db-ID)))
	   (start-itimer (format flash*db!update-timer-name db-ID) 
		       (` (lambda () (flash*user*update '(, db-ID))))
		       flash*user*update-interval  
		       flash*user*update-interval))
	 (flash*user*update db-ID)
	 (message "")
	 db-ID)
	(t
	 (s*db@clear-local-state db-ID)
	 (message "")
	 nil)))

(defun flash*exp*write-data ()
  (save-excursion
    (set-buffer (get-buffer-create "testt"))
    (dolist (dbs (s*{db}*IDs))
      (dolist (nodes (s*{node}*IDs dbs))
	(insert (s*node*data dbs nodes))
	(newline)
	(s*node*delete dbs nodes)))
    (append-to-file (point-min) (point-max) "/home/3/jgeis/extensions/newexp")
    (kill-buffer "testt"))
  (flash*exp*logged-in))


(defun flash*exp*logged-in ()
  (save-excursion
    (set-buffer (get-buffer-create "testtwo"))
    (dolist (dbs (s*{db}*IDs))
      (insert (prin1-to-string (s*{user}*connected-IDs dbs)))
      (insert (current-time-string))
      (newline)
      (append-to-file (point-min) (point-max) "/home/3/jgeis/extensions/loggedin")
      (kill-buffer "testtwo"))))


(defvar flash*exp!format "flashtimer-%s")

(defvar flash*exp!update-timer-name "flash*exp!update-timer-%s")

(defvar flash*exp*update-interval 3600)








