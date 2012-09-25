(defvar data nil)

(defun collect (n)
  (interactive "nData items per line: ")
  (setq data nil)
  (let ((items nil)
	(i 0)
	item)
    ;;empty string "" should be replaced with "E" first
    (while (re-search-forward "[0-9CWE]+" nil t)
      (setq item (buffer-substring (match-beginning 0) (match-end 0)))
      (cond ((equal item "E")
	     (setq item nil))
	    ((or (equal item "C") (equal item "W"))
	     nil)
	    (t
	     (setq item (string-to-int item))))
      (cond ((< i n)
	     (pushend item items)
	     (incf i))
	    ((= i n)
	     (pushend items data)
	     (setq items (list item))
	     (setq i 1)
	     ))
      )
    (pushend items data)
    data))

(defun eiam-count-wrong ()
  "Count wrong issues and strong wrong 
 (with Criticality and Confidence-level >= 2)"
  (let ((n 0) 
	(w 0) 
	pos)
    (dolist (items data)
      (when (and (setq pos (position "W" items :test 'equal))
		 (incf w)		;count wrong issues
		 (>= (nth (+ pos 1) items) 2)
		 (>= (nth (+ pos 2) items) 2))
	(incf n)))			;count strongly wrong issues
    (list w n)))

(defun egsm-count-wrong ()
  (let ((n 0) 
	(w 0) 
	pos)
    (dolist (items data)
      (when (and (member "W" items)
		 (incf w)		;count wrong issues
		 (>= (nth 2 items) 2)
		 (>= (nth 3 items) 2)
		 (>= (nth 5 items) 2)
		 (>= (nth 6 items) 2)
		 (>= (nth 8 items) 2)
		 (>= (nth 9 items) 2))
	(incf n)))			;count strongly wrong issues
    (list w n)))
  

