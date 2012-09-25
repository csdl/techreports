;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; This program should only be run on the latex table containing paraphrasing
;;; data
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


(defconst Pass1
  '(;("Type and var declarations" . 101)
    ("hextonum" . 27)
    ("Access\\_Symtab" . 58)
    ("Write\\_Int\\_File" . 36)
    ("P1\\_Read\\_Source" . 76)
    ("P1\\_Proc\\_START" . 30)
    ("P1\\_Proc\\_RESW" . 31)
    ("P1\\_Assign\\_Loc" . 29)
    ("P1\\_Assign\\_Sym" . 27)
    ("Pass\\_1" . 36)
    )
  "Table of Pass1 source node and the number of lines (without program specification)"
  )

(defconst Pass2
  '(;("Type and var declarations" . 135)
    ("dectonum" . 21)
    ("Read\\_Int\\_File" . 59)
    ("P2\\_Search\\_Optab" . 28)
    ("P2\\_Proc\\_START" . 19)
    ("P2\\_Proc\\_BYTE" . 50)
    ("P2\\_Assemble\\_Inst" . 42)
    ("P2\\_Write\\_Obj" . 72)
    ("Pass\\_2" . 38)
    )
  "Table of Pass2 source node and the number of lines (without program specification)")

(defconst Employee1
  '(;("Constant" . 2)
    ("Employee" . 19)
    ("Company1" . 14)
    ("Employee::Employee" . 8)
    ("Employee::\\~Employee" . 9)
    ("Employee::setName" . 19)
    ("Employee::setSocSecurity" . 33)
    ("Employee::setAge" . 11)
    ("Employee::setNumDependents" . 11)
    ("Employee::print" . 8)
    ("Employee::getSocSecurity" . 5)
    ("Company1::Company1" . 10)
    ("Company1::\\~Company1" . 6)
    ("Company1::addEmployee" . 39)
    ("Company1::findEmployee" . 10)
    ("Company1::deleteEmployee" . 20)
    ("Company1::print" . 12)
    ))

(defconst Employee2
  '(("Employee" . 19)
    ("EmployeeNode" . 12)
    ("Company2" . 14)
    ("Employee::Employee" . 8)
    ("Employee::\\~Employee" . 9)
    ("Employee::setName" . 11)
    ("Employee::setSocSecurity" . 27)
    ("Employee::setAge" . 9)
    ("Employee::setNumDependents" . 10)
    ("Employee::print" . 9)
    ("Employee::getSocSecurity" . 5)
    ("EmployeeNode::EmployeeNode" . 6)
    ("EmployeeNode::\\~EmployeeNode" . 5)
    ("Company2::Company2" . 7)
    ("Company2::\\~Company2" . 13)
    ("Company2::findEmployee" . 21)
    ("Company2::addEmployee" . 32)
    ("Company2::deleteEmployee" . 23)
    ("Company2::print" . 18)
    ))


(defparameter review-data nil
  "An alist of (source-node user1 user2 ..).")

(defparameter paraphrasing-data nil
  "An alist of (source-node user1 user2 ..).")

(defun read-review-time (userno)
  "Read review time data. If USERNO is >0, read for the particular USERNO,
otherwise read for all users data. The data is parsed from the latex
document Review rate table.
USERNO starts from 1"
  (interactive "nUserno: ")
  (let ((cur (point))
	(newdata nil)
	entry item p last )
    (setq review-data nil)
    (re-search-forward "\\\\end")
    (setq last (point))
    (goto-char cur)
    (while (re-search-forward "([0-9]+)" last t)
      (goto-char(match-end 0))
      (setq p (point))
      (re-search-forward "&")
      (setq item (filter-whitespaces 
		  (buffer-substring p (match-beginning 0))))
      (setq entry (cons item nil))
      (while (not (looking-at "\\\\"))
	(re-search-forward "[0-9]+" nil t)
	(setq item (buffer-substring (match-beginning 0) (match-end 0)))
	(pushend (string-to-int item) entry))
      (pushend entry review-data)
      (forward-line 1)
      (beginning-of-line)
      )
    (when (> userno 0)
      (dolist (e review-data)
	(pushend (list (car e) (nth userno e)) newdata))
      (setq review-data newdata))
    (message "Done")
    ))

(defun calc-paraphrasing-rate (table)
  "Calculate paraphrasing rate in review-data using TABLE.
Output is in global variable paraphrasing data."
  (interactive "XTable: ")
  (let (lines source entry info rate)
    (setq paraphrasing-data nil)
    (dolist (data review-data)
      (setq source (car data))
      (when (setq info (assoc source table))
	(setq lines (cdr info)) ;number of lines in source
	(setq entry (cons source nil))
	(dolist (val (cdr data))  ;val = review time in seconds
	  ;;rate = lines / (val / 3600)
	  ;;rate = lines * 3600 / val (#lines/hour)
	  (setq rate (/ (* lines 3600) (float val)))
	  (pushend rate entry))
	(pushend entry paraphrasing-data)
	))))

  
(defun print-paraphrasing-rate ()
  "Print paraphrasing data in latex format in current buffer."
  (interactive)
  (insert "\\begin{table}[hb]\n"
	  "\\begin{center}\n"
	  "\\begin{tabular}{|"
	  (mapconcat '(lambda (&rest ignore)
			"l|")
		     (car paraphrasing-data)
		     "")
	  "}\n"
	  "\\hline\n"
	  "\n"
	  "\\hline\n")
  (dolist (entry paraphrasing-data)
    (insert (car entry) 
	    " & "
	    (mapconcat '(lambda (f)
			  (format "%3.1f" f))
		       (cdr entry)
		       " & "))
    (insert "\\\\\n"))
  (insert "\\hline\n"
	  "\\end{tabular}\n"
	  "\\end{center}\n"
	  "\\caption{Paraphrasing Rate (lines/hour)}\n"
	  "\\end{table}\n")
  )
    


(defun filter-whitespaces (str)
  "Remove any leading and trailing whitespaces in string STR, and returns
the resulting string."
  (let ((non-ws "[^ \t\f\n]")
        (end (1- (length str)))
        start (found t) c)
    (cond ((setq start (string-match non-ws str))
           ;;skip whitespaces backward
           (while (and found (> end start))
             (setq c (aref str end))
             (if (or (char-equal c ? )
                     (char-equal c ?\n)
                     (char-equal c ?\t)
                     (char-equal c ?\f))
                 (decf end)
               (setq found nil)))       ;non-whitespaces
           (substring str start (1+ end)))
          (t ""))                       ;empty string
    ))




