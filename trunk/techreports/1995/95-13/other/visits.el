

(defconst Pass1
  '(("hextonum" . 27)
    ("Access_Symtab" . 58)
    ("Write_Int_File" . 36)
    ("P1_Read_Source" . 76)
    ("P1_Proc_START" . 30)
    ("P1_Proc_RESW" . 31)
    ("P1_Assign_Loc" . 29)
    ("P1_Assign_Sym" . 27)
    ("Pass_1" . 36)
    )
  "Table of Pass1 source node and the number of lines (without program specification)"
  )

(defconst Pass2
  '(("dectonum" . 21)
    ("Read_Int_File" . 59)
    ("P2_Search_Optab" . 28)
    ("P2_Proc_START" . 19)
    ("P2_Proc_BYTE" . 50)
    ("P2_Assemble_Inst" . 42)
    ("P2_Write_Obj" . 72)
    ("Pass_2" . 38)
    )
  "Table of Pass2 source node and the number of lines (without program specification)")

(defconst Employee1
  '(
    ("Company1::Company1" . 10)
    ("Employee::~Employee" . 9)
    ("Employee::setName" . 19)
    ("Company1::addEmployee" . 39)
    ("Employee::setSocSecurity" . 33)
    ("Company1::findEmployee" . 10)
    ("Employee::setAge" . 11)
    ("Company1::deleteEmployee" . 20)
    ("Employee::setNumDependents" . 11)
    ("Company1::print" . 12)
    ))

(defconst Employee2
  '(("Employee::~Employee" . 9)
    ("Employee::setName" . 11)
    ("Employee::setSocSecurity" . 27)
    ("Company2::~Company2" . 13)
    ("Employee::setAge" . 9)
    ("Company2::findEmployee" . 21)
    ("Employee::setNumDependents" . 10)
    ("Company2::addEmployee" . 32)
    ("Employee::print" . 9)
    ("Company2::deleteEmployee" . 23)
    ("Company2::print" . 18)
    ))

(defun visitation-all (l)
  "Returns an alist of number of visitation for all nodes in l"
  (let ((results nil)
	node-name n)
    (dolist (a l)
      (setq node-name (car a))
      (setq n (visits node-name))
      (pushend (cons node-name n) results))
    results))

(defun visits (node-name)
  "Returns number of visitations of node-name (activity-name = reading-node)
for all participants"
  (let ((n 0)
	(session-IDs (mt*{session}*IDs k*session*db-ID))
	)
    ;;collect those activities that involve reading-node for NODE-ID
    (mapc (function 
	   (lambda (activity-ID)
	     (when (and (eq 'reading-node (mt*activity*name k*session*db-ID activity-ID))
			(equal node-name (mt*activity*property k*session*db-ID activity-ID 'entity-name))
			(> (mt*activity*elapsed-time k*session*db-ID activity-ID) 10))
	       (incf n))))
	  (apply 'append (mapcar 
			  (function (lambda (session-ID)
				      (mt*session*activity-IDs k*session*db-ID session-ID)))
			  session-IDs)))
    n))
