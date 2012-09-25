(defconst egsm-p1 
  '( ("G3" 5 6 7 8 9 11 12 13 15 18)
     ("G4"  5 6 7 11 12)
     ("G9" 2 5 6 7 8 11 12 15 16 17 18 19)
     ("G10" 1 5 6 7 10 15 16 17 18)
     ("G11" 1 5 7 11 12 13 17)
     ("G12" 5 6 7 8 10 11 15 16 17 20)
     ("G13" 2 3 4 6 7 9 11 15 17)
     ))

(defconst eiam-p1
  '( ("G1" 3 7 11 12 17 19 20)
     ("G2" 5 7 9 11 12 18 20)
     ("G5" 5 6 7 8 11 12 13 15 16 17 18)
     ("G6" 1 4 5 7 11 12 13 15 16 17 20)
     ("G7" 6 7 10 13 14 15 17 18 20)
     ("G8"  2 3 6 7 8 9 10 11 12 13 15 17 18 20)
     ("G14" 7 9 10 11 12 13 14 17 20)
     ("G15"  2 3 7 11 17 18 20)
     ))

(defconst egsm-p2
  '( ("G1"  1 3 7 11 12 13)
     ("G2" 3 4 5 7 8 12 18)
     ("G5" 1 3 4 5 7 8 9 11 12 14 17 18)
     ("G6" 3 4 5 12 13 14 15 16 17)
     ("G7" 1 3 4 7 11 12 17 18)
     ("G8" 3 4 5 7 11 12 13 15 17 18)
     ("G14" 3 4 7 10 11 12 14 16 18)
     ("G15" 1 3 7 11 13 16)
     ))

(defconst eiam-p2
  '( ("G3" 3 4 5 7 9 11 12 16 17)
     ("G4" 3 5 7 9 11 12 15 16)
     ("G9" 3 4 5 7 9 11 12 15 16 17 18)
     ("G10" 1 3 6 7 11 12 17)
     ("G11" 1 3 5 6 7 11 12 13 17)
     ("G12" 3 5 7 11 16 17 19)
     ("G13" 1 3 4 6 7 8 11 12 15 16 17)
     ))

(defconst all-p1 (append egsm-p1 eiam-p1))
(defconst all-p2 (append egsm-p2 eiam-p2))

(defconst all-p1-seq '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(defconst all-p2-seq '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))

(defconst p1-egsm-seq '(0 1 2 3 4 5 6))
(defconst p2-egsm-seq '(0 1 2 3 4 5 6 7))

(defconst p1-eiam-seq '(0 1 2 3 4 5 6 7))
(defconst p2-eiam-seq '(0 1 2 3 4 5 6))

(defvar p1 nil)
(defvar p2 nil)

(defun generate-egsm-group (size) 
  "Generate EGSM groups of SIZE from egsm-p1 and egsm-p2 randomly. 
SIZE = N-fold. Results in p1 and p2." 
  (setq p1 nil)
  (setq p2 nil)

  (let ((pass1-seq (copy-list p1-egsm-seq))
	(pass2-seq (copy-list p2-egsm-seq))
	i l selected)
    ;;initialize random seed
    (random t)
    ;;process pass1
    (while (>= (length pass1-seq) size)
      (setq selected nil)
      (dotimes (j size)
	(setq i (random (length pass1-seq)))
	(pushend (nth i pass1-seq) selected)
	;;now take out the element
	(setq l (nthcdr i pass1-seq))
	(setf (nthcdr i pass1-seq) (cdr l)))
      (pushend selected p1))
    
    ;;process pass2
    (while (>= (length pass2-seq) size)
      (setq selected nil)
      (dotimes (j size)
	(setq i (random (length pass2-seq)))
	(pushend (nth i pass2-seq) selected)
	;;now take out the element
	(setq l (nthcdr i pass2-seq))
	(setf (nthcdr i pass2-seq) (cdr l)))
      (pushend selected p2))
    t))

(defun egsm-error-percentage ()
  "Returns error percentage from list p1 and p2"
  (let ((p1-errors nil) ;list of errors per selected group
	(p2-errors nil)
	(error-data nil)
	(p1-results nil) ;list of percentage errors caught per selected group
	(p2-results nil)
	(total 0.0)
	)
    ;;process p1
    (dolist (e p1)
      (setq error-data nil)
      (dolist (i e)
	(setq error-data (append error-data (cdr (nth i egsm-p1)))))
      (setq error-data (remove-duplicates error-data))
      (pushend error-data p1-errors))
    ;;now calculates percentage
    ;;Pass1 has 20 errors
    (dolist (e p1-errors)
      (pushend (/ (length e) 20.0) p1-results))

    ;;process p2
    (dolist (e p2)
      (setq error-data nil)
      (dolist (i e)
	(setq error-data (append error-data (cdr (nth i egsm-p2)))))
      (setq error-data (remove-duplicates error-data))
      (pushend error-data p2-errors))
    ;;now calculates percentage
    ;;Pass2 has 19 errors
    (dolist (e p2-errors)
      (pushend (/ (length e) 19.0) p2-results))
    
    ;;now average p1 and p2 results
    (setq total (apply '+ p1-results))
    (setq total (apply '+ total p2-results))
    (/ total (+ (length p1-results) (length p2-results)))
    ))

(defun generate-eiam-group (size) 
  "Generate EIAM groups of SIZE from eiam-p1 and eiam-p2 randomly. 
SIZE = N-fold. Results in p1 and p2." 
  (setq p1 nil)
  (setq p2 nil)

  (let ((pass1-seq (copy-list p1-eiam-seq))
	(pass2-seq (copy-list p2-eiam-seq))
	i l selected)
    ;;initialize random seed
    (random t)
    ;;process pass1
    (while (>= (length pass1-seq) size)
      (setq selected nil)
      (dotimes (j size)
	(setq i (random (length pass1-seq)))
	(pushend (nth i pass1-seq) selected)
	;;now take out the element
	(setq l (nthcdr i pass1-seq))
	(setf (nthcdr i pass1-seq) (cdr l)))
      (pushend selected p1))
    
    ;;process pass2
    (while (>= (length pass2-seq) size)
      (setq selected nil)
      (dotimes (j size)
	(setq i (random (length pass2-seq)))
	(pushend (nth i pass2-seq) selected)
	;;now take out the element
	(setq l (nthcdr i pass2-seq))
	(setf (nthcdr i pass2-seq) (cdr l)))
      (pushend selected p2))
    t))

(defun eiam-error-percentage ()
  "Returns error percentage from list p1 and p2"
  (let ((p1-errors nil) ;list of errors per selected group
	(p2-errors nil)
	(error-data nil)
	(p1-results nil) ;list of percentage errors caught per selected group
	(p2-results nil)
	(total 0.0)
	)
    ;;process p1
    (dolist (e p1)
      (setq error-data nil)
      (dolist (i e)
	(setq error-data (append error-data (cdr (nth i eiam-p1)))))
      (setq error-data (remove-duplicates error-data))
      (pushend error-data p1-errors))
    ;;now calculates percentage
    ;;Pass1 has 20 errors
    (dolist (e p1-errors)
      (pushend (/ (length e) 20.0) p1-results))

    ;;process p2
    (dolist (e p2)
      (setq error-data nil)
      (dolist (i e)
	(setq error-data (append error-data (cdr (nth i eiam-p2)))))
      (setq error-data (remove-duplicates error-data))
      (pushend error-data p2-errors))
    ;;now calculates percentage
    ;;Pass2 has 19 errors
    (dolist (e p2-errors)
      (pushend (/ (length e) 19.0) p2-results))
    
    ;;now average p1 and p2 results
    (setq total (apply '+ p1-results))
    (setq total (apply '+ total p2-results))
    (/ total (+ (length p1-results) (length p2-results)))
    ))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; All
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun generate-all-group (size)
  "Generate group number of SIZE from all-p1 and all-p2 randomly. 
SIZE = N-fold. Results in p1 and p2." 
  (setq p1 nil)
  (setq p2 nil)

  (let ((pass1-seq (copy-list all-p1-seq))
	(pass2-seq (copy-list all-p2-seq))
	i l selected)
    ;;initialize random seed
    (random t)
    ;;process pass1
    (while (>= (length pass1-seq) size)
      (setq selected nil)
      (dotimes (j size)
	(setq i (random (length pass1-seq)))
	(pushend (nth i pass1-seq) selected)
	;;now take out the element
	(setq l (nthcdr i pass1-seq))
	(setf (nthcdr i pass1-seq) (cdr l)))
      (pushend selected p1))
    
    ;;process pass2
    (while (>= (length pass2-seq) size)
      (setq selected nil)
      (dotimes (j size)
	(setq i (random (length pass2-seq)))
	(pushend (nth i pass2-seq) selected)
	;;now take out the element
	(setq l (nthcdr i pass2-seq))
	(setf (nthcdr i pass2-seq) (cdr l)))
      (pushend selected p2))
    t))

(defun all-error-percentage ()
  "Returns error percentage from list p1 and p2"
  (let ((p1-errors nil) ;list of errors per selected group
	(p2-errors nil)
	(error-data nil)
	(p1-results nil) ;list of percentage errors caught per selected group
	(p2-results nil)
	(total 0.0)
	)
    ;;process p1
    (dolist (e p1)
      (setq error-data nil)
      (dolist (i e)
	(setq error-data (append error-data (cdr (nth i all-p1)))))
      (setq error-data (remove-duplicates error-data))
      (pushend error-data p1-errors))
    ;;now calculates percentage
    ;;Pass1 has 20 errors
    (dolist (e p1-errors)
      (pushend (/ (length e) 20.0) p1-results))

    ;;process p2
    (dolist (e p2)
      (setq error-data nil)
      (dolist (i e)
	(setq error-data (append error-data (cdr (nth i all-p2)))))
      (setq error-data (remove-duplicates error-data))
      (pushend error-data p2-errors))
    ;;now calculates percentage
    ;;Pass2 has 19 errors
    (dolist (e p2-errors)
      (pushend (/ (length e) 19.0) p2-results))
    
    ;;now average p1 and p2 results
    (setq total (apply '+ p1-results))
    (setq total (apply '+ total p2-results))
    (/ total (+ (length p1-results) (length p2-results)))
    ))
