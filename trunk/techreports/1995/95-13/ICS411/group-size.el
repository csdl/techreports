(defconst Pass1-reviewers
  '( ("jedwards"  7 11 12 20) ;Group1  --seq 0
     ("jflores" 3 11 12 19)
     ("mjuan" 17)
     
     ("casuncio" 11 18) ;Group2 --seq 3
     ("ccheung" )
     ("rnekoba" 5 7 9 11 12 18 20)
     
     ("shuang" 6 7 12 13 15 16 17 18 );Group5 --seq 6
     ("kittu" 5 7 8 11 12 17 18)
     ("luzhang" 18)

     ("hhwang" 5 7 12 13 16 17 ) ;Group6 --seq 9
     ("giang" 1 4 5 7 11 13 15 17 20)
     ("rshen" 5 7 11 12 13 15 17 20)

     ("hchen" 6 7 15) ;Group7  --seq 12
     ("jchen")
     ("richardy"  7 10 13 14 15 17 18 20 )

     ("mchae" 7 8 11 15 17 18) ;Group8
     ("shan" 2 6 7 8)
     ("rsato" 3 6 7 8 9 10 11 12 13 15 18 20)
     
     ("acosta" 9 20 );Group14  --seq 18
     ("donthi" )
     ("yanwang" 7 9 10 11 12 13 14 17)

     ("cwchan" 3 7 18 20);Group15 --seq 21
     ("pli" 7 11)
     ("ysiou" 2 7 11 17)

     )
  "List of users and error numbers caught")


(defconst Pass2-reviewers
  '( ("cchan" 3 5 11 12 16) ;Group3
     ("clam" 3 7 11 12 17)
     ("siushan" 3 4 7 9 12 16)


     ("sko" 3 12) ;Group4
     ("sanaka" 3 5 7 9 11 12 16)
     ("wagnerm" 7 12 15)
     
     ("horigan" 3 5 11 15 16 17 18) ;Group9
     ("lnohara" 4)
     ("lmuramot" 3 4 7 9 12 17 18)

     ("casem" 3 7 17) ;Group10
     ("briang" 1 3 6 7 11 12)
     ("cokumoto" )

     ("chasek" 1 3 5 6 7 11) ;Group11
     ("kiyuna" 3 7 11 13 17)
     ("msamson" 3 5 7 12)

     ("scheung" 3 7 11 17 19); Group12
     ("kawak" 3 7)
     ("awong" 3 5 7 11 16 17)


     ("defiesta" 3 7 12) ;Group13
     ("andyl" 3 4 6 7 8 11 15 16 17)
     ("wrodrigu" 1 3 4 11)

     ))

(defconst pass1-original-seq '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))

(defconst pass2-original-seq '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(defvar p1 nil)
(defvar p2 nil)

(defun generate-group (size)
  "Generate EIAM groups of SIZE from Pass1-reviewers and Pass2-reviewers
randomly. Results in p1 and p2."
  (setq p1 nil)
  (setq p2 nil)

  (let ((pass1-seq (copy-list pass1-original-seq))
	(pass2-seq (copy-list pass2-original-seq))
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

(defun error-percentage ()
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
	(setq error-data (append error-data (cdr (nth i Pass1-reviewers)))))
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
	(setq error-data (append error-data (cdr (nth i Pass2-reviewers)))))
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

    


  
  

    




