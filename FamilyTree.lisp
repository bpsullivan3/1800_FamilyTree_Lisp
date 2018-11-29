;;;; -*- Mode: Lisp; -*- 

(defstruct person
  (name nil)
  (parent1 nil)
  (parent2 nil)
  (spouses (list))
  (children (list)))

(defun storeperson (symbol-name struct family-tree)
  "Enter the person structure in STRUCT into the hashtable in FAMILY-TREE with the key in SYMBOL-NAME."
  (setf (gethash symbol-name family-tree) struct))

(defun personstruct (symbol-name family-tree)
  "Returns a structure of type person corresponding to the key SYMBOL-NAME in the hashtable FAMILY-TREE. If there is no one in the tree with the name in SYMBOL-NAME, returns NIL."
  (gethash symbol-name family-tree nil))

(defun addspouse (spouse-name p)
  (setf (person-spouses p) (cons spouse-name (person-spouses p)))
  (setf (person-spouses p) (sort (person-spouses p) #'string<)))

(defun addchild (child-name p)
  (setf (person-children p) (cons child-name (person-children p)))
  (setf (person-children p) (sort (person-children p) #'string<)))

(defun ancestors (p tree)
  "Returns a list of symbol-names of all the ancestors of P in TREE."
     (let ((parent1 (personstruct (person-parent1 p) tree))
           (parent2 (personstruct (person-parent2 p) tree)))
     (when parent1
         (sort (delete-duplicates (append (list (person-name parent1) (person-name parent2))
                 (ancestors parent1 tree)
                 (ancestors parent2 tree)) :test #'equal) #'string<))))

(defun siblings (p tree)
  "Returns a list of symbol-names of all the siblings of P in TREE"
  (let ((parent1 (personstruct (person-parent1 p) tree))
        (parent2 (personstruct (person-parent2 p) tree)))
    (when parent1
       (sort (remove (person-name p) 
                     (delete-duplicates 
                     (append (person-children parent1)(person-children parent2)) :test #'equal)) #'string<))))

(defun ischild (p1 p2)
  "Returns a boolean value: True (t) if p1 is a child of p2, else False (nil)"
  (let ((child nil))
  (if (member (person-name p1) (person-children p2) :test #'equal) (setf child t))
  (if child t nil))) 

(defun isspouse (p1 p2)
  "Returns a boolean value: True (t) if p1 is a spouse of p2, else False (nil)"
  (let ((spouse nil))
  (if (member (person-name p1) (person-spouses p2) :test #'equal) (setf spouse t))
  (if spouse t nil)))

(defun isancestor(p1 p2 tree)
  (let ((ancestor nil))
  (if (member (person-name p1) (ancestors p2 tree) :test #'equal) (setf ancestor t))
  (if ancestor t nil)))

(defun issibling(p1 p2 tree)
  (let ((sibling nil))
  (if (member (person-name p1) (siblings p2 tree) :test #'equal) (setf sibling t))
  (if sibling t nil)))

(defun iscousin(p1 p2 tree)
  (let ((direct nil) (cousin nil))
  (if (string= (person-name p1) (person-name p2)) (setf direct t))
  (if (or (ischild p1 p2) (ischild p2 p1)) (setf direct t))
  (let ((ancestors1 (ancestors p1 tree)) (ancestors2 (ancestors p2 tree)))
  (if (or (member (person-name p1) ancestors2) (member (person-name p2) ancestors1)) (setf direct t))
  (when (not direct) ;if all of the above tests passed, then proceed to check for common ancestors
    (loop for p in ancestors1 doing (if (member p ancestors2 :test #'equal) (setf cousin t)))))
  (if cousin t nil)))

(defun isunrelated(p1 p2 tree)
  (let ((unrelated t))
  (if (or (ischild p1 p2) (ischild p2 p1)) (setf unrelated nil))
  (if (issibling p1 p2 tree) (setf unrelated nil))
  (if (iscousin p1 p2 tree) (setf unrelated nil))
  (if (or (isancestor p1 p2 tree) (isancestor p2 p1 tree)) (setf unrelated nil))
  (if unrelated t nil)))

(defun getcousins(p tree)
  (sort (remove nil (loop for v being the hash-values of tree 
        collecting (if (iscousin p v tree) (person-name v)))) #'string<))

(defun getunrelated(p tree)
  (sort (remove nil (loop for v being the hash-values of tree 
        collecting (if (isunrelated p v tree) (person-name v)))) #'string<))

(defun child-event (parent1 parent2 child tree) 
  "Create & Marry the parent1 and parent2 if they arent already, then create child with those parents if child does not exist"
  ;(format t "E ~a ~a ~a~%~%" parent1 parent2 child)
  (let ((p1 (personstruct parent1 tree)) (p2 (personstruct parent2 tree)) (c (personstruct child tree)))
     (if (not p1) (setf p1 (storeperson parent1 (make-person :name parent1) tree)))
     (if (not p2) (setf p2 (storeperson parent2 (make-person :name parent2) tree)))
     (if (not (isspouse p1 p2)) (addspouse (person-name p1) p2))
     (if (not (isspouse p2 p1)) (addspouse (person-name p2) p1))
     (when (not c) ; Do not make the child if it already exists 
        (setf c (storeperson child (make-person :name child :parent1 parent1 :parent2 parent2) tree))
        (addchild (person-name c) p1)
        (addchild (person-name c) p2))))

(defun marriage-event (person1 person2 tree) 
  "Marry person1 and person2, initializing them into existence if needed."
  ;(format t "E ~a ~a~%~%" person1 person2) 
  (let ((p1 (personstruct person1 tree)) (p2 (personstruct person2 tree)))
    (if (not p1) (setf p1 (storeperson person1 (make-person :name person1) tree)))
    (if (not p2) (setf p2 (storeperson person2 (make-person :name person2) tree)))
    (if (not (isspouse p1 p2)) (addspouse (person-name p1) p2))
    (if (not (isspouse p2 p1)) (addspouse (person-name p2) p1))))

(defun get-all-event (relation person tree)
  "Print out the query followed by a list of all people that are of the specified relation to person, one line at a time."
  (format t "W ~a ~a~%" relation person)
  (let ((p (personstruct person tree)))
     (cond ((string= relation "SPOUSE") 
               (loop for i in (person-spouses p) doing (format t "~a~%" i)))
           ((string= relation "CHILD")
               (loop for i in (person-children p) doing (format t "~a~%" i)))
           ((string= relation "ANCESTOR")
               (loop for i in (ancestors p tree) doing (format t "~a~%" i)))
           ((string= relation "SIBLING")
               (loop for i in (siblings p tree) doing (format t "~a~%" i)))
           ((string= relation "COUSIN")
               (loop for i in (getcousins p tree) doing (format t "~a~%" i)))
           ((string= relation "UNRELATED")
               (loop for i in (getunrelated p tree) doing (format t "~a~%" i))))
  (format t "~%")))

(defun is-a-event (person1 relation person2 tree) 
  "Check if person1 is related to person2 by a specified relation. Prints out the query followed by 'Yes' or 'No'"
  (format t "X ~a ~a ~a~%" person1 relation person2)
  (let ((p1 (personstruct person1 tree)) (p2 (personstruct person2 tree)))
     (cond ((string= relation "SPOUSE") 
               (if (isspouse p1 p2) (format t "Yes~%") (format t "No~%")))
           ((string= relation "CHILD")
              (if (ischild p1 p2) (format t "Yes~%") (format t "No~%")))
           ((string= relation "ANCESTOR")
               (if (isancestor p1 p2 tree) (format t "Yes~%") (format t "No~%")))
           ((string= relation "SIBLING")
              (if (issibling p1 p2 tree) (format t "Yes~%") (format t "No~%")))
           ((string= relation "COUSIN")
               (if (iscousin p1 p2 tree) (format t "Yes~%") (format t "No~%")))
           ((string= relation "UNRELATED")
               (if (isunrelated p1 p2 tree) (format t "Yes~%") (format t "No~%"))))
  (format t "~%")))

(defun family ()
  "This is the top-level function for the whole Lisp program."
  (let ((tree (make-hash-table :size 1000 :test #'equal)) (in *standard-input*))
    (when in
      (loop for line = (read in nil)
        while line doing
          (cond ((and (string= (first line) "E") (fourth line)) 
		    (child-event (second line) (third line) (fourth line) tree))
	        ((and (string= (first line) "E") (not (fourth line)))
		    (marriage-event (second line) (third line) tree))
                ((string= (first line) "W") (get-all-event (second line) (third line) tree))
                ((string= (first line) "X") (is-a-event (second line) (third line) (fourth line) tree)))))))
