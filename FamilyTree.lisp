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
  (setf (person-spouses p) (cons spouse-name (person-spouses p))))

(defun addchild (child-name p)
  (setf (person-children p) (cons child-name (person-children p))))

(defun ancestors (p tree)
  "Returns a list of symbol-names of all the ancestors of P in TREE."
   (let ((parent1 (personstruct (person-parent1 p) tree))
         (parent2 (personstruct (person-parent2 p) tree)))
     (when parent1
         (append (list (person-name parent1) (person-name parent2))
                 (ancestors parent1 tree)
                 (ancestors parent2 tree))))
   (setf ancestors (delete-duplicates ancestors :test #'equal)))

(defun siblings (p tree)
  "Returns a list of symbol-names of all the siblings of P in TREE"
  (let ((parent1 (personstruct (person-parent1 p) tree))
        (parent2 (personstruct (person-parent2 p) tree)))
    (when parent1
      (append (loop for p in parent1-children collecting (person-name p))
              (loop for p in parent2-children collecting (person-name p)))))
    (setf siblings (delete-duplicates siblings :test #'equal))
    (setf siblings (remove (person-name p) siblings)))

(defun ischild (p1 p2)
  "Returns a boolean value: True (t) if p1 is a child of p2, else False (nil)"
  (if (member (person-name p1) (person-children p2) :test #'equal) t nil)) 

(defun isspouse (p1 p2)
  "Returns a boolean value: True (t) if p1 is a spouse of p2, else False (nil)"
  (if (member (person-name p1) (person-spouses p2) :test #'equal) t nil)) 

(defun isancestor(p1 p2)
  (if (member (person-name p1) (ancestors p2 tree) :test #'equal) t nil))

(defun issibling(p1 p2)
  (if (member (person-name p1) (siblings p2 tree) :test #'equal) t nil))

(defun iscousin(p1 p2 tree)
  (setf cousin nil)
  (setf direct nil)
  (if (string= (person-name p1) (person-name p2)) (setf direct t) nil)
  (if (or (ischild(p1 p2)) (ischild(p2 p1))) (setf direct t) nil)
  (setf ancestors1 (ancestors p1 tree))
  (setf ancestors2 (ancestors p2 tree))
  (if (or (member (person-name p1) ancestors2) (member (person-name p2) ancestors1)) (setf direct t) nil)
  (when (not direct) ;if all of the above tests passed, then proceed to check for common ancestors
    (loop for p in ancestors1 doing (if (member (person-name p) ancestors2) (setf cousin t) nil)))
  (cousin))

(defun isunrelated(p1 p2 tree)
  (setf unrelated t)
  (if (or (ischild(p1 p2)) (ischild(p2 p1))) (setf unrelated nil) nil)
  (if (issibling(p1 p2)) (setf unrelated nil) nil)
  (if (iscousin p1 p2 tree) (setf unrelated nil) nil)
  (if (or (isancestor(p1 p2)) (isancestor(p2 p1))) (setf unrelated nil) nil)
  (unrelated))

(defun cousins(p tree)) ;TODO

(defun unrelated(p tree)) ;TODO

(defun family ()
  "This is the top-level function for the whole Lisp program."
  (let ((tree (make-hash-table :size 1000 :test #'equal)))))
