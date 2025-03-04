(defmacro list-to-hash-table (alist &key (test #'eql))
  "Creates constant hash-table from the list
  Alist contains the set of pairs (key . value)
  Example of alist:
  '((A (B C))
    (D (E)))
  ⟺
  |A ↦ (B C)
  |D ↦ (E)
  "
  `(let ((hash-table (make-hash-table :test ,test :size (length ,alist))))
     (loop for (key . value) in ,alist
           do (setf (gethash key hash-table) value))
     hash-table))

(defparameter *digits*
  (loop for i from 0 to 9 collect (list i)))

(defparameter *rules* 
  (list-to-hash-table 
    '((:S ((#\+ :U) (#\- :U) (:U)))
      (:U ((:D) (:U :D)))
      (:D *digits*))))

(loop for i in *rules* (format t "~a" i))
