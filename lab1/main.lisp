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
  (loop for i from 0 to 9 collect i))

(defparameter *rules* 
  (list-to-hash-table 
    (list 
      (list :S '((#\+ :U) (#\- :U) (:U)))
      (list :U '((:D) (:U :D)))
      (list :D (mapcar #'list *digits*)))))

(defparameter *rules2*
  (list-to-hash-table 
    (list 
      (list :S (append 
                 '((#\+ :T) (#\- :T)) 
                 (mapcar #'list *DIGITS*)
                 (mapcar (alexandria-2:curry #'list #\-) 
                         *digits*)))
      (list :T (mapcar (alexandria-2:rcurry #'list :F) 
                       (remove 0 *DIGITS*)))
      (list :F (append
                 (mapcar (alexandria:rcurry #'list :F)
                         *DIGITS*)
                 (mapcar #'list *digits*))))))

(defun print-rules (rules)
  (maphash 
    (alexandria:curry #'format t "|~a ↦ ~a~%")
    rules))

(defun pprint-rules (rules)
  (maphash
    (lambda (s-sym alts)
      (format t "{~a ↦ ~a~%" 
        s-sym
       (format nil "~{~a~^|~}"
               (mapcar 
                 (alexandria:curry #'format nil "~{~a~}") 
                 (car alts)))))
    rules))
