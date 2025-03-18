(in-package #:tofl.common)

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

(defun pprint-rules (rules)
  (maphash
    (lambda (s-sym alts)
      (format t "{~a ↦ ~a~%" 
       (format nil "~{~a~^~}" s-sym)
       (format nil "~{~a~^|~}"
               (mapcar 
                 (alexandria:curry #'format nil "~{~a~}") 
                 (car alts)))))
    rules))

(defun get-alternatives (non-terminal rules)
  "Возвращает список возможных замен для данного нетерминала."
   (car (gethash non-terminal rules)))

(defparameter *digits*
  (loop for i from 0 to 9 collect i))

