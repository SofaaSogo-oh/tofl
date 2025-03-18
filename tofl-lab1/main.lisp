(in-package #:tofl.lab1)

(defparameter *rules1* 
  (list-to-hash-table 
    (list 
      (list '(:S) '((#\+ :U) (#\- :U) (:U)))
      (list '(:U) '((:D) (:U :D)))
      (list '(:D) (mapcar #'list *digits*)))
    :test #'equal))


(defparameter *rules2*
  (list-to-hash-table 
    (list 
      (list '(:S) '((:B :D)))
      (list '(:D) '((:A :D) (#\b #\c)))
      (list '(#\b :A) '((:A #\b)))
      (list '(:B :A) '((#\a :B)))
      (list '(#\a :B #\b) '((#\a #\a #\b)))
      (list '(:B #\b #\c) '((#\a #\b #\c))))
    :test #'equal))

(defun print-rules (rules)
  (maphash 
    (alexandria:curry #'format t "|~a ↦ ~a~%")
    rules))

(pprint-rules *rules1*)
(pprint-rules *rules2*)
(get-alternatives (list :S) *rules2*)

(replace-subsequence '(:y c :y) '(c :y) '(:h))

(pprint-inference (interactive-replace-subsequences '(:S) *rules2*))

(format t "~{~a~%~}" (mapcar (alexandria-2:curry #'format nil "~a ~a") '(1 3 2) '(a b c)))
