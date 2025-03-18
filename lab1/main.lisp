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

(pprint-rules *rules1*)
(pprint-rules *rules2*)
(get-alternatives (list :S) *rules2*)

(defun get-alternatives (non-terminal rules)
  "Возвращает список возможных замен для данного нетерминала."
   (car (gethash non-terminal rules)))

(defun replace-subsequence (expression subsequence replacement)
  "Заменяет подпоследовательность в выражении на заданную альтернативу."
  (let ((result (copy-list expression))) ; Копируем, чтобы не модифицировать исходный список
    (loop for i from 0 to (- (length expression) (length subsequence))
          do (if (equal (subseq expression i (+ i (length subsequence))) subsequence)
                 (progn
                   (setf result (append (subseq result 0 i) replacement (subseq result (+ i (length subsequence)))))
                   (return-from replace-subsequence result)))) ; Замена только первого вхождения
    result))

(replace-subsequence '(:y c :y) '(c :y) '(:h))

(defun interactive-replace-subsequences (expression rules)
  (labels ((iter (history)
            (let* ((expression (car history))
                   (subsequences 
                    (loop for i from 0 to (1- (length expression))
                          append (loop for j from (1+ i) to (length expression)
                                  collect (subseq expression i j))))
                   (sorted-subsequences (sort subsequences #'> :key #'length))
                   (filtered-subsequences
                    (remove-if-not 
                      (alexandria-2:rcurry #'get-alternatives rules)
                      sorted-subsequences)))
              (if (null filtered-subsequences)
                  (progn 
                   (format t "Thre are terminals only~%")
                   history)
                  (let ((subsequence (first filtered-subsequences)))
                    (format t "Expression: ~a~%" expression)
                    (format t "Available rules for ~a are:~%" subsequence)
                    (let* ((alternatives (get-alternatives subsequence rules))
                           (n-alts (length alternatives)))
                      (if (null alternatives)
                          (progn
                            (format t "No rules for ~a!~%" subsequences)
                            expression)
                          (progn 
                            (format t "~{~a~%~}"
                                    (mapcar (alexandria-2:curry 
                                              #'format nil
                                              "~a. ~a")
                                      (alexandria-2:iota n-alts)
                                      alternatives))
                            (format t "Choose number of rule (or 'q' for exit): ")
                            (let ((choice (read)))
                              (cond 
                                ((eql choice 'q) (format t "Exit.~%") history)
                                ((and (integerp choice) (<= 0 choice) (< choice n-alts)
                                      (let ((new-expr (replace-subsequence 
                                                        expression subsequence 
                                                        (nth choice alternatives))))
                                        (if (equal new-expr expression)
                                            (progn
                                              (format t "Substitution error!~%")
                                              history)
                                            (iter (cons new-expr history))))))
                                (t
                                 (format t "Wrong input.~%")
                                 (iter history))))))))))))
    (reverse (iter (list expression)))))

(defun pprint-inference (alist)
  (format t "~{~a~^ ↝ ~}~%"
          (mapcar (alexandria-2:curry #'format nil "~{~a~}")
                  alist)))

(pprint-inference (interactive-replace-subsequences '(:S) *rules2*))

(format t "~{~a~%~}" (mapcar (alexandria-2:curry #'format nil "~a ~a") '(1 3 2) '(a b c)))
