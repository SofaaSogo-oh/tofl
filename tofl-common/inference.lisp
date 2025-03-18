(in-package #:tofl.common)

(defun replace-subsequence (expression subsequence replacement)
  "Заменяет подпоследовательность в выражении на заданную альтернативу."
  (let ((result (copy-list expression))) ; Копируем, чтобы не модифицировать исходный список
    (loop for i from 0 to (- (length expression) (length subsequence))
          do (if (equal (subseq expression i (+ i (length subsequence))) subsequence)
                 (progn
                   (setf result (append (subseq result 0 i) replacement (subseq result (+ i (length subsequence)))))
                   (return-from replace-subsequence result)))) ; Замена только первого вхождения
    result))

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
                    (format t "~v@{~a~:*~}~%" 10 #\=)
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
                            (format t "Choose number of rule (or 'q' for exit).~%")
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

