(in-package #:tofl.common)

(defun map-to-found-count (alist)
  (let ((result nil)
        (cnts (make-hash-table :test #'equal)))
    (loop for element in alist
          do (let ((cnt (or (gethash (list element) cnts) 1)))
               (push cnt result)
               (setf (gethash (list element) cnts) (1+ cnt))))
    (reverse result)))

(defun replace-subsequence (expression alist-subsequence replacement k)
  "Заменяет k-й элемент из alist-subsequence на replacement в expression."
  (let* ((n (length expression))
         (pref-cnts (map-to-found-count alist-subsequence))
         (target-sequence (nth k alist-subsequence))
         (ns (length target-sequence))
         (target-pref-cnt (nth k pref-cnts))
         (result (copy-list expression)))
    (print 'test)
    (print expression)
    (print alist-subsequence)
    (print replacement)
    (print k)
    (print 'test)
    (loop for i from 0 to (- n ns)
          do (if (equal (subseq expression i (+ i ns)) target-sequence)
                 (progn 
                   (decf target-pref-cnt)
                   (when (= target-pref-cnt 0)
                     (setf result (append 
                                    (subseq result 0 i)
                                    replacement
                                    (subseq result (+ i ns))))
                     (return-from replace-subsequence result)))))
    result))

(defun left-choice (alist)
  (declare (ignore alist))
  0)

(defun interactive-replace-subsequences (expression rules &key (alist-choicer #'left-choice))
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
              (format t "~a~%" filtered-subsequences)
              (if (null filtered-subsequences)
                  (progn 
                   (format t "Thre are terminals only~%")
                   history)
                  (let* ((sub-k (funcall alist-choicer filtered-subsequences))
                         (subsequence (nth sub-k filtered-subsequences)))
                    (print subsequence)
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
                                                        expression filtered-subsequences 
                                                        (nth choice alternatives)
                                                        sub-k)))
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

