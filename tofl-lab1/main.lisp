(in-package #:tofl.lab1)

;(defparameter *rules1* 
;  (list-to-hash-table 
;    (list 
;      (list '(:S) '((#\+ :U) (#\- :U) (:U)))
;      (list '(:U) '((:D) (:U :D)))
;      (list '(:D) (mapcar #'list *digits*)))
;    :test #'equal))
;
;(defparameter *rules2*
;  (list-to-hash-table 
;    (list 
;      (list '(:S) '((:B :D)))
;      (list '(:D) '((:A :D) (#\b #\c)))
;      (list '(#\b :A) '((:A #\b)))
;      (list '(:B :A) '((#\a :B)))
;      (list '(#\a :B #\b) '((#\a #\a #\b)))
;      (list '(:B #\b #\c) '((#\a #\b #\c))))
;    :test #'equal))
;
;(defun print-rules (rules)
;  (maphash 
;    (alexandria:curry #'format t "|~a ↦ ~a~%")
;    rules))
;
;(pprint-rules *rules1*)
;(pprint-rules *rules2*)
;
;(pprint-inference (interactive-replace-subsequences '(:S) *rules1* :alist-choicer #'N-CHOICE))

;;;;TASK 1
;;;Построить грамматику, порождающую язык в которой отсутствуют нули перед числом
;Правила
(defparameter *task1-rules*
  (list-to-hash-table
    (list
      (list '(:S) (append '((#\+ :T)) '((#\- :T)) 
                    (loop for i from 0 to 9 collect (list i))
                    (loop for i from 1 to 9 collect (list #\- i))))
      (list '(:T) (loop for i from 1 to 9 collect (list i :F)))
      (list '(:F) (append 
                    (loop for i from 0 to 9 collect (list i :F))
                    (loop for i from 0 to 9 collect (list i)))))))
  
(pprint-rules *TASK1-RULES*)
(pprint-inference (interactive-replace-subsequences '(:S) *TASK1-RULES*))

;;;;TASK 2
;;; Построить КЗ грамматику, порождающую язык L = {a^n b^n c^n : ∀n > 0}
(defparameter *task2-rules1*
  (list-to-hash-table
    (list
      (list '(:S) '(("a" :S "b") ("ab"))))))

(pprint-rules *TASK2-RULES1*)
(pprint-inference (interactive-replace-subsequences '(:S) *TASK2-RULES1* :ALIST-CHOICER #'left-choice))

(defparameter *task2-rules*
  (list-to-hash-table
    (list
      ;initial
      (list '(:S) '(("a" :S :B :C) ("a" :B :C)))
      ;subs
      (list '("b" :B) '(("b" "b")))
      (list '("b" :C) '(("b" "c")))
      (list '("c" :C) '(("c" "c")))
      (list '("a" :B) '(("a" "b")))
      ;swap CB -> BC 
      (list '(:C :B) '((:C :X)))
      (list '(:C :X) '((:Y :X)))
      (list '(:Y :X) '((:B :X)))
      (list '(:B :X) '((:B :C))))))

(pprint-rules *TASK2-RULES*)
(pprint-inference (interactive-replace-subsequences '(:S) *TASK2-RULES*))

;;;Task3
;;Для α=a⁴b⁴c⁴ построить:
;;- ЛСВ
;;- ПСВ
;;- Произвольный вывод
(defparameter *task3-rules*
  (list-to-hash-table
    (list 
      (list '(:S) '((:B :D)))
      (list '(:B) '(("a" :B "b" :C) ("a" "b")))
      (list '(:C "b") '(("b" :C)))
      (list '(:C :D) '((:D "c")))
      (list '("b" :D "c") '(("b" "c" "c")))
      (list '("a" "b" :D) '(("a" "b" "c"))))))

(pprint-rules *TASK3-RULES*)
(pprint-inference (interactive-replace-subsequences '(:S) *TASK3-RULES* :alist-choicer #'left-choice))
;;- ЛСВ:
;; S ↝ BD ↝ aBbCD ↝ aaBbCbCD ↝ aaaBbCbCbCD ↝ aaaabbCbCbCD ↝ aaaabbbCCbCD ↝ aaaabbbCbCCD ↝ aaaabbbbCCCD ↝ aaaabbbbCCDc ↝ aaaabbbbCDcc ↝ aaaabbbbDccc ↝ aaaabbbbcccc
;;- ПСВ (облом)
;; S ↝ BD ↝ aBbCD ↝ aBbDc ↝ aBbcc ↝ aaBbCbcc ↝ aaBbbCcc ↝ aaaBbCbbCcc ↝ aaaBbbCbCcc ↝ aaaBbbbCCcc ↝ aaaabbbbCCcc
(defun right-choice (alist)
  (format t "~a~%" alist)
  (1- (length alist)))

(pprint-inference (interactive-replace-subsequences '(:S) *TASK3-RULES* :alist-choicer #'right-choice))

