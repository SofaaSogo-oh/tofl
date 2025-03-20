(in-package #:tofl.lab1)

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
;;А так вроде норм:
;;S ↝ BD ↝ aBbCD ↝ aBbDc ↝ aaBbCbDc ↝ aaBbbCDc ↝ aaBbbDcc ↝ aaaBbCbbDcc ↝ aaaBbbCbDcc ↝ aaaBbbbCDcc ↝ aaaBbbbDccc ↝ aaaBbbbcccc ↝ aaaabbbbcccc

(defun right-choice (alist)
  (format t "~a~%" alist)
  (1- (length alist)))

(pprint-inference (interactive-replace-subsequences '(:S) *TASK3-RULES* :alist-choicer #'right-choice))
(pprint-inference (interactive-replace-subsequences '(:S) *TASK3-RULES*))

(defparameter *task4-rules*
  (list-to-hash-table
    (list
      (list '(:S) '((:B :D)))
      (list '(:D) '((:A "b" :D "c") ("b" "c")))
      (list '("b" :A) '((:A "b")))
      (list '(:B :A) '(("a" :B)))
      (list '("a" :B "b") '(("a" "a" "b")))
      (list '(:B "b" "c") '(("a" "b" "c"))))))

(pprint-inference (interactive-replace-subsequences '(:S) *task4-rules*))
;; ЛСВ
;; S ↝ BD ↝ BAbDc ↝ aBbDc ↝ aBbAbDcc ↝ aBAbbDcc ↝ aBAbbAbDccc ↝ aBAbAbbDccc ↝ aBAAbbbDccc ↝ aaBAbbbDccc ↝ aaaBbbbDccc ↝ aaaabbbDccc ↝ aaaabbbbcccc
;; ПСВ
;; S ↝ BD ↝ BAbDc ↝ BAbAbDcc ↝ BAbAbAbDccc ↝ BAbAbAbbcccc ↝ BAbAAbbbcccc ↝ BAAbAbbbcccc ↝ BAAAbbbbcccc ↝ aBAAbbbbcccc ↝ aaBAbbbbcccc ↝ aaaBbbbbcccc ↝ aaaabbbbcccc

;;;TASK5: построить грамматики 4 типов для G={a,b}⁺

; (a|b)+ -> (a|b)(a|b)* 
(defparameter *task5-rules-type3*
  (list-to-hash-table
    (list
      (list '(:S) '(("a") ("b") ("a" :S) ("b" :S))))))

(pprint-inference (interactive-replace-subsequences '(:S) *TASK5-RULES-TYPE3*))

(defparameter *task5-rules-type2*
  (list-to-hash-table
    (list
      (list))))

;;TASK6: для языка крамматики построить грамматику, в которой учитываются приоритет операций
(defparameter *task6-rules*
  (list-to-hash-table
    (list
      (list '(:S) '((:S "+" :T) (:s "-" :t) (:t)))
      (list '(:t) '((:t "*" :f) (:t "/" :f) (:f)))
      (list '(:f) '(("(" :s ")") ("a") ("b"))))))

(pprint-rules *TASK6-RULES*)

(pprint-inference (interactive-replace-subsequences '(:S) *TASK6-rules*))
;;;TASK7
(defparameter *task7-rules*
  (list-to-hash-table
    (list
      (list '(:S) '(
                    (:S "+" :S) (:S "-" :S) 
                    (:s "*" :s) (:s "/" :s)
                    ("(" :s ")")
                    ("a") ("b"))))))

(pprint-rules *TASK7-RULES*)
(pprint-inference (interactive-replace-subsequences '(:S) *TASK7-rules*))
;;ПСВ1:
; S ↝ S*S ↝ S*S+S ↝ S*S+S+S ↝ S*S+S+b ↝ S*S+a+b ↝ S*b+a+b ↝ (S)*b+a+b ↝ (S+S)*b+a+b ↝ (S+b)*b+a+b ↝ (a+b)*b+a+b
;;ПСВ2:
; S ↝ S+S ↝ S+b ↝ S+S+b ↝ S+a+b ↝ S*S+a+b ↝ S*b+a+b ↝ (S)*b+a+b ↝ (S+S)*b+a+b ↝ (S+b)*b+a+b ↝ (a+b)*b+a+b
;;ЛСВ1:
; S ↝ S*S ↝ (S)*S ↝ (S+S)*S ↝ (a+S)*S ↝ (a+b)*S ↝ (a+b)*S+S ↝ (a+b)*b+S ↝ (a+b)*b+S+S ↝ (a+b)*b+a+S ↝ (a+b)*b+a+b
;;ЛСВ2:
; S ↝ S+S ↝ S+S+S ↝ S*S+S+S ↝ (S)*S+S+S ↝ (S+S)*S+S+S ↝ (a+S)*S+S+S ↝ (a+b)*S+S+S ↝ (a+b)*b+S+S ↝ (a+b)*b+a+S ↝ (a+b)*b+a+b
; ДР в lab1.xopp
;;;TASK8
;;ПСВ1:
; S ↝ S+T ↝ S+F ↝ S+b ↝ S+T+b ↝ S+F+b ↝ S+a+b ↝ T+a+b ↝ T*F+a+b ↝ T*b+a+b ↝ F*b+a+b ↝ (S)*b+a+b ↝ (S+T)*b+a+b ↝ (S+F)*b+a+b ↝ (S+b)*b+a+b ↝ (T+b)*b+a+b ↝ (F+b)*b+a+b ↝ (a+b)*b+a+b
;;ПСВ2:
;
