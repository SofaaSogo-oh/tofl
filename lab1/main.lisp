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
      (list :s (append 
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
       (format nil "~{~a~^~}" s-sym)
       (format nil "~{~a~^|~}"
               (mapcar 
                 (alexandria:curry #'format nil "~{~a~}") 
                 (car alts)))))
    rules))

(defun get-alternatives (non-terminal rules)
  "Возвращает список возможных замен для данного нетерминала."
  (first (gethash non-terminal rules)))

(defun replace-non-terminal (expression non-terminal alternative)
  "Заменяет первое вхождение non-terminal в expression на alternative.
   Возвращает новое выражение или NIL, если замена невозможна."
  (let ((pos (position non-terminal expression)))
    (if pos
        (append (subseq expression 0 pos)
                alternative
                (subseq expression (+ pos 1)))
        nil)))

(defun interactive-replace (expression rules)
  "Интерактивно заменяет нетерминалы в выражении, запрашивая у пользователя выбор правила."
  (let ((non-terminals (remove-if-not (lambda (x) (keywordp x)) expression)))
    (if (null non-terminals)
        (progn
          (format t "Выражение содержит только терминалы: ~a~%" expression)
          expression)
        (let ((non-terminal (first non-terminals)))
          (format t "Выражение: ~a~%" expression)
          (format t "Доступные правила для ~a:~%" non-terminal)
          (let ((alternatives (get-alternatives non-terminal rules)))
            (if (null alternatives)
                (progn
                  (format t "Нет правил для ~a!~%" non-terminal)
                  expression) ; Или что-то другое по вашему желанию
                (progn
                  (loop for i from 0 to (1- (length alternatives))
                        do (format t "~d: ~a~%" i (nth i alternatives)))
                  (format t "Выберите номер правила (или 'q' для выхода): ")
                  (let ((choice (read)))
                    (cond
                      ((eql choice 'q)
                       (format t "Прерывание.~%")
                       expression) ; Возвращаем исходное выражение
                      ((and (integerp choice) (>= choice 0) (< choice (length alternatives)))
                       (let ((new-expression (replace-non-terminal expression non-terminal (nth choice alternatives))))
                         (if new-expression
                             (interactive-replace new-expression rules) ; Рекурсивный вызов для продолжения замен
                             (progn
                               (format t "Ошибка при замене!~%")
                               expression)))) ; Возвращаем исходное выражение в случае ошибки
                      (t
                       (format t "Неверный выбор.~%")
                       (interactive-replace expression rules))))))))))) ; Повторный вызов с тем же выражением
;; Пример использования:
(defparameter *initial-expression* '(:S))

(defun main ()
  (let ((final-expression (interactive-replace *initial-expression* *rules2*)))
    (format t "Финальное выражение: ~a~%" final-expression)))
