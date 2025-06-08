(load "cwd-hh")

(progn "def grammatics"
  (defparameter *lg-17*
    (concatenate 'string
      "S_9 \to S_1|S_2               & S_{11} \to S_5|S_6      \\"
      "S_1 \to S_{15}a|a             & S_5 \to S_{16}b|S_{15}b \\"
      "S_2 \to S_{15}b|b             & S_6 \to S_{16}c|S_{15}c \\"
      "S_{10} \to S_3|S_4            & S_{12} \to S_7|S_8      \\"
      "S_3 \to S_9a                  & S_7 \to S_{11}b         \\"
      "S_4 \to S_9b                  & S_8 \to S_{11}c         \\"
      "S_{15} \to S_{10}|\varepsilon & S_{16} \to S_{12}       \\"))
  (defparameter *rg-17*
   (concatenate 'string
      "S_9 \to S_1|S_2               & S_{11} \to S_5|S_6      \\"
      "S_1 \to aS_{10}               & S_5 \to bS_{12}         \\"
      "S_2 \to bS_{10}               & S_6 \to cS_{12}         \\"
      "S_{10} \to S_3|S_4            & S_{12} \to S_7|S_8      \\"
      "S_3 \to aS_{15}|aS_{16}       & S_7 \to bS_{16}|b       \\"
      "S_4 \to bS_{15}|bS_{16}       & S_8 \to cS_{16}|c       \\"
      "S_{15} \to S_{9}|S_{16}       & S_{16} \to S_{11}       \\"))
  (defparameter *rg-18*
   (concatenate 'string
      "S_9 \to S_1|S_2           & S_{11} \to S_5|S_6            \\"
      "S_1 \to aS_{10}           & S_5 \to bS_{12}               \\"
      "S_2 \to bS_{10}           & S_6 \to cS_{12}               \\"
      "S_{10} \to S_3|S_4        & S_{12} \to S_7|S_8            \\"
      "S_3 \to aS_{15}|aS_{16}|a & S_7 \to bS_{16}|b             \\"
      "S_4 \to bS_{15}|bS_{16}|b & S_8 \to cS_{16}|c             \\"
      "S_{15} \to S_9            & S_{16} \to S_{11}             \\"))
  (defparameter *lg-18*
    (concatenate 'string
          "S_1 \to S_{15}a|a                          & S_5 \to S_{16}b|S_{15}b              \\"
          "S_2 \to S_{15}b|b                          & S_6 \to S_{16}c|S_{15}c              \\"
          "S_3 \to S_9a                               & S_7 \to S_{11}b                      \\"
          "S_4 \to S_9b                               & S_8 \to S_{11}c                      \\"
          "S_9 \to S_{15}a|a|S_{15}b|b                & S_{10} \to S_9a|S_9b                 \\"
          "S_{11} \to S_{16}b|S_{15}b|S_{16}c|S_{15}c & S_{12} \to S_{11}b|S_{11}c           \\"
          "S_{15} \to S_9a|S_9b                       & S_{16} \to S_{11}b|S_{11}c|S_9a|S_9b \\"))
  (defparameter *rg-19*
    (concatenate 'string
          "S_1 \to aS_{10}            & S_5 \to bS_{12}                                \\"
          "S_2 \to bS_{10}            & S_6 \to cS_{12}                                \\"
          "S_3 \to aS_{15}|aS_{16}|a  & S_7 \to bS_{16}|b                              \\"
          "S_4 \to bS_{15}|bS_{16}|b  & S_8 \to cS_{16}|c                              \\"
          "S_9 \to aS_{10}|bS_{10}    & S_{10} \to aS_{15}|aS_{16}|a|bS_{15}|bS_{16}|b \\"
          "S_{11} \to bS_{12}|cS_{12} & S_{12} \to bS_{16}|b|cS_{16}|c                 \\"
          "S_{15} \to aS_{10}|bS_{10} & S_{16} \to bS_{12}|cS_{12}                  \\"))
  (defparameter *clg-17*
    (concatenate 'string
      "S_9 \to S_1|S_2         & S_{11} \to S_5|S_6            \\"
      "S_{10} \to S_3|S_4      & S_{12} \to S_7|S_8            \\"
      "S_{15} \to S_{10}       & S_{16} \to S_{12}|S_{15}      \\"))
  (defparameter *crg-18*
    (concatenate 'string
      "S_9 \to S_1|S_2           & S_{11} \to S_5|S_6            \\"
      "S_{10} \to S_3|S_4        & S_{12} \to S_7|S_8            \\"
      "S_{15} \to S_9            & S_{16} \to S_{11}             \\")))

(defparameter *grammar1* *lg-17*)
(defparameter *grammar2* *rg-18*)

(defun def-gramm-graphs (gramm)
  "def direct/reverse grammar graph"
  (defparameter *reverse-graph* (extract-revers-graph gramm))
  (defparameter *direct-graph* (extract-direct-graph gramm)))

(progn "def direct/reverse left/right grammar graph"
  (defparameter *reverse-graph1* (extract-revers-graph *grammar1*))
  (defparameter *reverse-graph2* (extract-revers-graph *grammar2*))
  (defparameter *direct-graph1* (extract-direct-graph *grammar1*))
  (defparameter *direct-graph2* (extract-direct-graph *grammar2*)))


(print-graph *reverse-graph2*)

(get-line *reverse-graph1* (read))
(get-line *reverse-graph2* (read))
(merge-cbr (read-line) (read-line))

;;Обход по обратным дугам;;;;;;;;;;;;;;;;;;
(gen-C (read-line) *reverse-graph1* :init-inx 1)
(gen-C (read-line) *reverse-graph2* :init-inx 1)
(gen-C (read-line) *reverse-graph* :init-inx 1)

;;Прямой обход ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(gen-C (read-line) *direct-graph1*)
(print-graph *direct-graph1*)
(print-graph *direct-graph2*)
(gen-C (read-line) *direct-graph2*)
(gen-c (read-line) *direct-graph*)

;Нахждение C для удаления пустых правил;;;;
(gen-C (read-line) *reverse-graph1*)
(gen-C (read-line) *reverse-graph2*)

;;Удаление цепных правил;;;;;;;;;;;;;;;;;;;
;;;Построение множеств \aleph;;;;;;;;;;;;;;
(loop for i from 0 to 16 do
      (let* ((init (get-s-by-num i))
             (stream (make-string-output-stream))
             (iter-name (format nil "\\aleph^{~a}" init))
             (res (gen-C init *direct-graph* :ITER-NAME iter-name :stream stream)))
        (format t "\\setconc{ 
~a ~a }{~a}\\\\~%"
          (format nil "~a_{0} & = \\cbr{~a}\\\\~%" iter-name init)
          (get-output-stream-string stream)
          (format nil "~a & = \\cbr{~a}"
                  iter-name res))))
