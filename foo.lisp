(defun print-set(hsh-set)
    (format 
      nil 
      "\rbr{~{S_{~A}~^, ~}}"
      (sort 
        (loop for key being the hash-key of hsh-set collect key) 
        #'<)))

(let ((table (make-hash-table :test 'eql)))
  (setf (gethash 11 table) t)
  (setf (gethash 8 table) t)
  (setf (gethash 5 table) t)
  (print-set table))

(cl-ppcre:do-matches-as-strings (m-var "\\d+" "foo42bar8")
  (format t "m-var: ~a~%" m-var))

(defparameter *determ-pattern*
  "(S_{\\d+}|S_\\d+)")

(defun extract-rule (str)
  (cl-ppcre:all-matches-as-strings 
    (format nil "~a\\s*\to\\s*(\\S+)" *determ-pattern*) str))

(defun extract-s (str)
 (cl-ppcre:all-matches-as-strings *DETERM-PATTERN* str))

(defun rule-to-edges (str)
  (let ((pre-res (extract-s str)))
    (loop for i in (cdr pre-res)
          collect (list (car pre-res) i))))

(defparameter *grammar1*
  (concatenate 'string
    "S_9 \to S_1|S_2         & S_{11} \to S_5|S_6            \\"
    "S_1 \to S_{15}a|a       & S_5 \to S_{16}b|S_{15}b       \\"
    "S_2 \to S_{15}b|b       & S_6 \to S_{16}c|S_{15}c       \\"
    "S_{10} \to S_3|S_4      & S_{12} \to S_7|S_8            \\"
    "S_3 \to S_9a            & S_7 \to S_{11}b               \\"
    "S_4 \to S_9b            & S_8 \to S_{11}c               \\"
    "S_{15} \to S_{10}       & S_{16} \to S_{12}|\varepsilon \\"))

(defparameter *grammar2*
 (concatenate 'string
    "S_9 \to S_1|S_2         & S_{11} \to S_5|S_6            \\"
    "S_1 \to aS_{10}         & S_5 \to bS_{12}               \\"
    "S_2 \to bS_{10}         & S_6 \to cS_{12}               \\"
    "S_{10} \to S_3|S_4      & S_{12} \to S_7|S_8            \\"
    "S_3 \to aS_{15}|aS_{16} & S_7 \to bS_{16}|b             \\"
    "S_4 \to bS_{15}|bS_{16} & S_8 \to cS_{16}|c             \\"
    "S_{15} \to S_9        & S_{16} \to S_{11}|\varepsilon \\"))


(defun extract-edges (grammar)
  (loop for i in 
        (extract-rule grammar) 
        append (rule-to-edges i)))

(defun extract-direct-graph (grammar)
 (let ((table (make-hash-table)))
   (loop for (from to) in (extract-edges grammar) do
     (setf (gethash from table) (push to (gethash from table))))
   table))

(defun extract-revers-graph (grammar)
 (let ((table (make-hash-table :test 'equal)))
   (loop for (from to) in (extract-edges grammar) do
     (setf (gethash to table) (push from (gethash to table))))
   table))
    
    
(defun print-graph (graph)
 (loop for key being the hash-keys of graph
       for value being the hash-values of graph
       do (format t "~a → ~a~%" key value)))

(defparameter *reverse-graph1* (extract-revers-graph *grammar1*))
(defparameter *reverse-graph2* (extract-revers-graph *grammar2*))

(print-graph *reverse-graph2*)

(defun get-line (graph num)
 (gethash 
  (if (< num 10)
    (format nil "S_~a" num)
    (format nil "S_{~a}" num))
  graph))

(defun list-to-cbr (alist)
 (format nil "~{~a~^, ~}" alist))

(defun normalize-cbr-alist (cbr-alist)
 (sort (remove-duplicates cbr-alist :test #'equal) #'string<))

(defun c-step-1 (graph prev)
  (normalize-cbr-alist (loop for i in (extract-s prev) 
                        append (gethash i graph))))

(defun c-step-2 (graph prev)
 (list-to-cbr 
   (normalize-cbr-alist 
     (append 
       (extract-s prev)
       (c-step-1 graph prev)))))

(get-line *reverse-graph1* (read))
(get-line *reverse-graph2* (read))
(merge-cbr (read-line) (read-line))

(defun gen-C (init graph)
  (labels ((iter (line inx)
            (let ((cur1 (c-step-1 graph line))
                  (cur2 (c-step-2 graph line))
                  (inx1 (1+ inx)))
              (format t 
                "C_{~a} & = \\cbr{~a} \\cup C_{~a} = \\cbr{~a}\\\\~%"
                inx1
                (list-to-cbr cur1) 
                inx
                cur2)
              (unless (equal line cur2)
                (iter cur2 inx1)))))
          (iter init 0)))

(gen-C (read-line) *reverse-graph2*)
                        
            
