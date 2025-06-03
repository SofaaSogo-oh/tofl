(ql:quickload :cl-ppcre)

(defun print-set(hsh-set)
    (format 
      nil 
      "\rbr{~{S_{~A}~^, ~}}"
      (sort 
        (loop for key being the hash-key of hsh-set collect key) 
        #'<)))

(defparameter *determ-pattern*
  "(S_{\\d+}|S_\\d+)")

(defun extract-rule (str)
  (cl-ppcre:all-matches-as-strings 
    (format nil "~a\\s*\to\\s*(\\S+)" *determ-pattern*) str))

(defun extract-s (str)
 (cl-ppcre:all-matches-as-strings *DETERM-PATTERN* str))

(defun extract-t (str)
 (cl-ppcre:all-matches-as-strings "a|b|c" str))

(defun rule-to-edges (str)
  (let ((pre-res (extract-s str)))
    (loop for i in (cdr pre-res)
          collect (list (car pre-res) i))))

(defun rule-to-tedges (str)
 (let ((pre-res (extract-t str))
       (from (car (extract-s str))))
  (loop for i in (cdr pre-res)
   collect (list from i))))

(defun extract-edges (grammar)
  (loop for i in 
        (extract-rule grammar) 
        append (rule-to-edges i)))

(defun extract-tedges (grammar)
 (loop for i in 
  (extract-rule grammar)
  append (rule-to-tedges i)))

(defun extract-direct-graph (grammar)
 (let ((table (make-hash-table :test 'equal)))
   (loop for (from to) in (append (extract-edges grammar)
                                  (extract-tedges grammar)) do
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

(defun get-s-by-num (num) 
  (if (< num 10)
    (format nil "S_~a" num)
    (format nil "S_{~a}" num)))

(defun get-line (graph num)
 (gethash 
  (get-s-by-num num)
  graph))

(defun list-to-cbr (alist)
 (format nil "~{~a~^, ~}" alist))

(defun normalize-cbr-alist (cbr-alist)
 (sort (remove-duplicates cbr-alist :test #'equal) #'string<))

(defun list-to-norm-cbr (alist)
  (list-to-cbr (normalize-cbr-alist alist)))

(defun c-step-1 (graph prev)
  (normalize-cbr-alist 
    (loop for i in (extract-s prev) 
     append (gethash i graph))))

(defun c-step-2 (graph prev)
  (normalize-cbr-alist 
       (append 
         (extract-s prev)
         (c-step-1 graph prev)))) 
   

(defun gen-C (init graph &key (init-inx 0) (iter-name "C") (stream T))
  (labels ((iter (line inx)
            (let* ((cur1 (append (c-step-1 graph line)))
                   (cur2 (c-step-2 graph line))
                   (lcur1 (list-to-norm-cbr cur1))
                   (lcur2 (list-to-norm-cbr cur2))
                   (inx1 (1+ inx)))
              (format stream 
                "~a_{~a} & = \\cbr{~a} \\cup ~a_{~a} = \\cbr{~a}\\\\~%"
                iter-name inx1
                lcur1 
                iter-name inx
                lcur2)
              (unless (equal line lcur2)
                (iter lcur2 inx1))
              lcur2)))
          (iter init init-inx)))

