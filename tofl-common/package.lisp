(defpackage #:tofl.common
  (:use #:cl)
  (:import-from #:alexandria-2 #:rcurry #:curry #:iota #:compose)
  (:export #:list-to-hash-table
           #:*digits*
           #:pprint-rules
           #:get-alternatives)
  (:export #:replace-subsequence
           #:interactive-replace-subsequences
           #:pprint-inference
           #:left-choice
           #:n-choice))
