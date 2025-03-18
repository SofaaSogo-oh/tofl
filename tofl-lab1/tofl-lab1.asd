(asdf:defsystem #:tofl-lab1
  :DEPENDS-ON (#:alexandria
               #:tofl-common)
  :serial t
  :components ((:file "package")
               (:file "main")))

