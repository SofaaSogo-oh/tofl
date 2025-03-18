(asdf:defsystem #:tofl-common
  :DEPENDS-ON (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "grammar")
               (:file "inference")))

