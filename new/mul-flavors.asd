(in-package #:asdf)

(asdf:defsystem #:mul-flavors
  :licence "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "mul-flavors")
               (:file "vanilla")))
