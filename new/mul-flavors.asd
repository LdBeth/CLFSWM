(in-package #:asdf)

(asdf:defsystem #:mul-flavors
  :licence "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "object-definition")
	       (:file "helpers")
               (:file "mul-flavors")
               (:file "vanilla")))
