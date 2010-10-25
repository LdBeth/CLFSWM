;;;; -*- Mode: Lisp -*-
;;;; ASDF System Definition
;;;

(in-package #:asdf)

(defsystem clfswm-client
  :description ""
  :licence "GNU Lesser General Public License (LGPL)"
  :components ((:file "md5")
	       (:file "net")
	       (:file "crypt")
	       (:file "key"
		      :depends-on ("crypt"))
	       (:file "clfswm-client"
		      :depends-on ("md5" "net" "crypt" "key"))))


