;;;; -*- Mode: Lisp -*-
;;;; ASDF System Definition
;;;

(in-package #:asdf)

(defsystem util-server
  :description ""
  :licence "GNU Lesser General Public License (LGPL)"
  :components ((:file "md5")
	       (:file "net")
	       (:file "crypt")
	       (:file "key"
		      :depends-on ("crypt"))))









