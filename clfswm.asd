;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <hocwp@free.fr>
;;;; ASDF System Definition
;;;
;;; #date#: Thu Mar  6 16:21:25 2008

(in-package #:asdf)

(defsystem clfswm
    :description "CLFSWM: Fullscreen Window Manager"
    :version "Please, see the package date (something between 0.5 and 1.5)"
    :author "Philippe Brochard  <hocwp@free.fr>"
    :licence "GNU Public License (GPL)"
    :components ((:file "tools")
		 (:file "my-html"
			:depends-on ("tools"))
		 (:file "package"
			:depends-on ("my-html" "tools"))
		 (:file "config"
			:depends-on ("package"))
		 (:file "keysyms"
			:depends-on ("package"))
		 (:file "xlib-util"
			:depends-on ("package" "keysyms" "config"))
		 (:file "netwm-util"
			:depends-on ("package" "xlib-util"))
		 (:file "clfswm-keys"
			:depends-on ("package" "config" "xlib-util" "keysyms"))
		 (:file "clfswm-internal"
			:depends-on ("xlib-util" "clfswm-keys" "netwm-util" "tools"))
		 (:file "clfswm"
			:depends-on ("xlib-util" "netwm-util" "clfswm-keys" "config"
						 "clfswm-internal" "tools"))
		 (:file "clfswm-second-mode"
			:depends-on ("package" "clfswm-internal"))
		 (:file "clfswm-info"
			:depends-on ("package" "xlib-util" "config" "clfswm-keys" "clfswm" "clfswm-internal"))
		 (:file "clfswm-util"
			:depends-on ("clfswm" "keysyms" "clfswm-info" "clfswm-second-mode" "clfswm-query"))
		 (:file "clfswm-query"
			:depends-on ("package" "config"))
		 (:file "clfswm-layout"
			:depends-on ("package" "clfswm-util" "clfswm-info"))
		 (:file "bindings"
			:depends-on ("clfswm" "clfswm-internal"))
		 (:file "bindings-second-mode"
			:depends-on ("clfswm" "clfswm-util" "clfswm-query" "bindings"))))





