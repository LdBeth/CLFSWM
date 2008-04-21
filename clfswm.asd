;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <hocwp@free.fr>
;;;; ASDF System Definition
;;;

(in-package #:asdf)

(defsystem clfswm
  :description "CLFSWM: Fullscreen Window Manager"
  :version "Please, see the package date (something between 0.5 and 1.5)"
  :author "Philippe Brochard  <hocwp@free.fr>"
  :licence "GNU Public License (GPL)"
  :components ((:module src
			:components
			((:file "tools")
			 (:file "my-html"
			  :depends-on ("tools"))
			 (:file "package"
			  :depends-on ("my-html" "tools" "version"))
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
			 (:file "version"
			  :depends-on ("tools"))
			 (:file "clfswm-second-mode"
			  :depends-on ("package" "clfswm-internal"))
			 (:file "clfswm-info"
			  :depends-on ("package" "version" "xlib-util" "config" "clfswm-keys" "clfswm" "clfswm-internal"))
			 (:file "clfswm-util"
			  :depends-on ("clfswm" "keysyms" "clfswm-info" "clfswm-second-mode" "clfswm-query"))
			 (:file "clfswm-query"
			  :depends-on ("package" "config"))
			 (:file "clfswm-layout"
			  :depends-on ("package" "clfswm-internal" "clfswm-util" "clfswm-info"))
			 (:file "clfswm-pack"
			  :depends-on ("clfswm" "clfswm-util" "clfswm-second-mode"))
			 (:file "clfswm-nw-hooks"
			  :depends-on ("package" "clfswm-util" "clfswm-info"))
			 (:file "clfswm-menu"
			  :depends-on ("package" "clfswm-info"))
			 (:file "bindings"
			  :depends-on ("clfswm" "clfswm-internal" "clfswm-util"))
			 (:file "bindings-second-mode"
			  :depends-on ("clfswm" "clfswm-util" "clfswm-query" "bindings" "clfswm-pack" "clfswm-menu"))))))





