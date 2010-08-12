;;;; -*- Mode: Lisp -*-
;;;; ASDF System Definition
;;;

(in-package #:asdf)

(defsystem clfswm-client
  :description ""
  :licence "GNU Lesser General Public License (LGPL)"
  :components ((:file "clfswm-client"))
  :depends-on (util-server))









