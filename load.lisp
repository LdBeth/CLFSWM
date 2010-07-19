;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: System loading functions
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2005 Philippe Brochard <hocwp@free.fr>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;
;;; --------------------------------------------------------------------------

(defparameter *base-dir* (directory-namestring *load-truename*))
(export '*base-dir*)

#+CMU
(setf ext:*gc-verbose* nil)

#+CMU
(require :clx)

#+SBCL
(require :asdf)

#+SBCL
(require :sb-posix)

#+(or SBCL ECL)
(require :clx)

#-ASDF
(load (make-pathname :host (pathname-host *base-dir*)
		     :device (pathname-device *base-dir*)
		     :directory (append (pathname-directory *base-dir*) (list "contrib"))
		     :name "asdf" :type "lisp"))

(push *base-dir* asdf:*central-registry*)


(asdf:oos 'asdf:load-op :clfswm)

(in-package :clfswm)

#-BUILD
(ignore-errors
  (main-unprotected :read-conf-file-p t))

;;(produce-all-docs)


;;; For debuging: start Xnest or Zephyr and
;;; add the lines above in a dot-clfswmrc-debug file
;;; mod-2 is the numlock key on some keyboards.
;;(setf *default-modifiers* '(:mod-2))
;;
;;(defun my-add-escape ()
;;  (define-main-key ("Escape" :mod-2) 'exit-clfswm))
;;
;;(add-hook *binding-hook* 'my-add-escape)
;;
;;(clfswm:main :display ":1" :alternate-conf #P"/where/is/dot-clfswmrc-debug")
