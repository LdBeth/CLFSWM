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

;;;------------------
;;; Customization part
;;;------------------
(pushnew :clfswm-build *features*)
(pushnew :clfswm-dump *features*)
(pushnew :clfswm-start *features*)
(pushnew :clfswm-install *features*)

;;;;;; Uncomment lines above to build the default documentation.
;;(pushnew :clfswm-build-doc *features*)

;;;;; Uncomment the line below if you want to see all ignored X errors
;;(pushnew :xlib-debug *features*)

;;;;; Uncomment the line below if you want to see all event debug messages
;;(pushnew :event-debug *features*)


(defparameter *base-dir* (directory-namestring *load-truename*))
(export '*base-dir*)


#+:CMU
(setf ext:*gc-verbose* nil)

;;;------------------
;;; ASDF part
;;;------------------
;;;; Loading ASDF
#+(or :SBCL :ECL)
(require :asdf)


#-:ASDF
(load (make-pathname :host (pathname-host *base-dir*)
		     :device (pathname-device *base-dir*)
		     :directory (append (pathname-directory *base-dir*) (list "contrib"))
		     :name "asdf" :type "lisp"))

(push *base-dir* asdf:*central-registry*)

;;(setf asdf:*verbose-out* t)


;;;------------------
;;; XLib part
;;;------------------
#+(or :CMU :ECL)
(require :clx)


;;; This part needs clisp >= 2.50
;;#+(AND CLISP (not CLX))
;;(when (fboundp 'require)
;;  (require "clx.lisp"))

;;;------------------
;;; CLFSWM loading
;;;------------------
#+:clfswm-build
(asdf:oos 'asdf:load-op :clfswm)


;;;-------------------------
;;; Starting clfswm
;;;-------------------------
(in-package :clfswm)

#+:clfswm-start
(ignore-errors
  (main :read-conf-file-p #-:clfswm-build-doc t #+:clfswm-build-doc nil))



;;;-------------------------
;;; Building documentation
;;;-------------------------
#+:clfswm-build-doc
(produce-all-docs)

;;;-----------------------
;;; Building image part
;;;-----------------------
#+:clfswm-build
(build-lisp-image "clfswm")

