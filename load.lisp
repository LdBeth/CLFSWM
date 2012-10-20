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
(pushnew :clfswm-compile *features*)
(pushnew :clfswm-run *features*)
(pushnew :clfswm-build-image *features*)

;;(pushnew :clfswm-install *features*)

;;;;;; Uncomment lines above to build the default documentation.
;;(pushnew :clfswm-build-doc *features*)

;;;;; Uncomment the line below if you want to see all ignored X errors
;;(pushnew :xlib-debug *features*)

;;;;; Uncomment the line below if you want to see all event debug messages
;;(pushnew :event-debug *features*)


(defparameter *binary-name* "clfswm")

(defparameter *install-prefix* "/tmp/usr/local")

(defun with-prefix (&rest place)
  (apply #'concatenate 'string *install-prefix* place))

(defparameter *install-bin*     (with-prefix "/bin/"))
(defparameter *install-contrib* (with-prefix "/lib/clfswm/"))
(defparameter *install-doc*     (with-prefix "/share/doc/clfswm/"))
(defparameter *install-man*     (with-prefix "/share/man/man1/"))



#+:CMU (setf ext:*gc-verbose* nil)

;;;------------------
;;; XLib part 1
;;;------------------
#+(or :CMU :ECL)
(require :clx)


;;;------------------
;;; ASDF part
;;;------------------
;;;; Loading ASDF
#+(or :SBCL :CMUCL :CCL :ECL)
(require :asdf)

#-:ASDF
(load "contrib/asdf.lisp")


;;;------------------
;;; XLib part 2
;;;------------------
;;; Loading clisp dynamic module. This part needs clisp >= 2.50
;;#+(AND CLISP (not CLX))
;;(when (fboundp 'require)
;;  (require "clx.lisp"))

;;;------------------
;;; CLFSWM loading
;;;------------------
#+:clfswm-compile
(asdf:oos 'asdf:load-op :clfswm)


;;;-------------------------
;;; Starting clfswm
;;;-------------------------
#+(or :clfswm-run :clfswm-build-doc :clfswm-build-image)
(in-package :clfswm)

#+:clfswm-run
(ignore-errors
  (main :read-conf-file-p t))


;;;-------------------------
;;; Building documentation
;;;-------------------------
#+:clfswm-build-doc
(produce-all-docs)

;;;-----------------------
;;; Building image part
;;;-----------------------
#+:clfswm-build-image
(build-lisp-image "clfswm")

;;;-----------------------
;;; Installation part
;;;-----------------------
#+:clfswm-install
(in-package :cl-user)

#+:SBCL
(require :sb-posix)

#+:clfswm-install
(load (compile-file "src/tools.lisp"))

#+:clfswm-install
(defun check-directory (dir)
  (format t "Checking ~A~%" dir)
  (ensure-directories-exist dir :verbose t))


#+:clfswm-install
(defun move-file (file where)
  (format t "cp -R ~A ~A~%" file where)
  (tools:fdo-shell "cp -R ~A ~A" file where))



#+:clfswm-install
(progn
  (check-directory *install-prefix*)
  (check-directory *install-bin*)
  (check-directory *install-contrib*)
  (check-directory *install-doc*)
  (check-directory *install-man*)
  (move-file *binary-name* *install-bin*)
  (move-file "contrib/*" *install-contrib*)
  (move-file "doc/*" *install-doc*)
  (move-file "clfswm.1" *install-man*)
  (format t "Please, adjust *contrib-dir* variable to ~A in your configuration file.~%" *install-contrib*)
  (format t "Something like: (setf *contrib-dir* ~S)~%" *install-contrib*)
  (sleep 0.5)
  (tools:fdo-shell "rm -f ~A/clfswm.1.gz && gzip ~A/clfswm.1" *install-man* *install-man*)
  (tools:uquit))

