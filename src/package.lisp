;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Package definition
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

(in-package :cl-user)

(defpackage clfswm
  (:use :common-lisp :my-html :tools)
  ;;(:shadow :defun)
  (:export :main))

(in-package :clfswm)


(defparameter *display* nil)
(defparameter *screen* nil)
(defparameter *root* nil)
(defparameter *no-focus-window* nil)
(defparameter *root-gc* nil)

(defparameter *contrib-dir* "")

(defparameter *default-font* nil)
;;(defparameter *default-font-string* "9x15")
(defparameter *default-font-string* "fixed")


(defparameter *child-selection* nil)

(defparameter *layout-list* nil)


;;(defstruct group (number (incf *current-group-number*)) name
;;	   (x 0) (y 0) (w 1) (h 1) rx ry rw rh
;;	   layout window gc child)

;;; CONFIG - Default group datas
(defparameter *default-group-data*
  (list '(:tile-size 0.8) '(:tile-space-size 0.1)))

(defclass group ()
  ((name :initarg :name :accessor group-name :initform nil)
   (number :initarg :number :accessor group-number :initform 0)
   ;;; Float size between 0 and 1 - Manipulate only this variable and not real size
   (x :initarg :x :accessor group-x :initform 0.1)
   (y :initarg :y :accessor group-y :initform 0.1)
   (w :initarg :w :accessor group-w :initform 0.8)
   (h :initarg :h :accessor group-h :initform 0.8)
   ;;; Real size (integer) in screen size - Don't set directly this variables
   ;;; they may be recalculated by the layout manager.
   (rx :initarg :rx :accessor group-rx :initform 0)
   (ry :initarg :ry :accessor group-ry :initform 0)
   (rw :initarg :rw :accessor group-rw :initform 800)
   (rh :initarg :rh :accessor group-rh :initform 600)
   (layout :initarg :layout :accessor group-layout :initform nil)
   (nw-hook :initarg :nw-hook :accessor group-nw-hook :initform nil
	      :documentation "Hook done by the group when a new window is mapped")
   (window :initarg :window :accessor group-window :initform nil)
   (gc :initarg :gc :accessor group-gc :initform nil)
   (child :initarg :child :accessor group-child :initform nil)
   (data :initarg :data :accessor group-data
	 :initform *default-group-data*
	 :documentation "An assoc list to store additional data")))



(defparameter *root-group* nil
  "Root of the root - ie the root group")
(defparameter *current-root* nil
  "The current fullscreen maximized child")
(defparameter *current-child* nil
  "The current child with the focus")

(defparameter *show-root-group-p* nil)


(defparameter *main-keys* (make-hash-table :test 'equal))
(defparameter *main-mouse* (make-hash-table :test 'equal))
(defparameter *second-keys* (make-hash-table :test 'equal))
(defparameter *second-mouse* (make-hash-table :test 'equal))
(defparameter *info-keys*  (make-hash-table :test 'equal))
(defparameter *info-mouse*  (make-hash-table :test 'equal))


(defparameter *open-next-window-in-new-workspace* nil
  "Set to t to open the next window in a new workspace
or to a number to open in a numbered workspace")

(defparameter *open-next-window-in-new-group* nil
  "Set to t to open the each next window in a new group
or set to :once open the next window in a new group and all
others in the same group")

(defparameter *arrow-action* nil
  "Arrow action in the second mode")



;;; Hook definitions
;;;
;;; A hook is a function, a symbol or a list of functions with a rest
;;; arguments.
;;;
;;; This hooks are set in clfswm.lisp, you can overwrite them or extend
;;; them with a hook list.
;;;
;;; See clfswm.lisp for hooks examples.

;;; Init hook. This hook is run just after the first root group is created
(defparameter *init-hook* nil)

;;; Main mode hooks (set in clfswm.lisp)
(defparameter *button-press-hook* nil)
(defparameter *button-release-hook* nil)
(defparameter *motion-notify-hook* nil)
(defparameter *key-press-hook* nil)
(defparameter *configure-request-hook* nil)
(defparameter *configure-notify-hook* nil)
(defparameter *create-notify-hook* nil)
(defparameter *destroy-notify-hook* nil)
(defparameter *enter-notify-hook* nil)
(defparameter *exposure-hook* nil)
(defparameter *map-request-hook* nil)
(defparameter *mapping-notify-hook* nil)
(defparameter *property-notify-hook* nil)
(defparameter *unmap-notify-hook* nil)


;;; Second mode hooks (set in clfswm-second-mode.lisp)
(defparameter *sm-button-press-hook* nil)
(defparameter *sm-button-release-hook* nil)
(defparameter *sm-motion-notify-hook* nil)
(defparameter *sm-key-press-hook* nil)
(defparameter *sm-configure-request-hook* nil)
(defparameter *sm-configure-notify-hook* nil)
(defparameter *sm-map-request-hook* nil)
(defparameter *sm-unmap-notify-hook* nil)
(defparameter *sm-destroy-notify-hook* nil)
(defparameter *sm-mapping-notify-hook* nil)
(defparameter *sm-property-notify-hook* nil)
(defparameter *sm-create-notify-hook* nil)
(defparameter *sm-enter-notify-hook* nil)
(defparameter *sm-exposure-hook* nil)



;;; Second mode global variables
(defparameter *motion-action* nil)
(defparameter *motion-object* nil)
(defparameter *motion-start-group* nil)
(defparameter *motion-dx* nil)
(defparameter *motion-dy* nil)


;; For debug - redefine defun
;;(shadow :defun)
;;
;;(defmacro defun (name args &body body)
;;  `(progn
;;    (format t "defun: ~A ~A~%" ',name ',args)
;;    (force-output)
;;    (cl:defun ,name ,args
;;      (handler-case
;;	  (progn
;;	    ,@body)
;;	(error (c)
;;	  (format t "New defun: Error in ~A : ~A~%" ',name c)
;;	  (format t "Root tree=~A~%All windows=~A~%"
;;		  (xlib:query-tree *root*) (get-all-windows))
;;	  (force-output))))))
