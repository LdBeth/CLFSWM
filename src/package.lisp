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
  (:use :common-lisp :my-html :tools :version)
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
(defparameter *nw-hook-list* nil)


;;(defstruct frame (number (incf *current-frame-number*)) name
;;	   (x 0) (y 0) (w 1) (h 1) rx ry rw rh
;;	   layout window gc child)

;;; CONFIG - Default frame datas
(defparameter *default-frame-data*
  (list '(:tile-size 0.8) '(:tile-space-size 0.1)))


;;; CONFIG - Default managed window type for a frame
;;; type can be  :all, :normal, :transient, :maxsize, :desktop, :dock, :toolbar, :menu, :utility, :splash, :dialog
(defparameter *default-managed-type* '(:normal))
;;(defparameter *default-managed-type* '(:normal :maxsize :transient))
;;(defparameter *default-managed-type* '(:normal :transient :maxsize :desktop :dock :toolbar :menu :utility :splash :dialog))
;;(defparameter *default-managed-type* '())
;;(defparameter *default-managed-type* '(:all))

(defclass frame ()
  ((name :initarg :name :accessor frame-name :initform nil)
   (number :initarg :number :accessor frame-number :initform 0)
   ;;; Float size between 0 and 1 - Manipulate only this variable and not real size
   (x :initarg :x :accessor frame-x :initform 0.1)
   (y :initarg :y :accessor frame-y :initform 0.1)
   (w :initarg :w :accessor frame-w :initform 0.8)
   (h :initarg :h :accessor frame-h :initform 0.8)
   ;;; Real size (integer) in screen size - Don't set directly this variables
   ;;; they may be recalculated by the layout manager.
   (rx :initarg :rx :accessor frame-rx :initform 0)
   (ry :initarg :ry :accessor frame-ry :initform 0)
   (rw :initarg :rw :accessor frame-rw :initform 800)
   (rh :initarg :rh :accessor frame-rh :initform 600)
   (layout :initarg :layout :accessor frame-layout :initform nil
	   :documentation "Layout to display windows on a frame")
   (nw-hook :initarg :nw-hook :accessor frame-nw-hook :initform nil
	    :documentation "Hook done by the frame when a new window is mapped")
   (managed-type :initarg :managed-type :accessor frame-managed-type
		 :initform *default-managed-type*
		 :documentation "Managed window type")
   (forced-managed-window :initarg :forced-managed-window
			  :accessor frame-forced-managed-window
			  :initform nil
			  :documentation "A list of forced managed windows (wm-name or window)")
   (forced-unmanaged-window :initarg :forced-unmanaged-window
			  :accessor frame-forced-unmanaged-window
			  :initform nil
			  :documentation "A list of forced unmanaged windows (wm-name or window)")
   (window :initarg :window :accessor frame-window :initform nil)
   (gc :initarg :gc :accessor frame-gc :initform nil)
   (child :initarg :child :accessor frame-child :initform nil)
   (data :initarg :data :accessor frame-data
	 :initform *default-frame-data*
	 :documentation "An assoc list to store additional data")))



(defparameter *root-frame* nil
  "Root of the root - ie the root frame")
(defparameter *current-root* nil
  "The current fullscreen maximized child")
(defparameter *current-child* nil
  "The current child with the focus")

(defparameter *show-root-frame-p* nil)


(defparameter *main-keys* (make-hash-table :test 'equal))
(defparameter *main-mouse* (make-hash-table :test 'equal))
(defparameter *second-keys* (make-hash-table :test 'equal))
(defparameter *second-mouse* (make-hash-table :test 'equal))
(defparameter *info-keys*  (make-hash-table :test 'equal))
(defparameter *info-mouse*  (make-hash-table :test 'equal))



(defstruct menu name item doc)
(defstruct menu-item key value)


(defvar *menu* (make-menu :name 'main :doc "Main menu"))


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
