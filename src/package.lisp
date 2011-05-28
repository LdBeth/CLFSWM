;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Package definition
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2011 Philippe Brochard <hocwp@free.fr>
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
;;  (:shadow :defun)
  (:export :main
	   :reload-clfswm
	   :reset-clfswm
	   :exit-clfswm))



(in-package :clfswm)

;;; CONFIG - Compress motion notify ?
;; This variable may be useful to speed up some slow version of CLX.
;; It is particulary useful with CLISP/MIT-CLX (and others).
(defconfig *have-to-compress-notify* t nil
           "Compress event notify?
This variable may be useful to speed up some slow version of CLX.
It is particulary useful with CLISP/MIT-CLX.")


(defconfig *show-root-frame-p* nil nil
           "Show the root frame information or not")


(defconfig *border-size* 1 nil
           "Windows and frames border size")



(defparameter *modifier-alias* '((:alt :mod-1)     (:alt-l :mod-1)
				 (:numlock :mod-2)
				 (:super_l :mod-4)
				 (:alt-r :mod-5)   (:alt-gr :mod-5)
				 (:capslock :lock))
  "Syntax: (modifier-alias effective-modifier)")


(defparameter *display* nil)
(defparameter *screen* nil)
(defparameter *root* nil)
(defparameter *no-focus-window* nil)

(defconfig *loop-timeout* 0.1 nil
           "Maximum time (in seconds) to wait before calling *loop-hook*")

(defparameter *pixmap-buffer* nil)

(defparameter *contrib-dir* "")

(defparameter *default-font* nil)
;;(defparameter *default-font-string* "9x15")
(defconfig *default-font-string* "fixed" nil
           "The default font used in clfswm")

(defconfig *color-move-window* "DeepPink" 'Main-mode
           "Color when moving or resizing a windows")

(defparameter *child-selection* nil)

;;; CONFIG - Default frame datas
(defconfig *default-frame-data*
  (list '(:tile-size 0.8) '(:tile-space-size 0.1)
	'(:fast-layout (tile-left-layout tile-layout))
	'(:main-layout-windows nil))
  nil
  "Default slots set in frame date")


;;; CONFIG - Default managed window type for a frame
;;; type can be  :all, :normal, :transient, :maxsize, :desktop, :dock, :toolbar, :menu, :utility, :splash, :dialog
(defconfig *default-managed-type* '(:normal) nil
  "Default managed window types")
;;(defparameter *default-managed-type* '(:normal :maxsize :transient))
;;(defparameter *default-managed-type* '(:normal :transient :maxsize :desktop :dock :toolbar :menu :utility :splash :dialog))
;;(defparameter *default-managed-type* '())
;;(defparameter *default-managed-type* '(:all))


;;; CONFIG - Default focus policy
(defconfig *default-focus-policy* :click nil
           "Default mouse focus policy. One of :click, :sloppy, :sloppy-strict or :sloppy-select.")

(defstruct child-rect child parent selected-p x y w h)


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
   (show-window-p :initarg :show-window-p :accessor frame-show-window-p :initform t)
   (hidden-children :initarg :hidden-children :accessor frame-hidden-children :initform nil
		    :documentation "A list of hidden children")
   (selected-pos :initarg :selected-pos :accessor frame-selected-pos :initform 0
		 :documentation "The position in the child list of the selected child")
   (focus-policy :initarg :focus-ploicy :accessor frame-focus-policy
		 :initform *default-focus-policy*)
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


(defparameter *main-keys* nil)
(defparameter *main-mouse* nil)
(defparameter *second-keys* nil)
(defparameter *second-mouse* nil)
(defparameter *info-keys* nil)
(defparameter *info-mouse* nil)
(defparameter *query-keys* nil)
(defparameter *circulate-keys* nil)
(defparameter *circulate-keys-release* nil)
(defparameter *expose-keys* nil)
(defparameter *expose-mouse* nil)


(defparameter *other-window-manager* nil)


(defstruct menu name item doc)
(defstruct menu-item key value)


(defparameter *menu* (make-menu :name 'main :doc "Main menu"))




(defconfig *binding-hook* nil 'Hook
           "Hook executed when keys/buttons are bounds")

(defconfig *loop-hook* nil 'Hook
           "Hook executed on each event loop")

(defconfig *main-entrance-hook* nil 'Hook
           "Hook executed on the main function entrance after
loading configuration file and before opening the display.")


(defparameter *in-second-mode* nil)


;;; Placement variables. A list of two absolute coordinates
;;; or a function: 'Y-X-placement' for absolute placement or
;;; 'Y-X-child-placement' for child relative placement.
;;; Where Y-X are one of:
;;;
;;; top-left     top-middle     top-right
;;; middle-left  middle-middle  middle-right
;;; bottom-left  bottom-middle  bottom-right
;;;
(defconfig *banish-pointer-placement* 'bottom-right-placement
  'Placement "Pointer banishment placement")
(defconfig *second-mode-placement* 'top-middle-placement
  'Placement "Second mode window placement")
(defconfig *info-mode-placement* 'top-left-placement
  'Placement "Info mode window placement")
(defconfig *query-mode-placement* 'top-left-placement
  'Placement "Query mode window placement")
(defconfig *circulate-mode-placement* 'bottom-middle-placement
  'Placement "Circulate mode window placement")
(defconfig *expose-mode-placement* 'top-left-child-placement
  'Placement "Expose mode window placement (Selection keys position)")
(defconfig *notify-window-placement* 'bottom-right-placement
  'Placement "Notify window placement")
(defconfig *ask-close/kill-placement* 'top-right-placement
  'Placement "Ask close/kill window placement")



(defparameter *in-process-existing-windows* nil)

;; For debug - redefine defun
;;(shadow :defun)

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


