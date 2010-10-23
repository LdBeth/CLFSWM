;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: OSD (On Screen Display) for presentations.
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2010 Philippe Brochard <hocwp@free.fr>
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

(in-package :clfswm)


;;; A more complex example I use to record my desktop and show
;;; documentation associated to each key press.
(defun display-doc (function code state)
  (let* ((modifiers (state->modifiers state))
	 (keysym (keysym->keysym-name (xlib:keycode->keysym *display* code 0))))
    (do-shell "pkill osd_cat")
    (do-shell (format nil "echo '~A~A' | osd_cat -d 3 -p bottom -c white -o -50 -f -*-fixed-*-*-*-*-14-*-*-*-*-*-*-1"
		      (if keysym
			  (format nil "~:(~{~A+~}~A~)" modifiers keysym)
			  "Menu")
		      (aif (documentation (first function) 'function)
			   (format nil ": ~A" it) "")))
    (force-output)))


(defun funcall-key-from-code (hash-table-key code state &rest args)
  (let ((function (find-key-from-code hash-table-key code state)))
    (when function
      (display-doc function code state)
      (apply (first function) (append args (second function)))
      t)))

;;; CONFIG - Screen size
(defun get-fullscreen-size ()
  "Return the size of root child (values rx ry rw rh)
You can tweak this to what you want"
  (values -2 -2 (+ (xlib:screen-width *screen*) 2) (- (xlib:screen-height *screen*) 25)))


;;; Display menu functions
(defun open-menu-do-action (action menu parent)
  (typecase action
    (menu (open-menu action (cons menu parent)))
    (null (awhen (first parent)
	    (open-menu it (rest parent))))
    (t (when (fboundp action)
	 (display-doc (list action) 0 0)
	 (funcall action)))))


(defun bottom-left-placement (&optional (width 0) (height 0))
  (declare (ignore width))
  (values 0
	  (- (xlib:screen-height *screen*) height 26)))

(defun bottom-middle-placement (&optional (width 0) (height 0))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  (- (xlib:screen-height *screen*) height 26)))

(defun bottom-right-placement (&optional (width 0) (height 0))
  (values (- (xlib:screen-width *screen*) width 1)
	  (- (xlib:screen-height *screen*) height 26)))
