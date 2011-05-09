;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: OSD (On Screen Display) for presentations.
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

(in-package :clfswm)

;; Uncomment the line above if you want to use the old OSD method
;;(pushnew :DISPLAY-OSD *features*)

#-DISPLAY-OSD
(progn
  (defparameter *osd-window* nil)
  (defparameter *osd-gc* nil)
  (defparameter *osd-font* nil)
  (defparameter *osd-font-string* "-*-fixed-*-*-*-*-14-*-*-*-*-*-*-1"))


;;; A more complex example I use to record my desktop and show
;;; documentation associated to each key press.
#+DISPLAY-OSD
(defun display-doc (function code state)
  (let* ((modifiers (state->modifiers state))
	 (keysym (keysym->keysym-name (xlib:keycode->keysym *display* code 0))))
    (do-shell "pkill osd_cat")
    (do-shell (format nil "( echo '~A~A' | osd_cat -d 3 -p bottom -c white -o -50 -f -*-fixed-*-*-*-*-14-*-*-*-*-*-*-1 ) &"
		      (if keysym
			  (format nil "~:(~{~A+~}~A~)" modifiers keysym)
			  "Menu")
		      (aif (documentation (first function) 'function)
			   (format nil ": ~A" it) "")))))

#-DISPLAY-OSD
(defun is-osd-window-p (win)
  (xlib:window-equal win *osd-window*))


#-DISPLAY-OSD
(defun display-doc (function code state &optional button-p)
  (unless *osd-window*
    (setf *osd-window* (xlib:create-window :parent *root*
					   :x 0 :y (- (xlib:drawable-height *root*) 25)
					   :width (xlib:drawable-width *root*) :height 25
					   :background (get-color "black")
					   :border-width 1
					   :border (get-color "black")
					   :colormap (xlib:screen-default-colormap *screen*)
					   :event-mask '(:exposure))
	  *osd-font* (xlib:open-font *display* *osd-font-string*)
	  *osd-gc* (xlib:create-gcontext :drawable *osd-window*
					 :foreground (get-color "white")
					 :background (get-color "gray10")
					 :font *osd-font*
					 :line-style :solid))
    (map-window *osd-window*))
  (let* ((modifiers (state->modifiers state))
	 (keysym (keysym->keysym-name (xlib:keycode->keysym *display* code 0))))
    (when (frame-p *current-child*)
      (push (list #'is-osd-window-p nil) *never-managed-window-list*))
    (raise-window *osd-window*)
    (rotatef (xlib:gcontext-foreground *osd-gc*) (xlib:gcontext-background *osd-gc*))
    (xlib:draw-rectangle *osd-window* *osd-gc*
			 0 0 (xlib:drawable-width *osd-window*) (xlib:drawable-height *osd-window*)
			 t)
    (rotatef (xlib:gcontext-foreground *osd-gc*) (xlib:gcontext-background *osd-gc*))
    (xlib:draw-glyphs *osd-window* *osd-gc* 20 15
                      (format nil "~A~A"
                              (cond (button-p (format nil "~:(~{~A+~}Button-~A~)" modifiers code))
                                    (keysym (format nil "~:(~{~A+~}~A~)" modifiers keysym))
                                    (t "Menu"))
                              (aif (documentation (first function) 'function)
                                   (format nil ": ~A" (substitute #\Space #\Newline it)) "")))
    (xlib:display-finish-output *display*)))


(fmakunbound 'funcall-key-from-code)
(defun funcall-key-from-code (hash-table-key code state &rest args)
  (let ((function (find-key-from-code hash-table-key code state)))
    (when function
      (display-doc function code state)
      (apply (first function) (append args (second function)))
      t)))


(fmakunbound 'funcall-button-from-code)
(defun funcall-button-from-code (hash-table-key code state window root-x root-y
				 &optional (action *fun-press*) args)
  (let ((state (modifiers->state (set-difference (state->modifiers state)
						 '(:button-1 :button-2 :button-3 :button-4 :button-5)))))
    (multiple-value-bind (function foundp)
	(gethash (list code state) hash-table-key)
      (if (and foundp (funcall action function))
	  (progn
            (unless (equal code 'motion)
              (display-doc function code state t))
	    (apply (funcall action function) `(,window ,root-x ,root-y ,@(append args (third function))))
	    t)
	  nil))))


(fmakunbound 'get-fullscreen-size)
;;; CONFIG - Screen size
(defun get-fullscreen-size ()
  "Return the size of root child (values rx ry rw rh)
You can tweak this to what you want"
  (values -2 -2 (+ (xlib:screen-width *screen*) 2) (- (xlib:screen-height *screen*) 25)))


(fmakunbound 'open-menu-do-action)
;;; Display menu functions
(defun open-menu-do-action (action menu parent)
  (typecase action
    (menu (open-menu action (cons menu parent)))
    (null (awhen (first parent)
	    (open-menu it (rest parent))))
    (t (when (fboundp action)
	 (display-doc (list action) 0 0)
	 (funcall action)))))

(fmakunbound 'bottom-left-placement)
(defun bottom-left-placement (&optional (width 0) (height 0))
  (declare (ignore width))
  (values 0
	  (- (xlib:screen-height *screen*) height 26)))

(fmakunbound 'bottom-middle-placement)
(defun bottom-middle-placement (&optional (width 0) (height 0))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  (- (xlib:screen-height *screen*) height 26)))

(fmakunbound 'bottom-right-placement)
(defun bottom-right-placement (&optional (width 0) (height 0))
  (values (- (xlib:screen-width *screen*) width 1)
	  (- (xlib:screen-height *screen*) height 26)))
