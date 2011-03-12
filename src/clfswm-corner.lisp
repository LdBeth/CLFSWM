;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Corner functions
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



(symbol-macrolet ((sw (xlib:screen-width *screen*))
		  (sh (xlib:screen-height *screen*))
		  (cs *corner-size*))
  (defun in-corner (corner x y)
    "Return t if (x, y) is in corner.
Corner is one of :bottom-right :bottom-left :top-right :top-left"
    (multiple-value-bind (xmin ymin xmax ymax)
	(case corner
	  (:bottom-right (values (- sw cs)  (- sh cs)  sw  sh))
	  (:bottom-left (values 0  (- sh cs)  cs  sh))
	  (:top-left (values 0 0 cs cs))
	  (:top-right (values (- sw cs)  0  sw  cs))
	  (t (values 10 10 0 0)))
      (and (<= xmin x xmax)
	   (<= ymin y ymax)))))


(symbol-macrolet ((sw (xlib:screen-width *screen*))
		  (sh (xlib:screen-height *screen*))
		  (cs *corner-size*))
  (defun find-corner (x y)
    (cond ((and (< cs x (- sw cs)) (< cs y (- sh cs))) nil)
	  ((and (<= 0 x cs) (<= 0 y cs)) :top-left)
	  ((and (<= (- sw cs) x sw) (<= 0 y cs)) :top-right)
	  ((and (<= 0 x cs) (<= (- sh cs) y sh)) :bottom-left)
	  ((and (<= (- sw cs) x sw) (<= (- sh cs) y sh)) :bottom-right)
	  (t nil))))




(defun do-corner-action (x y corner-list)
  (when (frame-p *current-root*)
    (let ((corner (find-corner x y)))
      (when corner
	(let ((fun (second (assoc corner corner-list))))
	  (when fun
	    (funcall fun)))))))





;;;***************************************;;;
;;; CONFIG - Corner actions definitions:  ;;;
;;;***************************************;;;
(defun find-window-in-query-tree (target-win)
  (dolist (win (xlib:query-tree *root*))
    (when (child-equal-p win target-win)
      (return t))))

(defun wait-window-in-query-tree (wait-test)
  (loop
     (dolist (win (xlib:query-tree *root*))
       (when (funcall wait-test win)
	 (return-from wait-window-in-query-tree win)))))


(defun generic-present-body (cmd wait-test win &optional focus-p)
  (stop-button-event)
  (unless (find-window-in-query-tree win)
    (do-shell cmd)
    (setf win (wait-window-in-query-tree wait-test))
    (grab-all-buttons win)
    (hide-window win))
  (cond ((window-hidden-p win)
	 (unhide-window win)
	 (when focus-p
	   (focus-window win))
	 (raise-window win))
	(t (hide-window win)
	   (show-all-children)))
  win)



(let (win)
  (defun close-virtual-keyboard ()
    (when win
      (xlib:destroy-window win)
      (xlib:display-finish-output *display*)
      (setf win nil)))
  (defun present-virtual-keyboard ()
    "Present a virtual keyboard"
    (setf win (generic-present-body *virtual-keyboard-cmd*
				    (lambda (win)
				      (string-equal (xlib:get-wm-class win) "xvkbd"))
				    win))
    t))


(let (win)
  (defun equal-clfswm-terminal (window)
    (when win
      (xlib:window-equal window win)))
  (defun close-clfswm-terminal ()
    (when win
      (xlib:destroy-window win)
      (xlib:display-finish-output *display*)
      (setf win nil)))
  (defun present-clfswm-terminal ()
    "Hide/Unhide a terminal"
    (setf win (generic-present-body *clfswm-terminal-cmd*
				    (lambda (win)
				      (string-equal (xlib:wm-name win) *clfswm-terminal-name*))
				    win
				    t))
    t))


