;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Expose functions - An expose like.
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2005-2015 Philippe Brochard <pbrochard@common-lisp.net>
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

(defparameter *expose-font* nil)
(defparameter *expose-selected-child* nil)

(defstruct expose-child number child key window gc string)

(defun leave-expose-mode ()
  "Leave the expose mode"
  (throw 'exit-expose-loop nil))

(defun valid-expose-mode ()
  "Valid the expose mode"
  (throw 'exit-expose-loop t))

(defun mouse-leave-expose-mode (window root-x root-y)
  "Leave the expose mode"
  (declare (ignore window root-x root-y))
  (throw 'exit-expose-loop nil))

(defun mouse-valid-expose-mode (window root-x root-y)
  "Valid the expose mode"
  (declare (ignore window root-x root-y))
  (throw 'exit-expose-loop t))


(defun expose-associate-keys ()
  (let* ((all nil)
         (new nil)
	 (all-numbers (loop for ec in *expose-child-list*
			 collect (expose-child-number ec))))
    (with-all-children-reversed (*root-frame* child)
      (unless (child-equal-p child *root-frame*)
        (push child all)
        (unless (member child *expose-child-list* :test #'child-equal-p :key #'expose-child-child)
	  (let ((number (find-free-number all-numbers)))
	    (push (make-expose-child :child child :number number :key (number->letter number)) new)
	    (push number all-numbers)))))
    (append (remove-if-not (lambda (x) (member x all :test #'child-equal-p)) *expose-child-list*
                           :key #'expose-child-child)
            (nreverse new))))





(defun expose-draw-letter ()
  (dolist (ex-child *expose-child-list*)
    (let ((window (expose-child-window ex-child))
          (gc (expose-child-gc ex-child)))
      (when (and window gc)
        (clear-pixmap-buffer window gc)
        (xlib:with-gcontext (gc :foreground (get-color (if (substring-equal *query-string* (expose-child-key ex-child))
                                                           *expose-foreground-letter*
                                                           *expose-foreground-letter-nok*))
                                :background (get-color (if (string-equal *query-string* (expose-child-key ex-child))
                                                           *expose-background-letter-match*
                                                           *expose-background*)))
          (xlib:draw-image-glyphs *pixmap-buffer* gc
                                  (xlib:max-char-width *expose-font*)
                                  (+ (xlib:font-ascent *expose-font*) (xlib:font-descent *expose-font*))
                                  (expose-child-key ex-child)))
        (xlib:draw-glyphs *pixmap-buffer* gc
                          (xlib:max-char-width *expose-font*)
                          (+ (* 2 (xlib:font-ascent *expose-font*)) (xlib:font-descent *expose-font*) 1)
                          (expose-child-string ex-child))
        (copy-pixmap-buffer window gc)))))


(defun expose-create-window (ex-child)
  (let ((child (expose-child-child ex-child)))
    (with-current-child (child)
      (let* ((string (format nil "~A"
                             (if *expose-show-window-title*
                                 (ensure-printable (child-fullname child))
                                 "")))
             (width (if *expose-show-window-title*
                        (min (* (xlib:max-char-width *expose-font*) (+ (length string) 2))
                             (- (child-width child) 4))
                        (* (xlib:max-char-width *expose-font*) 3)))
             (height (* (xlib:font-ascent *expose-font*) 3)))
        (with-placement (*expose-mode-placement* x y width height)
          (let* ((window (xlib:create-window :parent *root*
                                             :x x   :y y
                                             :width width   :height height
                                             :background (get-color *expose-background*)
                                             :border-width *border-size*
                                             :border (get-color *expose-border*)
                                             :colormap (xlib:screen-default-colormap *screen*)
                                             :event-mask '(:exposure :key-press)))
                 (gc (xlib:create-gcontext :drawable window
                                           :foreground (get-color *expose-foreground*)
                                           :background (get-color *expose-background*)
                                           :font *expose-font*
                                           :line-style :solid)))
            (setf (window-transparency window) *expose-transparency*)
            (map-window window)
            (setf (expose-child-window ex-child) window
                  (expose-child-gc ex-child) gc
                  (expose-child-string ex-child) string)))))))




(defun expose-query-key-press-hook (code state)
  (declare (ignore code state))
  (expose-draw-letter)
  (let ((two-letters-key (dolist (child *expose-child-list*)
			   (when (> (length (expose-child-key child)) 1)
			     (return t)))))
    (when (and *expose-direct-select* (not two-letters-key))
      (leave-query-mode :return))))

(defun expose-query-button-press-hook (code state x y)
  (declare (ignore state))
  (when (= code 1)
    (setf *expose-selected-child*
          (find (find-child-under-mouse x y) *expose-child-list* :test #'child-equal-p :key #'expose-child-child)))
  (leave-query-mode :click))


(defun expose-init ()
  (setf *expose-font* (xlib:open-font *display* *expose-font-string*)
	*expose-child-list* (expose-associate-keys)
	*expose-selected-child* nil
        *query-string* "")
  (xlib:warp-pointer *root* (truncate (/ (screen-width) 2))
		     (truncate (/ (screen-height) 2)))
  (add-hook *query-key-press-hook* 'expose-query-key-press-hook)
  (add-hook *query-button-press-hook* 'expose-query-button-press-hook))

(defun expose-present-windows ()
  (dolist (ex-child *expose-child-list*)
    (let ((child (expose-child-child ex-child)))
      (when (frame-p child)
        (setf (frame-data-slot child :old-layout) (frame-layout child)
              (frame-layout child) #'tile-space-layout))))
  (show-all-children t))

(defun expose-unpresent-windows ()
  (dolist (ex-child *expose-child-list*)
    (let ((child (expose-child-child ex-child)))
      (when (frame-p child)
        (setf (frame-layout child) (frame-data-slot child :old-layout)
              (frame-data-slot child :old-layout) nil)))))

(defun expose-mode-display-accel-windows ()
  (let ((all-hidden-windows (get-hidden-windows)))
	(with-all-root-child (root)
	  (with-all-children-reversed (root child)
		(let ((ex-child (find child *expose-child-list* :test #'child-equal-p :key #'expose-child-child)))
		  (when ex-child
			(if (or (frame-p (expose-child-child ex-child))
					(managed-window-p (expose-child-child ex-child)
									  (find-parent-frame (expose-child-child ex-child) *root-frame*)))
				(unless (child-member (expose-child-child ex-child) all-hidden-windows)
				  (expose-create-window ex-child))
				(hide-child (expose-child-child ex-child)))))))
	(expose-draw-letter)))


(defun expose-find-child-from-letters (letters)
  (find letters *expose-child-list* :test #'string-equal :key #'expose-child-key))

(defun expose-select-child ()
  (let ((*query-mode-placement* *expose-query-placement*))
    (multiple-value-bind (letters return)
        (query-string "Which child ?")
      (let ((ex-child (case return
                        (:return (expose-find-child-from-letters letters))
                        (:click *expose-selected-child*))))
        (when ex-child
          (expose-child-child ex-child))))))


(defun expose-restore-windows (&optional (present-window t))
  (remove-hook *query-key-press-hook* 'expose-query-key-press-hook)
  (remove-hook *query-button-press-hook* 'expose-query-button-press-hook)
  (dolist (ex-child *expose-child-list*)
    (awhen (expose-child-gc ex-child)
      (xlib:free-gcontext it))
    (awhen (expose-child-window ex-child)
      (xlib:destroy-window it))
    (setf (expose-child-gc ex-child) nil
          (expose-child-window ex-child) nil))
  (when *expose-font*
    (xlib:close-font *expose-font*))
  (when present-window
	(expose-unpresent-windows)))

(defun expose-focus-child (child)
  (let ((parent (typecase child
                  (xlib:window (find-parent-frame child))
                  (frame child))))
    (when (and child parent)
	  (change-root (find-root parent) parent)
      (setf (current-child) child)
      (focus-all-children child parent t))))

(defun expose-do-main (&optional (present-window t))
  (stop-button-event)
  (expose-init)
  (when present-window
	(expose-present-windows))
  (expose-mode-display-accel-windows)
  (let ((child (expose-select-child)))
	(expose-restore-windows present-window)
	child))


(defun expose-windows-mode ()
  "Present all windows in currents roots (An expose like)"
  (awhen (expose-do-main)
    (expose-focus-child it))
  (show-all-children)
  t)


(defun expose-all-windows-mode ()
  "Present all windows in all frames (An expose like)"
  (let ((child nil))
    (with-saved-root-list ()
      (dolist (root (get-root-list))
        (change-root root (root-original root)))
      (setf child (expose-do-main)))
    (when child
      (expose-focus-child child)))
  (show-all-children)
  t)

(defun expose-current-child-mode ()
  "Present all windows in currents roots (An expose like)"
  (with-saved-root-list ()
	(awhen (expose-do-main nil)
	  (expose-focus-child it)))
  (show-all-children)
  t)
