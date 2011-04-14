;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Main functions
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

(defparameter *circulate-window* nil)
(defparameter *circulate-font* nil)
(defparameter *circulate-gc* nil)

(defparameter *circulate-hit* 0)
(defparameter *circulate-orig* nil)
(defparameter *circulate-parent* nil)

(defun draw-circulate-mode-window ()
  (raise-window *circulate-window*)
  (clear-pixmap-buffer *circulate-window* *circulate-gc*)
  (let* ((text (format nil "~A [~A]"
		       (limit-length (ensure-printable (child-name (xlib:input-focus *display*)))
				     *circulate-text-limite*)
		       (limit-length (ensure-printable (child-name *current-child*))
				     *circulate-text-limite*)))
	 (len (length text)))
    (xlib:draw-glyphs *pixmap-buffer* *circulate-gc*
		      (truncate (/ (- *circulate-width* (* (xlib:max-char-width *circulate-font*) len)) 2))
		      (truncate (/ (+ *circulate-height* (- (xlib:font-ascent *circulate-font*) (xlib:font-descent *circulate-font*))) 2))
		      text))
  (copy-pixmap-buffer *circulate-window* *circulate-gc*))



(defun leave-circulate-mode ()
  "Leave the circulate mode"
  (throw 'exit-circulate-loop nil))



(defun reset-circulate-child ()
  (setf *circulate-hit* 0
	*circulate-parent* nil
	*circulate-orig* (frame-child *current-child*)))

(defun reset-circulate-brother ()
  (setf *circulate-parent* (find-parent-frame *current-child*))
  (when (frame-p *circulate-parent*)
    (setf *circulate-orig* (frame-child *circulate-parent*))))



(defun reorder-child (direction)
  (no-focus)
  (with-slots (child) *current-child*
    (unless *circulate-orig*
      (reset-circulate-child))
    (let ((len (length *circulate-orig*)))
      (when (plusp len)
	(let ((elem (nth (mod (incf *circulate-hit* direction) len) *circulate-orig*)))
	  (setf child (cons elem (child-remove elem *circulate-orig*)))))
      (show-all-children)
      (draw-circulate-mode-window))))


(defun reorder-brother (direction)
  (no-focus)
  (let ((frame-is-root? (and (child-equal-p *current-root* *current-child*)
			     (not (child-equal-p *current-root* *root-frame*)))))
    (select-current-frame nil)
    (unless (and *circulate-orig* *circulate-parent*)
      (reset-circulate-brother))
    (let ((len (length *circulate-orig*)))
      (when (plusp len)
	(when (frame-p *circulate-parent*)
	  (let ((elem (nth (mod  (incf *circulate-hit* direction) len) *circulate-orig*)))
	    (setf (frame-child *circulate-parent*) (cons elem (child-remove elem *circulate-orig*))
		  *current-child* (frame-selected-child *circulate-parent*))))
	(when frame-is-root?
	  (setf *current-root* *current-child*))))
    (show-all-children t)
    (draw-circulate-mode-window)))

(defun reorder-subchild (direction)
  (declare (ignore direction))
  (when (frame-p *current-child*)
    (let ((selected-child (frame-selected-child *current-child*)))
      (when (frame-p selected-child)
	(no-focus)
	(with-slots (child) selected-child
	  (let ((elem (first (last child))))
	    (setf child (cons elem (child-remove elem child)))
	    (show-all-children)
	    (draw-circulate-mode-window)))))))





(defun circulate-select-next-child ()
  "Select the next child"
  (when (frame-p *current-child*)
    (when *circulate-parent*
      (reset-circulate-child))
    (reorder-child +1)))

(defun circulate-select-previous-child ()
  "Select the previous child"
  (when (frame-p *current-child*)
    (when *circulate-parent*
      (reset-circulate-child))
    (reorder-child -1)))


(defun circulate-select-next-brother ()
  "Select the next brother"
  (unless *circulate-parent*
    (reset-circulate-brother))
  (reorder-brother +1))

(defun circulate-select-previous-brother ()
  "Select the previous borther"
  (unless *circulate-parent*
    (reset-circulate-brother))
  (reorder-brother -1))

(defun circulate-select-next-subchild ()
  "Select the next subchild"
  (reorder-subchild +1))



(add-hook *binding-hook* 'set-default-circulate-keys)

(defun set-default-circulate-keys ()
  (define-circulate-key ("Escape") 'leave-circulate-mode)
  (define-circulate-key ("g" :control) 'leave-circulate-mode)
  (define-circulate-key ("Escape" :alt) 'leave-circulate-mode)
  (define-circulate-key ("g" :control :alt) 'leave-circulate-mode)
  (define-circulate-key ("Tab" :mod-1) 'circulate-select-next-child)
  (define-circulate-key ("Tab" :mod-1 :control) 'circulate-select-next-subchild)
  (define-circulate-key ("Tab" :mod-1 :shift) 'circulate-select-previous-child)
  (define-circulate-key ("Iso_Left_Tab" :mod-1 :shift) 'circulate-select-previous-child)
  (define-circulate-key ("Right" :mod-1) 'circulate-select-next-brother)
  (define-circulate-key ("Left" :mod-1) 'circulate-select-previous-brother)
  (define-circulate-release-key ("Alt_L" :alt) 'leave-circulate-mode)
  (define-circulate-release-key ("Alt_L") 'leave-circulate-mode))


(defun circulate-leave-function ()
  (when *circulate-gc*
    (xlib:free-gcontext *circulate-gc*))
  (when *circulate-window*
    (xlib:destroy-window *circulate-window*))
  (when *circulate-font*
    (xlib:close-font *circulate-font*))
  (xlib:display-finish-output *display*)
  (setf *circulate-window* nil
	*circulate-gc* nil
	*circulate-font* nil))

(defun circulate-loop-function ()
  (unless (is-a-key-pressed-p)
    (leave-circulate-mode)))

(define-handler circulate-mode :key-press (code state)
  (unless (funcall-key-from-code *circulate-keys* code state)
    (setf *circulate-hit* 0
	  *circulate-orig* nil
	  *circulate-parent* nil)
    (funcall-key-from-code *main-keys* code state)))


(define-handler circulate-mode :key-release (code state)
  (funcall-key-from-code *circulate-keys-release* code state))



(defun circulate-mode (&key child-direction brother-direction subchild-direction)
  (setf *circulate-hit* 0)
  (with-placement (*circulate-mode-placement* x y *circulate-width* *circulate-height*)
    (setf *circulate-font* (xlib:open-font *display* *circulate-font-string*)
	  *circulate-window* (xlib:create-window :parent *root*
						 :x x
						 :y y
						 :width *circulate-width*
						 :height *circulate-height*
						 :background (get-color *circulate-background*)
						 :border-width *border-size*
						 :border (get-color *circulate-border*)
						 :colormap (xlib:screen-default-colormap *screen*)
						 :event-mask '(:exposure :key-press))
	  *circulate-gc* (xlib:create-gcontext :drawable *circulate-window*
					       :foreground (get-color *circulate-foreground*)
					       :background (get-color *circulate-background*)
					       :font *circulate-font*
					       :line-style :solid))
    (map-window *circulate-window*)
    (draw-circulate-mode-window)
    (when child-direction
      (reorder-child child-direction))
    (when brother-direction
      (reorder-brother brother-direction))
    (when subchild-direction
      (reorder-subchild subchild-direction))
    (let ((grab-keyboard-p (xgrab-keyboard-p))
	  (grab-pointer-p (xgrab-pointer-p)))
      (xgrab-pointer *root* 92 93)
      (unless grab-keyboard-p
	(ungrab-main-keys)
	(xgrab-keyboard *root*))
      (generic-mode 'circulate-mode 'exit-circulate-loop
		    :loop-function #'circulate-loop-function
		    :leave-function #'circulate-leave-function
		    :original-mode '(main-mode))
      (circulate-leave-function)
      (unless grab-keyboard-p
	(xungrab-keyboard)
	(grab-main-keys))
      (if grab-pointer-p
	  (xgrab-pointer *root* 66 67)
	  (xungrab-pointer)))))


(defun select-next-child ()
  "Select the next child"
  (when (frame-p *current-child*)
    (setf *circulate-orig* (frame-child *current-child*)
	  *circulate-parent* nil)
    (circulate-mode :child-direction +1)))

(defun select-previous-child ()
  "Select the previous child"
  (when (frame-p *current-child*)
    (setf *circulate-orig* (frame-child *current-child*)
	  *circulate-parent* nil)
    (circulate-mode :child-direction -1)))


(defun select-next-brother ()
  "Select the next brother"
  (setf *circulate-parent* (find-parent-frame *current-child*))
  (when (frame-p *circulate-parent*)
    (setf *circulate-orig* (frame-child *circulate-parent*)))
  (circulate-mode :brother-direction +1))

(defun select-previous-brother ()
  "Select the previous brother"
  (setf *circulate-parent* (find-parent-frame *current-child*))
  (when (frame-p *circulate-parent*)
    (setf *circulate-orig* (frame-child *circulate-parent*)))
  (circulate-mode :brother-direction -1))

(defun select-next-subchild ()
  "Select the next subchild"
  (when (and (frame-p *current-child*)
	     (frame-p (frame-selected-child *current-child*)))
    (setf *circulate-orig* (frame-child *current-child*)
	  *circulate-parent* nil)
    (circulate-mode :subchild-direction +1)))


(defun select-next-child-simple ()
  "Select the next child (do not enter in circulate mode)"
  (when (frame-p *current-child*)
    (with-slots (child) *current-child*
      (setf child (rotate-list child)))
    (show-all-children)))



(defun reorder-brother-simple (reorder-fun)
  (unless (child-equal-p *current-child* *current-root*)
    (no-focus)
    (select-current-frame nil)
    (let ((parent-frame (find-parent-frame *current-child*)))
      (when (frame-p parent-frame)
        (with-slots (child) parent-frame
          (setf child (funcall reorder-fun child)
                *current-child* (frame-selected-child parent-frame))))
      (show-all-children t))))


(defun select-next-brother-simple ()
  "Select the next brother frame (do not enter in circulate mode)"
  (reorder-brother-simple #'rotate-list))

(defun select-previous-brother-simple ()
  "Select the previous brother frame (do not enter in circulate mode)"
  (reorder-brother-simple #'anti-rotate-list))
