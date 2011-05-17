;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Second mode functions
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

(defparameter *sm-window* nil)
(defparameter *sm-font* nil)
(defparameter *sm-gc* nil)

(defparameter *second-mode-leave-function* nil
  "Execute the function if not nil")


(defun draw-second-mode-window ()
  (raise-window *sm-window*)
  (clear-pixmap-buffer *sm-window* *sm-gc*)
  (let* ((text (format nil "Second mode"))
	 (len (length text)))
    (xlib:draw-glyphs *pixmap-buffer* *sm-gc*
		      (truncate (/ (- *sm-width* (* (xlib:max-char-width *sm-font*) len)) 2))
		      (truncate (/ (+ *sm-height* (- (xlib:font-ascent *sm-font*) (xlib:font-descent *sm-font*))) 2))
		      text))
  (copy-pixmap-buffer *sm-window* *sm-gc*))




;;; Second mode handlers
(define-handler second-mode :key-press (code state)
  (funcall-key-from-code *second-keys* code state)
  (draw-second-mode-window))

(define-handler second-mode :enter-notify ()
  (draw-second-mode-window))

(define-handler second-mode :motion-notify (window root-x root-y)
  (unless (compress-motion-notify)
    (funcall-button-from-code *second-mouse* 'motion
			      (modifiers->state *default-modifiers*)
			      window root-x root-y *fun-press*)))

(define-handler second-mode :button-press (window root-x root-y code state)
  (funcall-button-from-code *second-mouse* code state window root-x root-y *fun-press*)
  (draw-second-mode-window))

(define-handler second-mode :button-release (window root-x root-y code state)
  (funcall-button-from-code *second-mouse* code state window root-x root-y *fun-release*)
  (draw-second-mode-window))

(define-handler second-mode :configure-request ()
  (apply #'handle-event-fun-main-mode-configure-request event-slots)
  (draw-second-mode-window))


(define-handler second-mode :configure-notify ()
  (draw-second-mode-window))


(define-handler second-mode :destroy-notify ()
  (apply #'handle-event-fun-main-mode-destroy-notify event-slots)
  (draw-second-mode-window))

(define-handler second-mode :map-request ()
  (apply #'handle-event-fun-main-mode-map-request event-slots)
  (draw-second-mode-window))

(define-handler second-mode :unmap-notify ()
  (apply #'handle-event-fun-main-mode-unmap-notify event-slots)
  (draw-second-mode-window))

(define-handler second-mode :exposure ()
  (apply #'handle-event-fun-main-mode-exposure event-slots)
  (draw-second-mode-window))




(defun sm-enter-function ()
  (with-placement (*second-mode-placement* x y *sm-width* *sm-height*)
    (setf *in-second-mode* t
	  *sm-window* (xlib:create-window :parent *root*
					  :x x :y y
					  :width *sm-width* :height *sm-height*
					  :background (get-color *sm-background-color*)
					  :border-width *border-size*
					  :border (get-color *sm-border-color*)
					  :colormap (xlib:screen-default-colormap *screen*)
					  :event-mask '(:exposure))
	  *sm-font* (xlib:open-font *display* *sm-font-string*)
	  *sm-gc* (xlib:create-gcontext :drawable *sm-window*
					:foreground (get-color *sm-foreground-color*)
					:background (get-color *sm-background-color*)
					:font *sm-font*
					:line-style :solid)))
  (map-window *sm-window*)
  (draw-second-mode-window)
  (no-focus)
  (ungrab-main-keys)
  (xgrab-keyboard *root*)
  (xgrab-pointer *root* 66 67)
  (speed-mouse-reset))

(defun sm-loop-function ()
  (raise-window *sm-window*))

(defun sm-leave-function ()
  (xlib:free-gcontext *sm-gc*)
  (xlib:close-font *sm-font*)
  (xlib:destroy-window *sm-window*)
  (xungrab-keyboard)
  (xungrab-pointer)
  (grab-main-keys)
  (show-all-children)
  (display-all-frame-info)
  (wait-no-key-or-button-press)
  (setf *in-second-mode* nil))

(defun second-key-mode ()
  "Switch to editing mode (second mode)"
  (generic-mode 'second-mode
		'exit-second-loop
		:enter-function #'sm-enter-function
		:loop-function #'sm-loop-function
		:leave-function #'sm-leave-function)
  (when *second-mode-leave-function*
    (funcall *second-mode-leave-function*)
    (setf *second-mode-leave-function* nil)))

(defun leave-second-mode ()
  "Leave second mode"
  (cond (*in-second-mode*
	 (setf *in-second-mode* nil)
	 (throw 'exit-second-loop nil))
	(t (setf *in-second-mode* nil)
	   (show-all-children))))


(defun sm-delete-focus-window ()
  "Close focus window: Delete the focus window in all frames and workspaces"
  (setf *second-mode-leave-function* 'delete-focus-window)
  (leave-second-mode))

(defun sm-ask-close/kill-current-window ()
  "Close or kill the current window (ask before doing anything)"
  (setf *second-mode-leave-function* #'ask-close/kill-current-window)
  (leave-second-mode))
