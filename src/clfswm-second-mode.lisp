;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Second mode functions
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

(in-package :clfswm)

(defparameter *sm-window* nil)
(defparameter *sm-font* nil)
(defparameter *sm-gc* nil)

(defparameter *second-mode-program* nil
  "Execute the program string if not nil")


;;(defun draw-second-mode-window ()
;;  (xlib:clear-area *sm-window*)
;;  (let* ((text (format nil "Workspace ~A ~:(~A~) ~A ~A ~A"
;;		       (workspace-number (current-workspace))
;;		       (if *arrow-action* *arrow-action* "")
;;		       (if *motion-action* *motion-action* "")
;;		       (cond ((numberp *open-next-window-in-new-workspace*)
;;			      (format nil ">W:~A" *open-next-window-in-new-workspace*))
;;			     (*open-next-window-in-new-workspace* ">W")
;;			     (t ""))
;;		       (cond ((equal *open-next-window-in-new-group* :once) ">G")
;;			     (*open-next-window-in-new-group* ">G+")
;;			     (t ""))))
;;	 (len (length text)))
;;    (xlib:draw-image-glyphs *sm-window* *sm-gc*
;;			    (truncate (/ (- *sm-width* (* (xlib:max-char-width *sm-font*) len)) 2))
;;			    (truncate (/ (+ *sm-height* (- (font-ascent *sm-font*) (font-descent *sm-font*))) 2))
;;			    text)))


(defun draw-second-mode-window ()
  (raise-window *sm-window*)
  (xlib:clear-area *sm-window*)
  (let* ((text (format nil "Second mode"))
	 (len (length text)))
    (xlib:draw-image-glyphs *sm-window* *sm-gc*
			    (truncate (/ (- *sm-width* (* (xlib:max-char-width *sm-font*) len)) 2))
			    (truncate (/ (+ *sm-height* (- (xlib:font-ascent *sm-font*) (xlib:font-descent *sm-font*))) 2))
			    text)))




;;; Second mode hooks
(defun sm-handle-key-press (&rest event-slots &key root code state &allow-other-keys)
  (declare (ignore event-slots root))
  (funcall-key-from-code *second-keys* code state)
  (draw-second-mode-window))

(defun sm-handle-enter-notify (&rest event-slots &key root-x root-y &allow-other-keys)
  (declare (ignore event-slots root-x root-y))
  ;;  (focus-group-under-mouse root-x root-y)
  (draw-second-mode-window))

(defun sm-handle-motion-notify (&rest event-slots &key root-x root-y &allow-other-keys)
  (declare (ignore event-slots))
  (unless (compress-motion-notify)
    (funcall-button-from-code *second-mouse* 'motion 0 root-x root-y #'first)))

(defun sm-handle-button-press (&rest event-slots &key window root-x root-y code state &allow-other-keys)
  (declare (ignore event-slots))
  (funcall-button-from-code *second-mouse* code state window root-x root-y #'first)
  (draw-second-mode-window))

(defun sm-handle-button-release (&rest event-slots &key window root-x root-y code state &allow-other-keys)
  (declare (ignore event-slots))
  (funcall-button-from-code *second-mouse* code state window root-x root-y #'third)
  (draw-second-mode-window))

(defun sm-handle-configure-request (&rest event-slots)
  (apply #'handle-configure-request event-slots)
  (draw-second-mode-window))


(defun sm-handle-configure-notify (&rest event-slots)
  (apply #'handle-configure-notify event-slots)
  (draw-second-mode-window))


(defun sm-handle-destroy-notify (&rest event-slots)
  (apply #'handle-destroy-notify event-slots)
  (draw-second-mode-window))

(defun sm-handle-map-request (&rest event-slots)
  (apply #'handle-map-request event-slots)
  (draw-second-mode-window))

(defun sm-handle-unmap-notify (&rest event-slots)
  (apply #'handle-unmap-notify event-slots)
  (draw-second-mode-window))

(defun sm-handle-exposure (&rest event-slots)
  (apply #'handle-exposure event-slots)
  (draw-second-mode-window))



;;(defun sm-handle-property-notify (&rest event-slots &key window &allow-other-keys)
;;  ;;(dbg (xlib:wm-name window))
;;  (draw-second-mode-window))


;;; CONFIG: Second mode hooks
(setf *sm-button-press-hook* #'sm-handle-button-press
      *sm-button-release-hook* #'sm-handle-button-release
      *sm-motion-notify-hook* #'sm-handle-motion-notify
      *sm-key-press-hook* #'sm-handle-key-press
      *sm-configure-request-hook* #'sm-handle-configure-request
      *sm-configure-notify-hook* #'sm-handle-configure-notify
      *sm-destroy-notify-hook* #'sm-handle-destroy-notify
      *sm-enter-notify-hook* #'sm-handle-enter-notify
      *sm-exposure-hook* #'sm-handle-exposure
      *sm-map-request-hook* #'sm-handle-map-request
      *sm-unmap-notify-hook* #'sm-handle-unmap-notify)





(defun sm-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  ;;(dbg event-key)
  (with-xlib-protect
    (case event-key
      (:button-press (call-hook *sm-button-press-hook* event-slots))
      (:button-release (call-hook *sm-button-release-hook* event-slots))
      (:motion-notify (call-hook *sm-motion-notify-hook* event-slots))
      (:key-press (call-hook *sm-key-press-hook* event-slots))
      (:configure-request (call-hook *sm-configure-request-hook* event-slots))
      (:configure-notify (call-hook *sm-configure-notify-hook* event-slots))
      (:map-request (call-hook *sm-map-request-hook* event-slots))
      (:unmap-notify (call-hook *sm-unmap-notify-hook* event-slots))
      (:destroy-notify (call-hook *sm-destroy-notify-hook* event-slots))
      (:mapping-notify (call-hook *sm-mapping-notify-hook* event-slots))
      (:property-notify (call-hook *sm-property-notify-hook* event-slots))
      (:create-notify (call-hook *sm-create-notify-hook* event-slots))
      (:enter-notify (call-hook *sm-enter-notify-hook* event-slots))
      (:exposure (call-hook *sm-exposure-hook* event-slots))))
  ;;(dbg "Ignore handle event" c event-slots)))
  t)



(defun second-key-mode ()
  "Switch to editing mode"
  ;;(dbg "Second key ignore" c)))))
  (setf *sm-window* (xlib:create-window :parent *root*
					:x (truncate (/ (- (xlib:screen-width *screen*) *sm-width*) 2))
					:y 0
					:width *sm-width* :height *sm-height*
					:background (get-color *sm-background-color*)
					:border-width 1
					:border (get-color *sm-border-color*)
					:colormap (xlib:screen-default-colormap *screen*)
					:event-mask '(:exposure))
	*sm-font* (xlib:open-font *display* *sm-font-string*)
	*sm-gc* (xlib:create-gcontext :drawable *sm-window*
				      :foreground (get-color *sm-foreground-color*)
				      :background (get-color *sm-background-color*)
				      :font *sm-font*
				      :line-style :solid))
  (xlib:map-window *sm-window*)
  (draw-second-mode-window)
  (no-focus)
  (ungrab-main-keys)
  (xgrab-keyboard *root*)
  (xgrab-pointer *root* 66 67)
  (unwind-protect
       (catch 'exit-second-loop
	 (loop
	    (raise-window *sm-window*)
	    (xlib:display-finish-output *display*)
	    (xlib:process-event *display* :handler #'sm-handle-event)
	    (xlib:display-finish-output *display*)))
    (xlib:free-gcontext *sm-gc*)
    (xlib:close-font *sm-font*)
    (xlib:destroy-window *sm-window*)
    (xungrab-keyboard)
    (xungrab-pointer)
    (grab-main-keys)
    (show-all-childs))
  (wait-no-key-or-button-press)
  (when *second-mode-program*
    (do-shell *second-mode-program*)
    (setf *second-mode-program* nil)))



(defun leave-second-mode ()
  "Leave second mode"
  (banish-pointer)
  (throw 'exit-second-loop nil))




