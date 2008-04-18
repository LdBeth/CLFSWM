;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Main functions
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





;;; Main mode hooks
(defun handle-key-press (&rest event-slots &key root code state &allow-other-keys)
  (declare (ignore event-slots root))
  (funcall-key-from-code *main-keys* code state))


(defun handle-button-press (&rest event-slots &key code state window root-x root-y &allow-other-keys)
  (declare (ignore event-slots))
  (unless (funcall-button-from-code *main-mouse* code state window root-x root-y *fun-press*)
    (replay-button-event)))


(defun handle-button-release (&rest event-slots &key code state window root-x root-y &allow-other-keys)
  (declare (ignore event-slots))
  (unless (funcall-button-from-code *main-mouse* code state window root-x root-y *fun-release*)
    (replay-button-event)))

(defun handle-motion-notify (&rest event-slots &key root-x root-y &allow-other-keys)
  (declare (ignore event-slots))
  (unless (compress-motion-notify)
    (funcall-button-from-code *main-mouse* 'motion 0 root-x root-y *fun-press*)))


(defun handle-configure-request (&rest event-slots &key stack-mode #|parent|# window #|above-sibling|#
				 x y width height border-width value-mask &allow-other-keys)
  (declare (ignore event-slots))
  (labels ((has-x (mask) (= 1 (logand mask 1)))
    	   (has-y (mask) (= 2 (logand mask 2)))
    	   (has-w (mask) (= 4 (logand mask 4)))
    	   (has-h (mask) (= 8 (logand mask 8)))
	   (has-bw (mask) (= 16 (logand mask 16)))
	   (has-stackmode (mask) (= 64 (logand mask 64)))
	   (adjust-from-request ()
	     (when (has-x value-mask) (setf (xlib:drawable-x window) x))
	     (when (has-y value-mask) (setf (xlib:drawable-y window) y))
	     (when (has-h value-mask) (setf (xlib:drawable-height window) height))
	     (when (has-w value-mask) (setf (xlib:drawable-width window) width))))
    (with-xlib-protect
	(xlib:with-state (window)
	  (when (has-bw value-mask)
	    (setf (xlib:drawable-border-width window) border-width))
	  (if (find-child window *current-root*)
	      (case (window-type window)
		(:normal (adapt-child-to-father window (find-father-frame window *current-root*))
			 (send-configuration-notify window))
		(t (adjust-from-request)))
	      (adjust-from-request))
	  (when (has-stackmode value-mask)
	    (case stack-mode
	      (:above (raise-window window))))))))




(defun handle-configure-notify (&rest event-slots)
  (declare (ignore event-slots)))




(defun handle-map-request (&rest event-slots &key window send-event-p &allow-other-keys)
  (declare (ignore event-slots))
  (unless send-event-p
    (unhide-window window)
    (process-new-window window)
    (xlib:map-window window)
    (show-all-children)))

(defun handle-unmap-notify (&rest event-slots &key send-event-p event-window window &allow-other-keys)
  (declare (ignore event-slots))
  (unless (and (not send-event-p)
	       (not (xlib:window-equal window event-window)))
    (when (find-child window *root-frame*)
      (remove-child-in-all-frames window)
      (show-all-children))))


(defun handle-destroy-notify (&rest event-slots &key send-event-p event-window window &allow-other-keys)
  (declare (ignore event-slots))
  (unless (or send-event-p
	      (xlib:window-equal window event-window))
    (when (find-child window *root-frame*)
      (remove-child-in-all-frames window)
      (show-all-children))))



(defun handle-enter-notify  (&rest event-slots &key root-x root-y &allow-other-keys)
  (declare (ignore event-slots root-x root-y)))



(defun handle-exposure   (&rest event-slots &key window &allow-other-keys)
  (declare (ignore event-slots))
  (awhen (find-frame-window window *current-root*)
    (display-frame-info it)))


(defun handle-create-notify (&rest event-slots)
  (declare (ignore event-slots)))






;;; CONFIG: Main mode hooks
(setf *key-press-hook* #'handle-key-press
      *configure-request-hook* #'handle-configure-request
      *configure-notify-hook* #'handle-configure-notify
      *destroy-notify-hook* #'handle-destroy-notify
      *enter-notify-hook* #'handle-enter-notify
      *exposure-hook* #'handle-exposure
      *map-request-hook* #'handle-map-request
      *unmap-notify-hook* #'handle-unmap-notify
      *create-notify-hook* #'handle-create-notify
      *button-press-hook* #'handle-button-press
      *button-release-hook* #'handle-button-release
      *motion-notify-hook* #'handle-motion-notify)




(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  ;;(dbg  event-key)
  (with-xlib-protect
      (case event-key
	(:button-press (call-hook *button-press-hook* event-slots))
	(:button-release (call-hook *button-release-hook* event-slots))
	(:motion-notify (call-hook *motion-notify-hook* event-slots))
	(:key-press (call-hook *key-press-hook* event-slots))
	(:configure-request (call-hook *configure-request-hook* event-slots))
	(:configure-notify (call-hook *configure-notify-hook* event-slots))
	(:map-request (call-hook *map-request-hook* event-slots))
	(:unmap-notify (call-hook *unmap-notify-hook* event-slots))
	(:destroy-notify (call-hook *destroy-notify-hook* event-slots))
	(:mapping-notify (call-hook *mapping-notify-hook* event-slots))
	(:property-notify (call-hook *property-notify-hook* event-slots))
	(:create-notify (call-hook *create-notify-hook* event-slots))
	(:enter-notify (call-hook *enter-notify-hook* event-slots))
	(:exposure (call-hook *exposure-hook* event-slots))))
  t)



(defun main-loop ()
  (loop
     (with-xlib-protect
       (xlib:display-finish-output *display*)
       (xlib:process-event *display* :handler #'handle-event))))
;;(dbg "Main loop finish" c)))))


(defun open-display (display-str protocol)
  (multiple-value-bind (host display-num) (parse-display-string display-str)
    (setf *display* (xlib:open-display host :display display-num :protocol protocol)
	  (getenv "DISPLAY") display-str)))


(defun init-display ()
  (setf *screen* (first (xlib:display-roots *display*))
	*root* (xlib:screen-root *screen*)
	*no-focus-window* (xlib:create-window :parent *root* :x 0 :y 0 :width 1 :height 1)
	*root-gc* (xlib:create-gcontext :drawable *root*
					:foreground (get-color *color-unselected*)
					:background (get-color "Black")
					:line-style :solid)
	*default-font* (xlib:open-font *display* *default-font-string*))
  (xgrab-init-pointer)
  (xgrab-init-keyboard)
  (xlib:map-window *no-focus-window*)
  (dbg *display*)
  (setf (xlib:window-event-mask *root*) (xlib:make-event-mask :substructure-redirect
							      :substructure-notify
							      :property-change
							      :exposure
							      :button-press
							      :button-release
							      :pointer-motion))
  ;;(intern-atoms *display*)
  (netwm-set-properties)
  (set-layout-once-documentation)
  (xlib:display-force-output *display*)
  (setf *child-selection* nil)
  (setf *root-frame* (create-frame :name "Root" :number 0) ;; :layout #'tile-space-layout)
	*current-root* *root-frame*
	*current-child* *current-root*)
  (call-hook *init-hook*)
  (process-existing-windows *screen*)
  (show-all-children *current-root*)
  (grab-main-keys)
  (xlib:display-finish-output *display*))



(defun xdg-config-home ()
  (pathname-directory (concatenate 'string (or (getenv "XDG_CONFIG_HOME")
					       (getenv "HOME"))
				   "/")))


(defun read-conf-file ()
  (let* ((user-conf (probe-file (merge-pathnames (user-homedir-pathname) #p".clfswmrc")))
	 (etc-conf (probe-file #p"/etc/clfswmrc"))
	 (config-user-conf (probe-file (make-pathname :directory (append (xdg-config-home) '("clfswm"))
						      :name "clfswmrc")))
	 (conf (or config-user-conf user-conf etc-conf)))
    (if conf
	(handler-case (load conf)
	  (error (c)
	    (format t "~2%*** Error loading configurtion file: ~A ***~&~A~%" conf c)
	    (values nil (format nil "~s" c) conf))
	  (:no-error (&rest args)
	    (declare (ignore args))
	    (values t nil conf)))
	(values t nil nil))))



(defun main (&key (display (or (getenv "DISPLAY") ":0")) protocol
	     (base-dir (directory-namestring (or *load-truename* ""))))
  (setf *contrib-dir* base-dir)
  (read-conf-file)
  (handler-case
      (open-display display protocol)
    (xlib:access-error (c)
      (format t "~&~A~&Maybe another window manager is running.~%" c)
      (force-output)
      (return-from main 'init-display-error)))
  (handler-case
      (init-display)
    (xlib:access-error (c)
      (ungrab-main-keys)
      (xlib:destroy-window *no-focus-window*)
      (xlib:close-display *display*)
      (format t "~&~A~&Maybe another window manager is running.~%" c)
      (force-output)
      (return-from main 'init-display-error)))
  (unwind-protect
       (catch 'exit-main-loop
	 (main-loop))
    (ungrab-main-keys)
    (xlib:destroy-window *no-focus-window*)
    (xlib:close-display *display*)))
      


