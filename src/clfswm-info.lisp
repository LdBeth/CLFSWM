;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Info function (see the end of this file for user definition
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

(defstruct info window gc font list ilw ilh x y max-x max-y)


(defun leave-info-mode (info)
  "Leave the info mode"
  (declare (ignore info))
  (throw 'exit-info-loop nil))

(defun mouse-leave-info-mode (window root-x root-y info)
  "Leave the info mode"
  (declare (ignore window root-x root-y info))
  (throw 'exit-info-loop nil))





(defun draw-info-window (info)
  (clear-pixmap-buffer (info-window info) (info-gc info))
  (loop for line in (info-list info)
     for y from 0 do
     (xlib:draw-glyphs *pixmap-buffer* (info-gc info)
		       (- (info-ilw info) (info-x info))
		       (- (+ (* (info-ilh info) y) (info-ilh info)) (info-y info))
		       (format nil "~A" line)))
  (copy-pixmap-buffer (info-window info) (info-gc info)))


;;;,-----
;;;| Key binding
;;;`-----

(define-info-key (#\q) 'leave-info-mode)
(define-info-key ("Return") 'leave-info-mode)
(define-info-key ("Escape") 'leave-info-mode)
(define-info-key (#\Space) 'leave-info-mode)

(define-info-key ("twosuperior")
    (defun info-banish-pointer (info)
      "Move the pointer to the lower right corner of the screen"
      (declare (ignore info))
      (banish-pointer)))

(define-info-key ("Down")
    (defun info-next-line (info)
      "Move one line down"
      (setf (info-y info) (min (+ (info-y info) (info-ilh info)) (info-max-y info)))
      (draw-info-window info)))

(define-info-key ("Up")
    (defun info-previous-line (info)
      "Move one line up"
      (setf (info-y info) (max (- (info-y info) (info-ilh info)) 0))
      (draw-info-window info)))

(define-info-key ("Left")
    (defun info-previous-char (info)
      "Move one char left"
      (setf (info-x info) (max (- (info-x info) (info-ilw info)) 0))
      (draw-info-window info)))

(define-info-key ("Right")
    (defun info-next-char (info)
      "Move one char right"
      (setf (info-x info) (min (+ (info-x info) (info-ilw info)) (info-max-x info)))
      (draw-info-window info)))


(define-info-key ("Home")
    (defun info-first-line (info)
      "Move to first line"
      (setf (info-x info) 0
	    (info-y info) 0)
      (draw-info-window info)))

(define-info-key ("End")
    (defun info-end-line (info)
      "Move to last line"
      (setf (info-x info) 0
	    (info-y info) (- (* (length (info-list info)) (info-ilh info)) (xlib:drawable-height (info-window info))))
      (draw-info-window info)))


(define-info-key ("Page_Down")
    (defun info-next-ten-lines (info)
      "Move ten lines down"
      (incf (info-y info) (* (info-ilh info) 10))
      (draw-info-window info)))

(define-info-key ("Page_Up")
    (defun info-previous-ten-lines (info)
      "Move ten lines up"
      (decf (info-y info) (* (info-ilh info) 10))
      (draw-info-window info)))




(defparameter *info-start-grab-x* nil)
(defparameter *info-start-grab-y* nil)


(defun info-begin-grab (window root-x root-y info)
  "Begin grab text"
  (declare (ignore window))
  (setf *info-start-grab-x* (min (max (+ root-x (info-x info)) 0) (info-max-x info))
	*info-start-grab-y* (min (max (+ root-y (info-y info)) 0) (info-max-y info)))
  (draw-info-window info))

(defun info-end-grab (window root-x root-y info)
  "End grab"
  (declare (ignore window))
  (setf (info-x info) (min (max (- *info-start-grab-x* root-x) 0) (info-max-x info))
	(info-y info) (min (max (- *info-start-grab-y* root-y) 0) (info-max-y info))
	*info-start-grab-x* nil
	*info-start-grab-y* nil)
  (draw-info-window info))

(defun info-mouse-next-line (window root-x root-y info)
  "Move one line down"
  (declare (ignore window root-x root-y))
  (setf (info-y info) (min (+ (info-y info) (info-ilh info)) (info-max-y info)))
  (draw-info-window info))

(defun info-mouse-previous-line (window root-x root-y info)
  "Move one line up"
  (declare (ignore window root-x root-y))
  (setf (info-y info) (max (- (info-y info) (info-ilh info)) 0))
  (draw-info-window info))


(defun info-mouse-motion (window root-x root-y info)
  "Grab text"
  (declare (ignore window))
  (when (and *info-start-grab-x* *info-start-grab-y*)
    (setf (info-x info) (min (max (- *info-start-grab-x* root-x) 0) (info-max-x info))
	  (info-y info) (min (max (- *info-start-grab-y* root-y) 0) (info-max-y info)))
    (draw-info-window info)))




(define-info-mouse (1) 'info-begin-grab 'info-end-grab)
(define-info-mouse (2) 'mouse-leave-info-mode)
(define-info-mouse (4) 'info-mouse-previous-line)
(define-info-mouse (5) 'info-mouse-next-line)
(define-info-mouse ('Motion) 'info-mouse-motion nil)


;;;,-----
;;;| Main mode
;;;`-----

(defun info-mode (info-list &key (x 0) (y 0) (width nil) (height nil))
  "Open the info mode. Info-list is a list of info: One string per line"
  (when info-list
    (let* ((pointer-grabbed (xgrab-pointer-p))
	   (keyboard-grabbed (xgrab-keyboard-p))
	   (font (xlib:open-font *display* *info-font-string*))
	   (ilw (xlib:max-char-width font))
	   (ilh (+ (xlib:max-char-ascent font) (xlib:max-char-descent font) 1))
	   (window (xlib:create-window :parent *root*
				       :x x :y y
				       :width (or width
						  (min (* (+ (loop for l in info-list maximize (length l)) 2) ilw)
						       (- (xlib:screen-width *screen*) 2 x)))
				       :height (or height
						   (min (+ (* (length info-list) ilh) (/ ilh 2))
							(- (xlib:screen-height *screen*) 2 y)))
				       :background (get-color *info-background*)
				       :colormap (xlib:screen-default-colormap *screen*)
				       :border-width 1
				       :border (get-color *info-border*)
				       :event-mask '(:exposure)))
	   (gc (xlib:create-gcontext :drawable window
				     :foreground (get-color *info-foreground*)
				     :background (get-color *info-background*)
				     :font font
				     :line-style :solid))
	   (info (make-info :window window :gc gc :x 0 :y 0 :list info-list
							    :font font :ilw ilw :ilh ilh
							    :max-x (* (loop for l in info-list maximize (length l)) ilw)
							    :max-y (* (length info-list) ilh))))
      (labels ((handle-key (&rest event-slots &key root code state &allow-other-keys)
		 (declare (ignore event-slots root))
		 (funcall-key-from-code *info-keys* code state info))
	       (handle-motion-notify (&rest event-slots &key root-x root-y &allow-other-keys)
		 (declare (ignore event-slots))
		 (unless (xlib:event-case (*display* :discard-p nil :peek-p t :timeout 0)
			   (:motion-notify () t))
		   (funcall-button-from-code *info-mouse* 'motion 0 window root-x root-y *fun-press* (list info))))
	       (handle-button-press (&rest event-slots &key window root-x root-y code state &allow-other-keys)
		 (declare (ignore event-slots))
		 (funcall-button-from-code *info-mouse* code state window root-x root-y *fun-press* (list info)))
	       (handle-button-release (&rest event-slots &key window root-x root-y code state &allow-other-keys)
		 (declare (ignore event-slots))
		 (funcall-button-from-code *info-mouse* code state window root-x root-y *fun-release* (list info)))
	       (info-handle-unmap-notify (&rest event-slots)
		 (apply #'handle-unmap-notify event-slots)
		 (draw-info-window info))
	       (info-handle-destroy-notify (&rest event-slots)
		 (apply #'handle-destroy-notify event-slots)
		 (draw-info-window info))
	       (handle-events (&rest event-slots &key display event-key &allow-other-keys)
		 (declare (ignore display))
		 (case event-key
		   (:key-press (apply #'handle-key event-slots) t)
		   (:button-press (apply #'handle-button-press event-slots) t)
		   (:button-release (apply #'handle-button-release event-slots) t)
		   (:motion-notify (apply #'handle-motion-notify event-slots) t)
		   (:map-request nil)
		   (:unmap-notify (apply #'info-handle-unmap-notify event-slots) t)
		   (:destroy-notify (apply #'info-handle-destroy-notify event-slots) t)
		   (:mapping-notify nil)
		   (:property-notify nil)
		   (:create-notify nil)
		   (:enter-notify nil)
		   (:exposure (draw-info-window info)))
		 t))
	(xlib:map-window window)
	(draw-info-window info)
	(xgrab-pointer *root* 68 69)
	(unless keyboard-grabbed
	  (xgrab-keyboard *root*))
	(unwind-protect
	     (catch 'exit-info-loop
	       (loop
		  (xlib:display-finish-output *display*)
		  (xlib:process-event *display* :handler #'handle-events)))
	  (if pointer-grabbed
	      (xgrab-pointer *root* 66 67)
	      (xungrab-pointer))
	  (unless keyboard-grabbed
	    (xungrab-keyboard))
	  (xlib:free-gcontext gc)
	  (xlib:destroy-window window)
	  (xlib:close-font font)
	  (wait-no-key-or-button-press))))))






(defun info-mode-menu (item-list &key (x 0) (y 0) (width nil) (height nil))
  "Open an info help menu.
Item-list is: '((key function) (key function))
or with explicit docstring: '((key function \"documentation 1\") (key function \"bla bla\") (key function)) 
key is a character, a keycode or a keysym"
  (let ((info-list nil)
	(action nil))
    (dolist (item item-list)
      (destructuring-bind (key function explicit-doc) (ensure-n-elems item 3)
	(push (format nil "~@(~A~): ~A" key (or explicit-doc
						(documentation function 'function)))
	      info-list)
	(define-info-key-fun (list key 0)
	    (lambda (&optional args)
	      (declare (ignore args))
	      (setf action function)
	      (throw 'exit-info-loop nil)))))
    (info-mode (nreverse info-list) :x x :y y :width width :height height)
    (dolist (item item-list)
      (let ((key (first item)))
	(undefine-info-key-fun (list key 0))))
    (when (fboundp action)
      (funcall action))))





(defun keys-from-list (list)
  "Produce a key menu based on list item"
  (loop for l in list
     for i from 0
     collect (list (code-char (+ (char-code #\a) i)) l)))


;;;,-----
;;;| CONFIG - Info mode functions
;;;`-----
(defun append-space (string)
  "Append spaces before Newline on each line"
  (with-output-to-string (stream)
    (loop for c across string do
	 (when (equal c #\Newline)
	   (princ " " stream))
	 (princ c stream))))


(defun show-key-binding (&rest hash-table-key)
  "Show the binding of each hash-table-key"
  (info-mode (split-string (append-space (with-output-to-string (stream)
					   (produce-doc hash-table-key
							stream)))
			   #\Newline)))


(defun show-global-key-binding ()
  "Show all key binding"
  (show-key-binding *main-keys* *main-mouse* *second-keys* *second-mouse*
		    *info-keys* *info-mouse*))

(defun show-main-mode-key-binding ()
  "Show the main mode binding"
  (show-key-binding *main-keys* *main-mouse*))

(defun show-second-mode-key-binding ()
  "Show the second mode key binding"
  (show-key-binding *second-keys* *second-mouse*))


(defun show-date ()
  "Show the current time and date"
  (info-mode (list (date-string))))






(defun info-on-shell (program)
  (let ((lines (do-shell program nil t)))
    (info-mode (loop for line = (read-line lines nil nil)
		  while line
		  collect line))))


(defun show-cpu-proc ()
  "Show current processes sorted by CPU usage"
  (info-on-shell "ps --cols=1000 --sort='-%cpu,uid,pgid,ppid,pid' -e -o user,pid,stime,pcpu,pmem,args"))

(defun show-mem-proc ()
  "Show current processes sorted by memory usage"
  (info-on-shell "ps --cols=1000 --sort='-vsz,uid,pgid,ppid,pid' -e -o user,pid,stime,pcpu,pmem,args"))

(defun show-xmms-status ()
  "Show the current xmms status"
  (info-on-shell "xmms-shell -e status"))

(defun show-xmms-playlist ()
  "Show the current xmms playlist"
  (info-on-shell "xmms-shell -e list"))


(defun xmms-info-menu ()
  "Open the xmms menu"
  (info-mode-menu '((#\s show-xmms-status)
		    (#\l show-xmms-playlist))))



(defun show-cd-info ()
  "Show the current CD track"
  (info-on-shell "pcd i"))

(defun show-cd-playlist ()
  "Show the current CD playlist"
  (info-on-shell "pcd mi"))

(defun info-on-cd-menu ()
  "Open the CD info menu"
  (info-mode-menu '((#\i show-cd-info)
		    (#\l show-cd-playlist))))


(defun show-version ()
  "Show the current CLFSWM version"
  (info-mode (list *version*)))

(defun help-on-clfswm ()
  "Open the help and info window"
  (info-mode-menu '((#\h show-global-key-binding)
		    (#\b show-main-mode-key-binding)
		    (#\t show-date)
		    (#\c show-cpu-proc)
		    (#\m show-mem-proc)
		    (#\x xmms-info-menu)
		    (#\v show-version)
		    (#\d info-on-cd-menu))))


(defun help-on-second-mode ()
  "Open the help and info window for the second mode"
  (info-mode-menu '((#\h show-global-key-binding)
		    (#\b show-second-mode-key-binding)
		    (#\t show-date)
		    (#\c show-cpu-proc)
		    (#\m show-mem-proc)
		    (#\x xmms-info-menu)
		    (#\v show-version)
		    (#\d info-on-cd-menu))))





