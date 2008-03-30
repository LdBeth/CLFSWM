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


;;; Conversion functions
;;; Float -> Pixel conversion
(defun x-fl->px (x father)
  "Convert float X coordinate to pixel"
  (round (+ (* x (frame-rw father)) (frame-rx father))))

(defun y-fl->px (y father)
  "Convert float Y coordinate to pixel"
  (round (+ (* y (frame-rh father)) (frame-ry father))))

(defun w-fl->px (w father)
  "Convert float Width coordinate to pixel"
  (round (* w (frame-rw father))))

(defun h-fl->px (h father)
  "Convert float Height coordinate to pixel"
  (round (* h (frame-rh father))))

;;; Pixel -> Float conversion
(defun x-px->fl (x father)
  "Convert pixel X coordinate to float"
  (/ (- x (frame-rx father)) (frame-rw father)))

(defun y-px->fl (y father)
  "Convert pixel Y coordinate to float"
  (/ (- y (frame-ry father)) (frame-rh father)))

(defun w-px->fl (w father)
  "Convert pixel Width coordinate to float"
  (/ w (frame-rw father)))

(defun h-px->fl (h father)
  "Convert pixel Height coordinate to float"
  (/ h (frame-rh father)))







(defgeneric frame-p (frame))
(defmethod frame-p ((frame frame))
  (declare (ignore frame))
  t)
(defmethod frame-p (frame)
  (declare (ignore frame))
  nil)



;;; Frame data manipulation functions
(defun frame-data-slot (frame slot)
  "Return the value associated to data slot"
  (when (frame-p frame)
    (second (assoc slot (frame-data frame)))))

(defun set-frame-data-slot (frame slot value)
  "Set the value associated to data slot"
  (when (frame-p frame)
    (with-slots (data) frame
      (setf data (remove (assoc slot data) data))
      (push (list slot value) data))
    value))

(defsetf frame-data-slot set-frame-data-slot)






(defgeneric child-name (child))

(defmethod child-name ((child xlib:window))
  (xlib:wm-name child))

(defmethod child-name ((child frame))
  (frame-name child))

(defmethod child-name (child)
  (declare (ignore child))
  "???")



(defgeneric rename-child (child name))

(defmethod rename-child ((child frame) name)
  (setf (frame-name child) name))

(defmethod rename-child ((child xlib:window) name)
  (setf (xlib:wm-name child) name))

(defmethod rename-child (child name)
  (declare (ignore child name)))



;; (with-all-children (*root-frame* child) (typecase child (xlib:window (print child)) (frame (print (frame-number child)))))
(defmacro with-all-children ((root child) &body body)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(labels ((,rec (,child)
		,@body
		(when (frame-p ,child)
		  (dolist (,sub-child (frame-child ,child))
		    (,rec ,sub-child)))))
       (,rec ,root))))


;; (with-all-frames (*root-frame* frame) (print (frame-number frame)))
(defmacro with-all-frames ((root frame) &body body)
  (let ((rec (gensym))
	(child (gensym)))
    `(labels ((,rec (,frame)
		(when (frame-p ,frame)
		  ,@body
		  (dolist (,child (frame-child ,frame))
		    (,rec ,child)))))
       (,rec ,root))))


;; (with-all-windows (*root-frame* window) (print window))
(defmacro with-all-windows ((root window) &body body)
  (let ((rec (gensym))
	(child (gensym)))
    `(labels ((,rec (,window)
		(when (xlib:window-p ,window)
		  ,@body)
		(when (frame-p ,window)
		  (dolist (,child (frame-child ,window))
		    (,rec ,child)))))
       (,rec ,root))))



;; (with-all-frames-windows (*root-frame* child) (print child) (print (frame-number child)))
(defmacro with-all-windows-frames ((root child) body-window body-frame)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(labels ((,rec (,child)
		(typecase ,child
		  (xlib:window ,body-window)
		  (frame ,body-frame
			 (dolist (,sub-child (frame-child ,child))
			   (,rec ,sub-child))))))
       (,rec ,root))))



(defun frame-find-free-number ()
  (let ((all-numbers nil))
    (with-all-frames (*root-frame* frame)
      (push (frame-number frame) all-numbers))
    (find-free-number all-numbers)))


(defun create-frame (&rest args &key (number (frame-find-free-number)) &allow-other-keys)
  (let* ((window (xlib:create-window :parent *root*
				     :x 0
				     :y 0
				     :width 200
				     :height 200
				     :background (get-color "Black")
				     :colormap (xlib:screen-default-colormap *screen*)
				     :border-width 1
				     :border (get-color "Red")
				     :event-mask '(:exposure :button-press :button-release :pointer-motion)))
	 (gc (xlib:create-gcontext :drawable window
				   :foreground (get-color "Green")
				   :background (get-color "Black")
				   :font *default-font*
				   :line-style :solid)))
    (apply #'make-instance 'frame :number number :window window :gc gc args)))



(defun add-frame (frame father)
  (push frame (frame-child father)))


(defun place-frame (frame father prx pry prw prh)
  "Place a frame from real (pixel) coordinates"
  (with-slots (window x y w h) frame
    (setf (xlib:drawable-x window) prx
	  (xlib:drawable-y window) pry
	  (xlib:drawable-width window) prw
	  (xlib:drawable-height window) prh
	  x (x-px->fl prx father)
	  y (y-px->fl pry father)
	  w (w-px->fl prw father)
	  h (h-px->fl prh father))))





;;(defun get-current-child ()
;;  "Return the current focused child"
;;  (unless (equal *current-child* *root-frame*)
;;    (typecase *current-child*
;;      (xlib:window *current-child*)
;;      (frame (if (xlib:window-p (first (frame-child *current-child*)))
;;		 (first (frame-child *current-child*))
;;		 *current-child*)))))


(defun find-child (to-find root)
  "Find to-find in root or in its children"
  (with-all-children (root child)
    (when (equal child to-find)
      (return-from find-child t))))



(defun find-father-frame (to-find &optional (root *root-frame*))
  "Return the father frame of to-find"
  (with-all-frames (root frame)
    (when (member to-find (frame-child frame))
      (return-from find-father-frame frame))))

  

(defun find-frame-window (window &optional (root *root-frame*))
  "Return the frame with the window window"
  (with-all-frames (root frame)
    (when (xlib:window-equal window (frame-window frame))
      (return-from find-frame-window frame))))


(defun find-frame-by-name (name)
  "Find a frame from its name"
  (when name
    (with-all-frames (*root-frame* frame)
      (when (string-equal name (frame-name frame))
	(return-from find-frame-by-name frame)))))

(defun find-frame-by-number (number)
  "Find a frame from its number"
  (when (numberp number)
    (with-all-frames (*root-frame* frame)
      (when (= number (frame-number frame))
	(return-from find-frame-by-number frame)))))




(defun get-all-windows (&optional (root *root-frame*))
  "Return all windows in root and in its children"
  (let ((acc nil))
    (with-all-windows (root window)
      (push window acc))
    acc))


(defun get-hidden-windows ()
  "Return all hiddens windows"
  (let ((all-windows (get-all-windows))
	(hidden-windows (remove-if-not #'window-hidden-p
				       (copy-list (xlib:query-tree *root*)))))
    (set-difference hidden-windows all-windows)))




(defun display-frame-info (frame)
  (let ((dy (+ (xlib:max-char-ascent *default-font*) (xlib:max-char-descent *default-font*))))
    (with-slots (name number gc window child) frame
      (when (equal frame *current-root*)
	(xlib:clear-area window))
      (setf (xlib:gcontext-foreground gc) (get-color (if (and (equal frame *current-root*)
							      (equal frame *current-child*))
							 "Red" "Green")))
      (xlib:draw-image-glyphs window gc 5 dy		 
			      (format nil "Frame: ~A~A                                                  "
				      number
				      (if name  (format nil " - ~A" name) "")))
      (let ((pos dy))
	(when (equal frame *current-root*)
	  (xlib:draw-image-glyphs window gc 5 (incf pos dy)
				  (format nil "~A hidden windows             " (length (get-hidden-windows))))
	  (when *child-selection*
	    (xlib:draw-image-glyphs window gc 5 (incf pos dy)
				    (with-output-to-string (str)
				      (format str "Selection: ")
				      (dolist (child *child-selection*)
					(typecase child
					  (xlib:window (format str "~A " (xlib:wm-name child)))
					  (frame (format str "frame:~A[~A] " (frame-number child)
							 (aif (frame-name child) it "")))))
				      (format str "                                                   ")))))
	(dolist (ch child)
	  (when (xlib:window-p ch)
	    (xlib:draw-glyphs window gc 5 (incf pos dy) (ensure-printable (xlib:wm-name ch)))))))))










(defun get-father-layout (child father)
  (if (frame-p father)
      (aif (frame-layout father)
	   (funcall it child father)
	   (no-layout child father))
      (get-fullscreen-size)))


(defgeneric adapt-child-to-father (child father))

(defmethod adapt-child-to-father ((window xlib:window) father)
  (with-xlib-protect
      (multiple-value-bind (nx ny nw nh raise-p)
	  (get-father-layout window father)
	(setf (xlib:drawable-x window) nx
	      (xlib:drawable-y window) ny
	      (xlib:drawable-width window) nw
	      (xlib:drawable-height window) nh)
	raise-p)))

(defmethod adapt-child-to-father ((frame frame) father)
  (with-xlib-protect
      (multiple-value-bind (nx ny nw nh raise-p)
	  (get-father-layout frame father)
	(with-slots (rx ry rw rh window) frame
	  (setf rx nx  ry ny  rw nw  rh nh)
	  (setf (xlib:drawable-x window) rx
		(xlib:drawable-y window) ry
		(xlib:drawable-width window) rw
		(xlib:drawable-height window) rh)
	  raise-p))))
   




(defun raise-if-needed (window raise-p first-p)
  (when (or (eql raise-p t)
	    (and (eql raise-p :first-only) first-p))
    (raise-window window)))

(defgeneric show-child (child father first-p))

(defmethod show-child ((frame frame) father first-p)
  (with-xlib-protect
      (with-slots (window) frame
	(let ((raise-p (adapt-child-to-father frame father)))
	  (when (or *show-root-frame-p* (not (equal frame *current-root*)))
	    (setf (xlib:window-background window) (get-color "Black"))
	    (xlib:map-window window)
	    (raise-if-needed window raise-p first-p)
	    (display-frame-info frame))))))


(defmethod show-child ((window xlib:window) father first-p)
  (with-xlib-protect
      (let ((raise-p nil))
	(when (eql (window-type window) :normal)
	  (setf raise-p (adapt-child-to-father window father)))
	(xlib:map-window window)
	(raise-if-needed window raise-p first-p))))



(defgeneric hide-child (child))

(defmethod hide-child ((frame frame))
  (with-xlib-protect
      (with-slots (window) frame
	(xlib:unmap-window window))))

(defmethod hide-child ((window xlib:window))
  (hide-window window))






(defgeneric select-child (child selected))

(defmethod select-child ((frame frame) selected)
  (with-xlib-protect
      (when (and (frame-p frame) (frame-window frame))
	(setf (xlib:window-border (frame-window frame))
	      (get-color (cond ((equal selected :maybe) *color-maybe-selected*)
			       ((equal selected nil) *color-unselected*)
			       (selected *color-selected*)))))))

(defmethod select-child ((window xlib:window) selected)
  (with-xlib-protect
      (setf (xlib:window-border window)
	    (get-color (cond ((equal selected :maybe) *color-maybe-selected*)
			     ((equal selected nil) *color-unselected*)
			     (selected *color-selected*))))))

(defun select-current-frame (selected)
  (select-child *current-child* selected))



(defun set-focus-to-current-child ()
  (labels ((rec (child)
	     (typecase child
	       (xlib:window (focus-window child))
	       (frame (rec (first (frame-child child)))))))
    (no-focus)
    (rec *current-child*)))





(defun show-all-children ()
  "Show all children from *current-root*"
  (labels ((rec (root father first-p first-father)
	     (show-child root father first-p)
	     (select-child root (if (equal root *current-child*) t
				    (if (and first-p first-father) :maybe nil)))
	     (when (frame-p root)
	       (let ((first-child (first (frame-child root))))
		 (dolist (child (reverse (frame-child root)))
		   (rec child root (equal child first-child) first-p))))))
    (rec *current-root* nil t t)
    (set-focus-to-current-child)))



(defun hide-all-children (root)
  "Hide all root children"
  (when (frame-p root)
    (dolist (child (frame-child root))
      (hide-all child))))

(defun hide-all (root)
  "Hide root and all its children"
  (hide-child root)
  (hide-all-children root))




(defun select-next/previous-brother (fun-rotate)
  "Select the next/previous brother frame"
  (let ((frame-is-root? (and (equal *current-root* *current-child*)
			     (not (equal *current-root* *root-frame*)))))
    (if frame-is-root?
	(hide-all *current-root*)
	(select-current-frame nil))
    (let ((father (find-father-frame *current-child*)))
      (when (frame-p father)
	(with-slots (child) father
	  (setf child (funcall fun-rotate child))
	  (setf *current-child* (first child)))))
    (when frame-is-root?
      (setf *current-root* *current-child*))
    (show-all-children)))


(defun select-next-brother ()
  "Select the next brother frame"
  (select-next/previous-brother #'anti-rotate-list))

(defun select-previous-brother ()
  "Select the previous brother frame"
  (select-next/previous-brother #'rotate-list))


(defun select-next-level ()
  "Select the next level in frame"
  (select-current-frame :maybe)
  (when (frame-p *current-child*)
    (awhen (first (frame-child *current-child*))
	   (setf *current-child* it)))
  (select-current-frame t))

(defun select-previous-level ()
  "Select the previous level in frame"
  (unless (equal *current-child* *current-root*)
    (select-current-frame :maybe)
    (awhen (find-father-frame *current-child*)
	   (setf *current-child* it))
    (select-current-frame t)))



(defun select-next/previous-child (fun-rotate)
  "Select the next/previous child"
  (when (frame-p *current-child*)
    (with-slots (child) *current-child*
      (setf child (funcall fun-rotate child)))
    (show-all-children)))


(defun select-next-child ()
  "Select the next child"
  (select-next/previous-child #'anti-rotate-list))

(defun select-previous-child ()
  "Select the previous child"
  (select-next/previous-child #'rotate-list))



(defun enter-frame ()
  "Enter in the selected frame - ie make it the root frame"
  (hide-all *current-root*)
  (setf *current-root* *current-child*)
  (show-all-children))

(defun leave-frame ()
  "Leave the selected frame - ie make its father the root frame"
  (hide-all *current-root*)
  (awhen (find-father-frame *current-root*)
	 (when (frame-p it)
	   (setf *current-root* it)))
  (show-all-children))


(defun switch-to-root-frame ()
  "Switch to the root frame"
  (hide-all *current-root*)
  (setf *current-root* *root-frame*)
  (show-all-children))

(defun switch-and-select-root-frame ()
  "Switch and select the root frame"
  (hide-all *current-root*)
  (setf *current-root* *root-frame*)
  (setf *current-child* *current-root*)
  (show-all-children))


(defun toggle-show-root-frame ()
  "Show/Hide the root frame"
  (hide-all *current-root*)
  (setf *show-root-frame-p* (not *show-root-frame-p*))
  (show-all-children))


(defun focus-child (child father)
  "Focus child - Return true if something has change"
  (when (and (frame-p father)
	     (member child (frame-child father)))
    (when (not (equal child (first (frame-child father))))
      (loop until (equal child (first (frame-child father)))
	 do (setf (frame-child father) (rotate-list (frame-child father))))
      t)))

(defun focus-child-rec (child father)
  "Focus child and its fathers - Return true if something has change"
  (let ((change nil))
    (labels ((rec (child father)
	       (when (focus-child child father)
		 (setf change t))
	       (when father
		 (rec father (find-father-frame father)))))
      (rec child father))
    change))


(defun set-current-child-generic (child)
  (unless (equal *current-child* child)
    (setf *current-child* child)
    t))

(defgeneric set-current-child (child father window-father))

(defmethod set-current-child ((child xlib:window) father window-father)
  (set-current-child-generic (if window-father father child)))

(defmethod set-current-child ((child frame) father window-father)
  (declare (ignore father window-father))
  (set-current-child-generic child))


(defun set-current-root (father)
  "Set current root if father is not in current root"
  (unless (find-child father *current-root*)
    (setf *current-root* father)))


(defun focus-all-children (child father &optional (window-father t))
  "Focus child and its fathers -
For window: set current child to window or its father according to window-father"
  (let ((new-focus (focus-child-rec child father))
	(new-current-child (set-current-child child father window-father))
	(new-root (set-current-root father)))
    (or new-focus new-current-child new-root)))



(defun remove-child-in-frame (child frame)
  "Remove the child in frame"
  (when (frame-p frame)
    (setf (frame-child frame) (remove child (frame-child frame) :test #'equal))))

(defun remove-child-in-frames (child root)
  "Remove child in the frame root and in all its children"
  (with-all-frames (root frame)
    (remove-child-in-frame child frame))
  (when (xlib:window-p child)
    (netwm-remove-in-client-list child)))



(defun remove-child-in-all-frames (child)
  "Remove child in all frames from *root-frame*"
  (when (equal child *current-root*)
    (setf *current-root* (find-father-frame child)))
  (when (equal child *current-child*)
    (setf *current-child* *current-root*))
  (remove-child-in-frames child *root-frame*))





(defun place-window-from-hints (window)
  "Place a window from its hints"
  (with-xlib-protect
    (let* ((hints (xlib:wm-normal-hints window))
	   (min-width (or (and hints (xlib:wm-size-hints-min-width hints)) 0))
	   (min-height (or (and hints (xlib:wm-size-hints-min-height hints)) 0))
	   (max-width (or (and hints (xlib:wm-size-hints-max-width hints)) (xlib:drawable-width *root*)))
	   (max-height (or (and hints (xlib:wm-size-hints-max-height hints)) (xlib:drawable-height *root*)))
	   (rwidth (or (and hints (or (xlib:wm-size-hints-width hints) (xlib:wm-size-hints-base-width hints)))
		       (xlib:drawable-width window)))
	   (rheight (or (and hints (or (xlib:wm-size-hints-height hints) (xlib:wm-size-hints-base-height hints)))
			(xlib:drawable-height window))))
      (setf (xlib:drawable-width window) (min (max min-width rwidth) max-width)
	    (xlib:drawable-height window) (min (max min-height rheight) max-height))
      (setf (xlib:drawable-x window) (truncate (+ (frame-rx *current-child*) (/ (- (frame-rw *current-child*) (xlib:drawable-width window)) 2)))
	    (xlib:drawable-y window) (truncate (+ (frame-ry *current-child*) (/ (- (frame-rh *current-child*) (xlib:drawable-height window)) 2)))))))



(defun do-all-frames-nw-hook (window)
  "Call nw-hook of each frame."
  (let ((found nil))
    (with-all-frames (*root-frame* frame)
      (awhen (frame-nw-hook frame)
	(call-hook it (list frame window))
	(setf found t)))
    found))



(defun process-new-window (window)
  "When a new window is created (or when we are scanning initial
windows), this function dresses the window up and gets it ready to be
managed."
  (with-xlib-protect
    (setf (xlib:window-event-mask window) *window-events*)
    (set-window-state window +normal-state+)
    (setf (xlib:drawable-border-width window) (case (window-type window)
						(:normal 1)
						(:maxsize 1)
						(:transient 1)
						(t 1)))
    (grab-all-buttons window)
;;    (when (frame-p *current-child*) ;; PHIL: Remove this!!!
;;      (setf (frame-nw-hook *current-child*) #'open-in-new-frame-nw-hook))
    (unless (do-all-frames-nw-hook window)
      (default-frame-nw-hook nil window))
    (netwm-add-in-client-list window)))




(defun hide-existing-windows (screen)
  "Hide all existing windows in screen"
  (dolist (win (xlib:query-tree (xlib:screen-root screen)))
    (hide-window win)))

(defun process-existing-windows (screen)
  "Windows present when clfswm starts up must be absorbed by clfswm."
  (let ((id-list nil)
	(all-windows (get-all-windows)))
    (dolist (win (xlib:query-tree (xlib:screen-root screen)))
      (unless (member win all-windows)
	(let ((map-state (xlib:window-map-state win))
	      (wm-state (window-state win)))
	  (unless (or (eql (xlib:window-override-redirect win) :on)
		      (eql win *no-focus-window*))
	    (when (or (eql map-state :viewable)
		      (eql wm-state +iconic-state+))
	      (format t "Processing ~S: type=~A ~S~%" (xlib:wm-name win) (window-type win)win)
	      (unhide-window win)
	      (process-new-window win)
	      (xlib:map-window win)
	      (raise-window win)
	      (pushnew (xlib:window-id win) id-list))))))
    (netwm-set-client-list id-list)))
