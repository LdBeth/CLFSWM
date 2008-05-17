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
(defun x-fl->px (x parent)
  "Convert float X coordinate to pixel"
  (round (+ (* x (frame-rw parent)) (frame-rx parent))))

(defun y-fl->px (y parent)
  "Convert float Y coordinate to pixel"
  (round (+ (* y (frame-rh parent)) (frame-ry parent))))

(defun w-fl->px (w parent)
  "Convert float Width coordinate to pixel"
  (round (* w (frame-rw parent))))

(defun h-fl->px (h parent)
  "Convert float Height coordinate to pixel"
  (round (* h (frame-rh parent))))

;;; Pixel -> Float conversion
(defun x-px->fl (x parent)
  "Convert pixel X coordinate to float"
  (/ (- x (frame-rx parent)) (frame-rw parent)))

(defun y-px->fl (y parent)
  "Convert pixel Y coordinate to float"
  (/ (- y (frame-ry parent)) (frame-rh parent)))

(defun w-px->fl (w parent)
  "Convert pixel Width coordinate to float"
  (/ w (frame-rw parent)))

(defun h-px->fl (h parent)
  "Convert pixel Height coordinate to float"
  (/ h (frame-rh parent)))







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


(defun managed-window-p (window frame)
  "Return t only if window is managed by frame"
  (if (frame-p frame)
      (with-slots ((managed forced-managed-window)
		   (unmanaged forced-unmanaged-window)) frame
	(and (not (member window unmanaged))
	     (not (member (xlib:wm-name window) unmanaged :test #'string-equal-p))
	     (or (member :all (frame-managed-type frame))
		 (member (window-type window) (frame-managed-type frame))
		 (member window managed)
		 (member (xlib:wm-name window) managed :test #'string-equal-p))))
      t))





(defgeneric child-name (child))

(defmethod child-name ((child xlib:window))
  (xlib:wm-name child))

(defmethod child-name ((child frame))
  (frame-name child))

(defmethod child-name (child)
  (declare (ignore child))
  "???")


(defgeneric child-fullname (child))

(defmethod child-fullname ((child xlib:window))
  (format nil "~A (~A)" (xlib:wm-name child) (xlib:get-wm-class child)))

(defmethod child-fullname ((child frame))
  (aif (frame-name child)
       (format nil "~A (Frame ~A)" it (frame-number child))
       (format nil "Frame ~A" (frame-number child))))

(defmethod child-fullname (child)
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
		  (dolist (,sub-child (reverse (frame-child ,child)))
		    (,rec ,sub-child)))))
       (,rec ,root))))


;; (with-all-frames (*root-frame* frame) (print (frame-number frame)))
(defmacro with-all-frames ((root frame) &body body)
  (let ((rec (gensym))
	(child (gensym)))
    `(labels ((,rec (,frame)
		(when (frame-p ,frame)
		  ,@body
		  (dolist (,child (reverse (frame-child ,frame)))
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
		  (dolist (,child (reverse (frame-child ,window)))
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
			 (dolist (,sub-child (reverse (frame-child ,child)))
			   (,rec ,sub-child))))))
       (,rec ,root))))

(defmacro with-all-windows-frames-and-parent ((root child parent) body-window body-frame)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(labels ((,rec (,child ,parent)
		(typecase ,child
		  (xlib:window ,body-window)
		  (frame ,body-frame
			 (dolist (,sub-child (reverse (frame-child ,child)))
			   (,rec ,sub-child ,child))))))
       (,rec ,root nil))))



(defun frame-find-free-number ()
  (let ((all-numbers nil))
    (with-all-frames (*root-frame* frame)
      (pushnew (frame-number frame) all-numbers))
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



(defun add-frame (frame parent)
  (push frame (frame-child parent))
  frame)


(defun place-frame (frame parent prx pry prw prh)
  "Place a frame from real (pixel) coordinates"
  (when (and (frame-p frame) (frame-p parent))
    (with-slots (window x y w h) frame
      (setf (xlib:drawable-x window) prx
	    (xlib:drawable-y window) pry
	    (xlib:drawable-width window) prw
	    (xlib:drawable-height window) prh
	    x (x-px->fl prx parent)
	    y (y-px->fl pry parent)
	    w (w-px->fl prw parent)
	    h (h-px->fl prh parent)))))

(defun fixe-real-size (frame parent)
  "Fixe real (pixel) coordinates in float coordinates"
  (when (frame-p frame)
    (with-slots (x y w h rx ry rw rh) frame
      (setf x (x-px->fl rx parent)
	    y (y-px->fl ry parent)
	    w (w-px->fl rw parent)
	    h (h-px->fl rh parent)))))

(defun fixe-real-size-current-child ()
  "Fixe real (pixel) coordinates in float coordinates for children in the current child"
  (when (frame-p *current-child*)
    (dolist (child (frame-child *current-child*))
      (fixe-real-size child *current-child*))))





(defun find-child (to-find root)
  "Find to-find in root or in its children"
  (with-all-children (root child)
    (when (equal child to-find)
      (return-from find-child t))))



(defun find-parent-frame (to-find &optional (root *root-frame*))
  "Return the parent frame of to-find"
  (with-all-frames (root frame)
    (when (member to-find (frame-child frame))
      (return-from find-parent-frame frame))))

  

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


(defun find-child-in-parent (child base)
  "Return t if child is in base or in its parents"
  (labels ((rec (base)
	     (when (equal child base)
	       (return-from find-child-in-parent t))
	     (let ((parent (find-parent-frame base)))
	       (when parent
		 (rec parent)))))
    (rec base)))




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
      (clear-pixmap-buffer window gc)
      (setf (xlib:gcontext-foreground gc) (get-color (if (and (equal frame *current-root*)
							      (equal frame *current-child*))
							 "Red" "Green")))
      (xlib:draw-glyphs *pixmap-buffer* gc 5 dy		 
			(format nil "Frame: ~A~A"
				number
				(if name  (format nil " - ~A" name) "")))
      (let ((pos dy))
	(when (equal frame *current-root*)
	  (xlib:draw-glyphs *pixmap-buffer* gc 5 (incf pos dy)
			    (format nil "~A hidden windows" (length (get-hidden-windows))))
	  (when *child-selection*
	    (xlib:draw-glyphs *pixmap-buffer* gc 5 (incf pos dy)
			      (with-output-to-string (str)
				(format str "Selection: ")
				(dolist (child *child-selection*)
				  (typecase child
				    (xlib:window (format str "~A " (xlib:wm-name child)))
				    (frame (format str "frame:~A[~A] " (frame-number child)
						   (aif (frame-name child) it "")))))))))
	(dolist (ch child)
	  (when (xlib:window-p ch)
	    (xlib:draw-glyphs *pixmap-buffer* gc 5 (incf pos dy) (ensure-printable (xlib:wm-name ch))))))
      (copy-pixmap-buffer window gc))))


(defun display-all-frame-info (&optional (root *current-root*))
  (with-all-frames (root frame)
    (display-frame-info frame)))





(defun get-parent-layout (child parent)
  (if (frame-p parent)
      (aif (frame-layout parent)
	   (funcall it child parent)
	   (no-layout child parent))
      (get-fullscreen-size)))



(defgeneric adapt-child-to-parent (child parent))

(defmethod adapt-child-to-parent ((window xlib:window) parent)
  (with-xlib-protect
    (if (managed-window-p window parent)
	(multiple-value-bind (nx ny nw nh raise-p)
	    (get-parent-layout window parent)
	  (setf nw (max nw 1)  nh (max nh 1))
	  (let ((change (or (/= (xlib:drawable-x window) nx)
			    (/= (xlib:drawable-y window) ny)
			    (/= (xlib:drawable-width window) nw)
			    (/= (xlib:drawable-height window) nh))))
	    (setf (xlib:drawable-x window) nx
		  (xlib:drawable-y window) ny
		  (xlib:drawable-width window) nw
		  (xlib:drawable-height window) nh)
	    (values raise-p change)))
	(values nil nil))))

(defmethod adapt-child-to-parent ((frame frame) parent)
  (with-xlib-protect
    (multiple-value-bind (nx ny nw nh raise-p)
	(get-parent-layout frame parent)
      (with-slots (rx ry rw rh window) frame
	(setf rx nx  ry ny
	      rw (max nw 1)
	      rh (max nh 1))
	(let ((change (or (/= (xlib:drawable-x window) rx)
			  (/= (xlib:drawable-y window) ry)
			  (/= (xlib:drawable-width window) rw)
			  (/= (xlib:drawable-height window) rh))))
	  (setf (xlib:drawable-x window) rx
		(xlib:drawable-y window) ry
		(xlib:drawable-width window) rw
		(xlib:drawable-height window) rh)
	  (values raise-p change))))))

(defmethod adapt-child-to-parent (child parent)
  (declare (ignore child parent))
  (values nil nil))




(defun raise-if-needed (window raise-p first-p)
  (when (or (eql raise-p t)
	    (and (eql raise-p :first-only) first-p))
    (raise-window window)))

(defgeneric show-child (child parent display-p raise-p first-p))

(defmethod show-child ((frame frame) parent display-p raise-p first-p)
  (declare (ignore parent))
  (with-xlib-protect
    (when display-p
      (with-slots (window) frame
	(when (or *show-root-frame-p* (not (equal frame *current-root*)))
	  (setf (xlib:window-background window) (get-color "Black"))
	  (xlib:map-window window)
	  (raise-if-needed window raise-p first-p))))
    (display-frame-info frame)))


(defmethod show-child ((window xlib:window) parent display-p raise-p first-p)
  (with-xlib-protect
    (if (or (managed-window-p window parent)
	    (equal parent *current-child*))
	(when display-p
	  (xlib:map-window window)
	  (raise-if-needed window raise-p first-p))
	(hide-window window))))

(defmethod show-child (child parent display-p raise-p first-p)
  (declare (ignore child parent display-p raise-p first-p))
  ())


(defgeneric hide-child (child))

(defmethod hide-child ((frame frame))
  (with-xlib-protect
    (with-slots (window) frame
      (xlib:unmap-window window))))

(defmethod hide-child ((window xlib:window))
  (hide-window window))

(defmethod hide-child (child)
  (declare (ignore child))
  ())




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

(defmethod select-child (child selected)
  (declare (ignore child selected))
  ())

(defun select-current-frame (selected)
  (select-child *current-child* selected))

(defun unselect-all-frames ()
  (with-all-children (*current-root* child)
    (select-child child nil)))



(defun set-focus-to-current-child ()
  (labels ((rec (child)
	     (typecase child
	       (xlib:window (focus-window child))
	       (frame (rec (first (frame-child child)))))))
    (no-focus)
    (rec *current-child*)))





(defun show-all-children (&optional (display-child *current-child*))
  "Show all children from *current-root*. Start the effective display
only for display-child and its children"
  (let ((geometry-change nil))
    (labels ((rec (root parent first-p first-parent display-p)
	       (multiple-value-bind (raise-p change)
		   (adapt-child-to-parent root parent)
		 (when change (setf geometry-change change))
		 (show-child root parent display-p raise-p first-p))
	       (select-child root (if (equal root *current-child*) t
				      (if (and first-p first-parent) :maybe nil)))
	       (when (frame-p root)
		 (let ((first-child (first (frame-child root))))
		   (dolist (child (reverse (frame-child root)))
		     (rec child root (equal child first-child) (and first-p first-parent)
			  (or display-p (equal root display-child))))))))
      (rec *current-root* nil t t (equal display-child *current-root*))
      (set-focus-to-current-child)
      geometry-change)))



(defun hide-all-children (root)
  "Hide all root children"
  (when (frame-p root)
    (dolist (child (frame-child root))
      (hide-all child))))

(defun hide-all (root)
  "Hide root and all its children"
  (hide-child root)
  (hide-all-children root))






(defun focus-child (child parent)
  "Focus child - Return true if something has change"
  (when (and (frame-p parent)
	     (member child (frame-child parent)))
    (when (not (equal child (first (frame-child parent))))
      (loop until (equal child (first (frame-child parent)))
	 do (setf (frame-child parent) (rotate-list (frame-child parent))))
      t)))

(defun focus-child-rec (child parent)
  "Focus child and its parents - Return true if something has change"
  (let ((change nil))
    (labels ((rec (child parent)
	       (when (focus-child child parent)
		 (setf change t))
	       (when parent
		 (rec parent (find-parent-frame parent)))))
      (rec child parent))
    change))


(defun set-current-child-generic (child)
  (unless (equal *current-child* child)
    (setf *current-child* child)
    t))

(defgeneric set-current-child (child parent window-parent))

(defmethod set-current-child ((child xlib:window) parent window-parent)
  (set-current-child-generic (if window-parent parent child)))

(defmethod set-current-child ((child frame) parent window-parent)
  (declare (ignore parent window-parent))
  (set-current-child-generic child))

(defmethod set-current-child (child parent window-parent)
  (declare (ignore child parent window-parent))
  ())


(defun set-current-root (parent)
  "Set current root if parent is not in current root"
  (unless (find-child parent *current-root*)
    (setf *current-root* parent)))


(defun focus-all-children (child parent &optional (window-parent t))
  "Focus child and its parents -
For window: set current child to window or its parent according to window-parent"
  (let ((new-focus (focus-child-rec child parent))
	(new-current-child (set-current-child child parent window-parent))
	(new-root (set-current-root parent)))
    (or new-focus new-current-child new-root)))






(defun select-next/previous-sister (fun-rotate)
  "Select the next/previous sister frame"
  (let ((frame-is-root? (and (equal *current-root* *current-child*)
			     (not (equal *current-root* *root-frame*)))))
    (if frame-is-root?
	(hide-all *current-root*)
	(select-current-frame nil))
    (let ((parent (find-parent-frame *current-child*)))
      (when (frame-p parent)
	(with-slots (child) parent
	  (setf child (funcall fun-rotate child))
	  (setf *current-child* (first child)))))
    (when frame-is-root?
      (setf *current-root* *current-child*))
    (show-all-children *current-root*)))


(defun select-next-sister ()
  "Select the next sister frame"
  (select-next/previous-sister #'anti-rotate-list))

(defun select-previous-sister ()
  "Select the previous sister frame"
  (select-next/previous-sister #'rotate-list))


(defun select-next-level ()
  "Select the next level in frame"
  (select-current-frame :maybe)
  (when (frame-p *current-child*)
    (awhen (first (frame-child *current-child*))
      (setf *current-child* it)))
  (show-all-children))

(defun select-previous-level ()
  "Select the previous level in frame"
  (unless (equal *current-child* *current-root*)
    (select-current-frame :maybe)
    (awhen (find-parent-frame *current-child*)
      (setf *current-child* it))
    (show-all-children)))



(defun select-next/previous-child (fun-rotate)
  "Select the next/previous child"
  (when (frame-p *current-child*)
    (unselect-all-frames)
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
  (show-all-children *current-root*))

(defun leave-frame ()
  "Leave the selected frame - ie make its parent the root frame"
  (hide-all *current-root*)
  (awhen (find-parent-frame *current-root*)
    (when (frame-p it)
      (setf *current-root* it)))
  (show-all-children *current-root*))


(defun switch-to-root-frame (&key (show-later nil))
  "Switch to the root frame"
  (hide-all *current-root*)
  (setf *current-root* *root-frame*)
  (unless show-later
    (show-all-children *current-root*)))

(defun switch-and-select-root-frame (&key (show-later nil))
  "Switch and select the root frame"
  (hide-all *current-root*)
  (setf *current-root* *root-frame*)
  (setf *current-child* *current-root*)
  (unless show-later
    (show-all-children *current-root*)))


(defun toggle-show-root-frame ()
  "Show/Hide the root frame"
  (hide-all *current-root*)
  (setf *show-root-frame-p* (not *show-root-frame-p*))
  (show-all-children *current-root*))


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
    (setf *current-root* (find-parent-frame child)))
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
      (setf (xlib:drawable-width window) (min (max min-width rwidth *default-window-width*) max-width)
	    (xlib:drawable-height window) (min (max min-height rheight *default-window-height*) max-height))
      (setf (xlib:drawable-x window) (truncate (/ (- (xlib:screen-width *screen*) (xlib:drawable-width window)) 2))
	    (xlib:drawable-y window) (truncate (/ (- (xlib:screen-height *screen*) (xlib:drawable-height window)) 2))))))



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
    (unless (do-all-frames-nw-hook window)
      (call-hook *default-nw-hook* (list *root-frame* window)))
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
	      (format t "Processing ~S: type=~A ~S~%" (xlib:wm-name win) (window-type win) win)
	      (unhide-window win)
	      (process-new-window win)
	      (xlib:map-window win)
	      (raise-window win)
	      (pushnew (xlib:window-id win) id-list))))))
    (netwm-set-client-list id-list)))
