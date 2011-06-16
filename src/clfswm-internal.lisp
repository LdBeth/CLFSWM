;; --------------------------------------------------------------------------
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
  (/ (- x (frame-rx parent) *border-size*) (frame-rw parent)))

(defun y-px->fl (y parent)
  "Convert pixel Y coordinate to float"
  (/ (- y (frame-ry parent) *border-size*) (frame-rh parent)))

(defun w-px->fl (w parent)
  "Convert pixel Width coordinate to float"
  (/ w (frame-rw parent)))

(defun h-px->fl (h parent)
  "Convert pixel Height coordinate to float"
  (/ h (frame-rh parent)))



(defun rect-hidden-p (rect1 rect2)
  "Return T if child-rect1 hide child-rect2"
  (and (<= (child-rect-x rect1) (child-rect-x rect2))
       (<= (child-rect-y rect1) (child-rect-y rect2))
       (>= (+ (child-rect-x rect1) (child-rect-w rect1)) (+ (child-rect-x rect2) (child-rect-w rect2)))
       (>= (+ (child-rect-y rect1) (child-rect-h rect1)) (+ (child-rect-y rect2) (child-rect-h rect2)))))



(defgeneric frame-p (frame))
(defmethod frame-p ((frame frame))
  (declare (ignore frame))
  t)
(defmethod frame-p (frame)
  (declare (ignore frame))
  nil)



;;; in-*: Find if point (x,y) is in frame, window or child
(defun in-frame (frame x y)
  (and (frame-p frame)
       (<= (frame-rx frame) x (+ (frame-rx frame) (frame-rw frame)))
       (<= (frame-ry frame) y (+ (frame-ry frame) (frame-rh frame)))))

(defun in-window (window x y)
  (and (xlib:window-p window)
       (<= (xlib:drawable-x window) x (+ (xlib:drawable-x window) (xlib:drawable-width window)))
       (<= (xlib:drawable-y window) y (+ (xlib:drawable-y window) (xlib:drawable-height window)))))

(defgeneric in-child (child x y))

(defmethod in-child ((child frame) x y)
  (in-frame child x y))
(defmethod in-child ((child xlib:window) x y)
  (in-window child x y))
(defmethod in-child (child x y)
  (declare (ignore child x y))
  nil)




(defun frame-selected-child (frame)
  (when (frame-p frame)
    (with-slots (child selected-pos) frame
      (let ((len (length child)))
	(cond ((minusp selected-pos) (setf selected-pos 0))
	      ((>= selected-pos len) (setf selected-pos (max (1- len) 0)))))
      (nth selected-pos child))))





(defgeneric child-equal-p (child-1 child-2))

(defmethod child-equal-p ((child-1 xlib:window) (child-2 xlib:window))
  (xlib:window-equal child-1 child-2))

(defmethod child-equal-p ((child-1 frame) (child-2 frame))
  (equal child-1 child-2))

(defmethod child-equal-p (child-1 child-2)
  (declare (ignore child-1 child-2))
  nil)


(declaim (inline child-member child-remove child-position))

(defun child-member (child list)
  (member child list :test #'child-equal-p))

(defun child-remove (child list)
  (remove child list :test #'child-equal-p))

(defun child-position (child list)
  (position child list :test #'child-equal-p))



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


(defun remove-frame-data-slot (frame slot)
  "Remove a slot in frame data slots"
  (when (frame-p frame)
    (with-slots (data) frame
      (setf data (remove (assoc slot data) data)))))



(defun managed-window-p (window frame)
  "Return t only if window is managed by frame"
  (if (frame-p frame)
      (with-slots ((managed forced-managed-window)
		   (unmanaged forced-unmanaged-window)) frame
	(and (xlib:window-p window)
	     (not (child-member window unmanaged))
	     (not (member (xlib:wm-name window) unmanaged :test #'string-equal-p))
	     (or (member :all (frame-managed-type frame))
		 (member (window-type window) (frame-managed-type frame))
		 (child-member window managed)
		 (member (xlib:wm-name window) managed :test #'string-equal-p))))
      t))


(defun never-managed-window-p (window)
  (when (xlib:window-p window)
    (dolist (type *never-managed-window-list*)
      (when (funcall (first type) window)
	(return (values t (second type)))))))



(defgeneric child-name (child))

(defmethod child-name ((child xlib:window))
  (xlib:wm-name child))

(defmethod child-name ((child frame))
  (frame-name child))

(defmethod child-name (child)
  (declare (ignore child))
  "???")


(defgeneric set-child-name (child name))

(defmethod set-child-name ((child xlib:window) name)
  (setf (xlib:wm-name child) name))

(defmethod set-child-name ((child frame) name)
  (setf (frame-name child) name))

(defmethod set-child-name (child name)
  (declare (ignore child name)))

(defsetf child-name set-child-name)




(defgeneric child-fullname (child))

(defmethod child-fullname ((child xlib:window))
  (format nil "~A (~A)" (or (xlib:wm-name child) "?") (or (xlib:get-wm-class child) "?")))

(defmethod child-fullname ((child frame))
  (aif (frame-name child)
       (format nil "~A (Frame ~A)" it (frame-number child))
       (format nil "Frame ~A" (frame-number child))))

(defmethod child-fullname (child)
  (declare (ignore child))
  "???")


(defgeneric child-x (child))
(defmethod child-x ((child xlib:window))
  (xlib:drawable-x child))
(defmethod child-x ((child frame))
  (frame-rx child))

(defgeneric child-y (child))
(defmethod child-y ((child xlib:window))
  (xlib:drawable-y child))
(defmethod child-y ((child frame))
  (frame-ry child))

(defgeneric child-width (child))
(defmethod child-width ((child xlib:window))
  (xlib:drawable-width child))
(defmethod child-width ((child frame))
  (frame-rw child))

(defgeneric child-height (child))
(defmethod child-height ((child xlib:window))
  (xlib:drawable-height child))
(defmethod child-height ((child frame))
  (frame-rh child))

(defgeneric child-x2 (child))
(defmethod child-x2 ((child xlib:window))
  (+ (xlib:drawable-x child) (xlib:drawable-width child)))
(defmethod child-x2 ((child frame))
  (+ (frame-rx child) (frame-rw child)))

(defgeneric child-y2 (child))
(defmethod child-y2 ((child xlib:window))
  (+ (xlib:drawable-y child) (xlib:drawable-height child)))
(defmethod child-y2 ((child frame))
  (+ (frame-ry child) (frame-rh child)))



(defgeneric child-center (child))

(defmethod child-center ((child xlib:window))
  (values (+ (xlib:drawable-x child) (/ (xlib:drawable-width child) 2))
          (+ (xlib:drawable-y child) (/ (xlib:drawable-height child) 2))))

(defmethod child-center ((child frame))
  (values (+ (frame-rx child) (/ (frame-rw child) 2))
          (+ (frame-ry child) (/ (frame-rh child) 2))))

(defun child-distance (child1 child2)
  (multiple-value-bind (x1 y1) (child-center child1)
    (multiple-value-bind (x2 y2) (child-center child2)
      (values (+ (abs (- x2 x1)) (abs (- y2 y1)))
              (- x2 x1)
              (- y2 y1)))))

(defun middle-child-x (child)
  (+ (child-x child) (/ (child-width child) 2)))

(defun middle-child-y (child)
  (+ (child-y child) (/ (child-height child) 2)))

(declaim (inline adj-border-xy adj-border-wh))
(defgeneric adj-border-xy (value child))
(defgeneric adj-border-wh (value child))

(defmethod adj-border-xy (v (child xlib:window))
  (+ v (xlib:drawable-border-width child)))

(defmethod adj-border-xy (v (child frame))
  (+ v (xlib:drawable-border-width (frame-window child))))

(defmethod adj-border-wh (v (child xlib:window))
  (- v (* (xlib:drawable-border-width child) 2)))

(defmethod adj-border-wh (v (child frame))
  (- v (* (xlib:drawable-border-width (frame-window child)) 2)))


(declaim (inline anti-adj-border-xy anti-adj-border-wh))
(defgeneric anti-adj-border-xy (value child))
(defgeneric anti-adj-border-wh (value child))

(defmethod anti-adj-border-xy (v (child xlib:window))
  (- v (xlib:drawable-border-width child)))

(defmethod anti-adj-border-xy (v (child frame))
  (- v (xlib:drawable-border-width (frame-window child))))

(defmethod anti-adj-border-wh (v (child xlib:window))
  (+ v (* (xlib:drawable-border-width child) 2)))

(defmethod anti-adj-border-wh (v (child frame))
  (+ v (* (xlib:drawable-border-width (frame-window child)) 2)))




(defmacro with-focus-window ((window) &body body)
  `(let ((,window (xlib:input-focus *display*)))
     (when (and ,window (not (xlib:window-equal ,window *no-focus-window*)))
       ,@body)))


(defgeneric rename-child (child name))

(defmethod rename-child ((child frame) name)
  (setf (frame-name child) name)
  (display-frame-info child))

(defmethod rename-child ((child xlib:window) name)
  (setf (xlib:wm-name child) name))

(defmethod rename-child (child name)
  (declare (ignore child name)))


(defun is-in-current-child-p (child)
  (and (frame-p *current-child*)
       (child-member child (frame-child *current-child*))))



;; (with-all-children (*root-frame* child) (typecase child (xlib:window (print child)) (frame (print (frame-number child)))))
(defmacro with-all-children ((root child) &body body)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(block nil
       (labels ((,rec (,child)
		  ,@body
		  (when (frame-p ,child)
		    (dolist (,sub-child (reverse (frame-child ,child)))
		      (,rec ,sub-child)))))
	 (,rec ,root)))))


;; (with-all-children (*root-frame* child) (typecase child (xlib:window (print child)) (frame (print (frame-number child)))))
(defmacro with-all-children-reversed ((root child) &body body)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(block nil
       (labels ((,rec (,child)
		  ,@body
		  (when (frame-p ,child)
		    (dolist (,sub-child (frame-child ,child))
		      (,rec ,sub-child)))))
	 (,rec ,root)))))





;; (with-all-frames (*root-frame* frame) (print (frame-number frame)))
(defmacro with-all-frames ((root frame) &body body)
  (let ((rec (gensym))
	(child (gensym)))
    `(block nil
       (labels ((,rec (,frame)
		  (when (frame-p ,frame)
		    ,@body
		    (dolist (,child (reverse (frame-child ,frame)))
		      (,rec ,child)))))
	 (,rec ,root)))))


;; (with-all-windows (*root-frame* window) (print window))
(defmacro with-all-windows ((root window) &body body)
  (let ((rec (gensym))
	(child (gensym)))
    `(block nil
       (labels ((,rec (,window)
		  (when (xlib:window-p ,window)
		    ,@body)
		  (when (frame-p ,window)
		    (dolist (,child (reverse (frame-child ,window)))
		      (,rec ,child)))))
	 (,rec ,root)))))



;; (with-all-frames-windows (*root-frame* child) (print child) (print (frame-number child)))
(defmacro with-all-windows-frames ((root child) body-window body-frame)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(block nil
       (labels ((,rec (,child)
		  (typecase ,child
		    (xlib:window ,body-window)
		    (frame ,body-frame
			   (dolist (,sub-child (reverse (frame-child ,child)))
			     (,rec ,sub-child))))))
	 (,rec ,root)))))

(defmacro with-all-windows-frames-and-parent ((root child parent) body-window body-frame)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(block nil
       (labels ((,rec (,child ,parent)
		  (typecase ,child
		    (xlib:window ,body-window)
		    (frame ,body-frame
			   (dolist (,sub-child (reverse (frame-child ,child)))
			     (,rec ,sub-child ,child))))))
	 (,rec ,root nil)))))



(defun create-frame-window ()
  (xlib:create-window :parent *root*
                      :x 0
                      :y 0
                      :width 200
                      :height 200
                      :background (get-color *frame-background*)
                      :colormap (xlib:screen-default-colormap *screen*)
                      :border-width *border-size*
                      :border (get-color *color-selected*)
                      :event-mask '(:exposure :button-press :button-release :pointer-motion :enter-window)))

(defun create-frame-gc (window)
  (xlib:create-gcontext :drawable window
                        :foreground (get-color *frame-foreground*)
                        :background (get-color *frame-background*)
                        :font *default-font*
                        :line-style :solid))


(defun destroy-all-frames-window ()
  (with-all-frames (*root-frame* frame)
    (when (frame-gc frame)
      (xlib:free-gcontext (frame-gc frame))
      (setf (frame-gc frame) nil))
    (when (frame-window frame)
      (xlib:destroy-window (frame-window frame))
      (setf (frame-window frame) nil))))

(defun create-all-frames-window ()
  (with-all-frames (*root-frame* frame)
    (unless (frame-window frame)
      (setf (frame-window frame) (create-frame-window)))
    (unless (frame-gc frame)
      (setf (frame-gc frame) (create-frame-gc (frame-window frame)))))
  (with-all-frames (*root-frame* frame)
    (dolist (child (frame-child frame))
      (handler-case
          (dbg (child-fullname child))
        (error (c)
          (setf (frame-child frame) (remove child (frame-child frame) :test #'child-equal-p))
          (dbg c child))))))




(defun frame-find-free-number ()
  (let ((all-numbers nil))
    (with-all-frames (*root-frame* frame)
      (pushnew (frame-number frame) all-numbers))
    (find-free-number all-numbers)))


(defun create-frame (&rest args &key (number (frame-find-free-number)) &allow-other-keys)
  (let* ((window (create-frame-window))
	 (gc (create-frame-gc window)))
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
	    h (h-px->fl prh parent))
      (xlib:display-finish-output *display*))))

(defun fixe-real-size (frame parent)
  "Fixe real (pixel) coordinates in float coordinates"
  (when (frame-p frame)
    (with-slots (x y w h rx ry rw rh) frame
      (setf x (x-px->fl rx parent)
	    y (y-px->fl ry parent)
	    w (w-px->fl (anti-adj-border-wh rw parent) parent)
	    h (h-px->fl (anti-adj-border-wh rh parent) parent)))))

(defun fixe-real-size-current-child ()
  "Fixe real (pixel) coordinates in float coordinates for children in the current child"
  (when (frame-p *current-child*)
    (dolist (child (frame-child *current-child*))
      (fixe-real-size child *current-child*))))




(defun find-child (to-find root)
  "Find to-find in root or in its children"
  (with-all-children (root child)
    (when (child-equal-p child to-find)
      (return-from find-child t))))



(defmacro with-find-in-all-frames (test &optional return-value)
  `(let (ret)
     (block return-block
       (with-all-frames (root frame)
	 (when ,test
	   (if first-foundp
	       (return-from return-block (or ,return-value frame))
	       (setf ret frame))))
       (or ,return-value ret))))

(defun find-parent-frame  (to-find &optional (root *root-frame*) first-foundp)
  "Return the parent frame of to-find"
  (with-find-in-all-frames
      (child-member to-find (frame-child frame))))

(defun find-frame-window (window &optional (root *root-frame*) first-foundp)
  "Return the frame with the window window"
  (with-find-in-all-frames
      (xlib:window-equal window (frame-window frame))))

(defun find-frame-by-name (name &optional (root *root-frame*) first-foundp)
  "Find a frame from its name"
  (when name
    (with-find-in-all-frames
	(string-equal name (frame-name frame)))))

(defun find-frame-by-number (number &optional (root *root-frame*) first-foundp)
  "Find a frame from its number"
  (when (numberp number)
    (with-find-in-all-frames
	(= number (frame-number frame)))))


(defun find-child-in-parent (child base)
  "Return t if child is in base or in its parents"
  (labels ((rec (base)
	     (when (child-equal-p child base)
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



;;; Current window utilities
(defun get-current-window ()
  (typecase *current-child*
    (xlib:window  *current-child*)
    (frame (frame-selected-child *current-child*))))

(defmacro with-current-window (&body body)
  "Bind 'window' to the current window"
  `(let ((window (get-current-window)))
      (when (xlib:window-p window)
	,@body)))

(defun get-first-window ()
  (typecase *current-child*
    (xlib:window  *current-child*)
    (frame (or (first (frame-child *current-child*))
               *current-child*))))





(defun display-frame-info (frame)
  (when (frame-p frame)
    (let ((dy (+ (xlib:max-char-ascent *default-font*) (xlib:max-char-descent *default-font*))))
      (with-slots (name number gc window child hidden-children) frame
        (setf (xlib:gcontext-background gc) (get-color *frame-background*)
              (xlib:window-background window) (get-color *frame-background*))
        (clear-pixmap-buffer window gc)
        (setf (xlib:gcontext-foreground gc) (get-color (if (and (child-equal-p frame *current-root*)
                                                                (child-equal-p frame *current-child*))
                                                           *frame-foreground-root* *frame-foreground*)))
        (xlib:draw-glyphs *pixmap-buffer* gc 5 dy
                          (format nil "Frame: ~A~A"
                                  number
                                  (if name  (format nil " - ~A" name) "")))
        (let ((pos dy))
          (when (child-equal-p frame *current-root*)
            (when *child-selection*
              (xlib:draw-glyphs *pixmap-buffer* gc 5 (incf pos dy)
                                (with-output-to-string (str)
                                  (format str "  Selection: ")
                                  (dolist (child *child-selection*)
                                    (typecase child
                                      (xlib:window (format str "  ~A " (xlib:wm-name child)))
                                      (frame (format str "  frame:~A[~A] " (frame-number child)
                                                     (aif (frame-name child) it "")))))))))
          (dolist (ch child)
            (xlib:draw-glyphs *pixmap-buffer* gc 5 (incf pos dy)
                              (format nil "  ~A" (ensure-printable (child-fullname ch))))))
        (copy-pixmap-buffer window gc)
        (values t t)))))


(defun display-all-frame-info (&optional (root *current-root*))
  (with-all-frames (root frame)
    (display-frame-info frame)))





(defun get-parent-layout (child parent)
  (if (child-equal-p child *current-root*)
      (get-fullscreen-size)
      (if (or (frame-p child) (managed-window-p child parent))
          (if (frame-p parent)
              (aif (frame-layout parent)
                   (funcall it child parent)
                   (no-layout child parent))
              (get-fullscreen-size))
          (values (xlib:drawable-x child) (xlib:drawable-y child)
                  (xlib:drawable-width child) (xlib:drawable-height child)))))





(defgeneric adapt-child-to-parent (child parent))

(defmethod adapt-child-to-parent ((window xlib:window) parent)
  (when (managed-window-p window parent)
    (multiple-value-bind (nx ny nw nh)
	(get-parent-layout window parent)
      (setf nw (max nw 1)  nh (max nh 1))
      (let ((change (or (/= (xlib:drawable-x window) nx)
			(/= (xlib:drawable-y window) ny)
			(/= (xlib:drawable-width window) nw)
			(/= (xlib:drawable-height window) nh))))
        (when change
          (setf (xlib:drawable-x window) nx
                (xlib:drawable-y window) ny
                (xlib:drawable-width window) nw
                (xlib:drawable-height window) nh))
	change))))


(defmethod adapt-child-to-parent ((frame frame) parent)
  (declare (ignore parent))
    (with-slots (rx ry rw rh window) frame
      (let ((change (or (/= (xlib:drawable-x window) rx)
			(/= (xlib:drawable-y window) ry)
			(/= (xlib:drawable-width window) rw)
			(/= (xlib:drawable-height window) rh))))
        (when change
          (setf (xlib:drawable-x window) rx
                (xlib:drawable-y window) ry
                (xlib:drawable-width window) rw
                (xlib:drawable-height window) rh))
	change)))

(defmethod adapt-child-to-parent (child parent)
  (declare (ignore child parent))
  nil)


(defgeneric set-child-stack-order (window child)
  (:documentation "Raise window if child is NIL else put window just below child"))

(defmethod set-child-stack-order (window (child xlib:window))
  (lower-window window child))

(defmethod set-child-stack-order (window (child frame))
  (lower-window window (frame-window child)))

(defmethod set-child-stack-order (window child)
  (declare (ignore child))
  (raise-window window))



(defgeneric show-child (child parent previous))

(defmethod show-child ((frame frame) parent previous)
  (declare (ignore parent))
  (with-slots (window show-window-p) frame
    (if (and show-window-p
             (or *show-root-frame-p* (not (child-equal-p frame *current-root*))))
        (progn
          (map-window window)
          (set-child-stack-order window previous)
	  (display-frame-info frame))
	(hide-window window))))



(defun hide-unmanaged-window-p (parent)
  (let ((action (frame-data-slot parent :unmanaged-window-action)))
    (case action
      (:hide t)
      (:show nil)
      (t *hide-unmanaged-window*))))


(defmethod show-child ((window xlib:window) parent previous)
  (if (or (managed-window-p window parent)
	  (child-equal-p window *current-child*)
	  (not (hide-unmanaged-window-p parent))
	  (child-equal-p parent *current-child*))
      (progn
	(map-window window)
	(set-child-stack-order window previous))
      (hide-window window)))

(defmethod show-child (child parent raise-p)
  (declare (ignore child parent raise-p))
  ())


(defgeneric hide-child (child))

(defmethod hide-child ((frame frame))
  (with-slots (window) frame
    (xlib:unmap-window window)))

(defmethod hide-child ((window xlib:window))
  (hide-window window))

(defmethod hide-child (child)
  (declare (ignore child))
  ())


(defgeneric select-child (child selected))

(labels ((get-selected-color (child selected-p)
           (get-color (cond ((child-equal-p child *current-child*) *color-selected*)
                            (selected-p *color-maybe-selected*)
                            (t *color-unselected*)))))
  (defmethod select-child ((frame frame) selected-p)
    (when (and (frame-p frame) (frame-window frame))
      (setf (xlib:window-border (frame-window frame))
            (get-selected-color frame selected-p))))

  (defmethod select-child ((window xlib:window) selected-p)
    (setf (xlib:window-border window)
          (get-selected-color window selected-p)))

  (defmethod select-child (child selected)
    (declare (ignore child selected))
    ()))

(defun select-current-frame (selected)
  (select-child *current-child* selected))

(defun unselect-all-frames ()
  (with-all-children (*current-root* child)
    (select-child child nil)))



(defun set-focus-to-current-child ()
  (labels ((rec (child)
	     (typecase child
	       (xlib:window (focus-window child))
	       (frame (rec (frame-selected-child child))))))
    (no-focus)
    (rec *current-child*)))




(defun adapt-frame-to-parent (frame parent)
  (multiple-value-bind (nx ny nw nh)
      (get-parent-layout frame parent)
    (with-slots (rx ry rw rh window) frame
      (setf rx nx  ry ny
	    rw (max nw 1)
	    rh (max nh 1)))))


(defun adapt-child-to-rect (rect)
  (let ((window (typecase (child-rect-child rect)
                  (xlib:window (when (managed-window-p (child-rect-child rect) (child-rect-parent rect))
                                 (child-rect-child rect)))
                  (frame (frame-window (child-rect-child rect))))))
    (when window
      (let ((change (or (/= (xlib:drawable-x window) (child-rect-x rect))
			(/= (xlib:drawable-y window) (child-rect-y rect))
			(/= (xlib:drawable-width window) (child-rect-w rect))
			(/= (xlib:drawable-height window) (child-rect-h rect)))))
        (when change
          (setf (xlib:drawable-x window) (child-rect-x rect)
                (xlib:drawable-y window) (child-rect-y rect)
                (xlib:drawable-width window) (child-rect-w rect)
                (xlib:drawable-height window) (child-rect-h rect)))
	change))))




(defun show-all-children (&optional (from-root-frame nil))
  "Show all children from *current-root*. When from-root-frame is true
Display all children from root frame and hide those not in *current-root*"
  (let ((geometry-change nil)
        (displayed-child nil)
        (hidden-child nil))
    (labels ((in-displayed-list (child)
               (member child displayed-child :test (lambda (c rect)
                                                     (child-equal-p c (child-rect-child rect)))))

             (add-in-hidden-list (child)
               (pushnew child hidden-child :test #'child-equal-p))

             (set-geometry (child parent in-current-root child-current-root-p)
               (if (or in-current-root child-current-root-p)
                   (when (frame-p child)
                     (adapt-frame-to-parent child (if child-current-root-p nil parent)))
                   (add-in-hidden-list child)))

             (recurse-on-frame-child (child in-current-root child-current-root-p selected-p)
               (let ((selected-child (frame-selected-child child)))
                 (dolist (sub-child (frame-child child))
                   (rec sub-child child
                        (and selected-p (child-equal-p sub-child selected-child))
                        (or in-current-root child-current-root-p)))))

             (hidden-child-p (rect)
               (dolist (r displayed-child)
                 (when (rect-hidden-p r rect)
                   (return t))))

             (select-and-display (child parent selected-p)
               (multiple-value-bind (nx ny nw nh)
                   (get-parent-layout child parent)
                 (let ((rect (make-child-rect :child child :parent parent
                                              :selected-p selected-p
                                              :x nx :y ny :w nw :h nh)))
                   (if (hidden-child-p rect)
                       (add-in-hidden-list child)
                       (push rect displayed-child)))))

             (display-displayed-child ()
               (let ((previous nil))
                 (dolist (rect (nreverse displayed-child))
                   (when (adapt-child-to-rect rect)
                     (setf geometry-change t))
                   (select-child (child-rect-child rect) (child-rect-selected-p rect))
                   (show-child (child-rect-child rect)
                               (child-rect-parent rect)
                               previous)
                   (setf previous (child-rect-child rect)))))

             (rec (child parent selected-p in-current-root)
               (let ((child-current-root-p (child-equal-p child *current-root*)))
                 (unless (in-displayed-list child)
                   (set-geometry child parent in-current-root child-current-root-p))
                 (when (frame-p child)
                   (recurse-on-frame-child child in-current-root child-current-root-p selected-p))
                 (when (and (or in-current-root child-current-root-p)
                            (not (in-displayed-list child)))
                   (select-and-display child parent selected-p)))))

      (rec (if from-root-frame *root-frame* *current-root*)
           nil t (child-equal-p *current-root* *root-frame*))
      (display-displayed-child)
      (dolist (child hidden-child)
        (hide-child child))
      (set-focus-to-current-child)
      (xlib:display-finish-output *display*)
      geometry-change)))






(defun hide-all-children (root &optional except)
  "Hide all root children"
  (when (and (frame-p root) (not (child-equal-p root except)))
    (dolist (child (frame-child root))
      (hide-all child except))))

(defun hide-all (root &optional except)
  "Hide root and all its children"
  (unless (child-equal-p root except)
    (hide-child root))
  (hide-all-children root except))





(defun focus-child (child parent)
  "Focus child - Return true if something has change"
  (when (and (frame-p parent)
	     (child-member child (frame-child parent)))
    (when (not (child-equal-p child (frame-selected-child parent)))
      (with-slots ((parent-child child) selected-pos) parent
	(setf parent-child (nth-insert selected-pos child (child-remove child parent-child))))
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
  (unless (child-equal-p *current-child* child)
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


(defun set-current-root (child parent window-parent)
  "Set current root if parent is not in current root"
  (when (and window-parent
             (not (child-equal-p child *current-root*))
             (not (find-child parent *current-root*)))
    (setf *current-root* parent)
    t))


(defun focus-all-children (child parent &optional (window-parent t))
  "Focus child and its parents -
For window: set current child to window or its parent according to window-parent"
  (let ((new-focus (focus-child-rec child parent))
	(new-current-child (set-current-child child parent window-parent))
	(new-root (set-current-root child parent window-parent)))
    (or new-focus new-current-child new-root)))




(defun select-next-level ()
  "Select the next level in frame"
  (select-current-frame :maybe)
  (when (frame-p *current-child*)
    (awhen (frame-selected-child *current-child*)
      (setf *current-child* it)))
  (show-all-children))

(defun select-previous-level ()
  "Select the previous level in frame"
  (unless (child-equal-p *current-child* *current-root*)
    (select-current-frame :maybe)
    (awhen (find-parent-frame *current-child*)
      (setf *current-child* it))
    (show-all-children)))



(defun enter-frame ()
  "Enter in the selected frame - ie make it the root frame"
  (setf *current-root* *current-child*)
  (show-all-children t))

(defun leave-frame ()
  "Leave the selected frame - ie make its parent the root frame"
  (unless (child-equal-p *current-root* *root-frame*)
    (hide-all *current-root* (get-first-window))
    (awhen (find-parent-frame *current-root*)
           (when (frame-p it)
             (setf *current-root* it)))
    (show-all-children)))


;;; Other actions (select-next-child, select-next-brother...) are in
;;; clfswm-circulate-mode.lisp



(defun frame-lower-child ()
  "Lower the child in the current frame"
  (when (frame-p *current-child*)
    (with-slots (child selected-pos) *current-child*
      (unless (>= selected-pos (length child))
	(when (nth (1+ selected-pos) child)
	  (rotatef (nth selected-pos child)
		   (nth (1+ selected-pos) child)))
	(incf selected-pos)))
    (show-all-children)))


(defun frame-raise-child ()
  "Raise the child in the current frame"
  (when (frame-p *current-child*)
    (with-slots (child selected-pos) *current-child*
      (unless (< selected-pos 1)
	(when (nth (1- selected-pos) child)
	  (rotatef (nth selected-pos child)
		   (nth (1- selected-pos) child)))
	(decf selected-pos)))
    (show-all-children)))


(defun frame-select-next-child ()
  "Select the next child in the current frame"
  (when (frame-p *current-child*)
    (with-slots (child selected-pos) *current-child*
      (unless (>= selected-pos (length child))
	(incf selected-pos)))
    (show-all-children)))


(defun frame-select-previous-child ()
  "Select the previous child in the current frame"
  (when (frame-p *current-child*)
    (with-slots (child selected-pos) *current-child*
      (unless (< selected-pos 1)
	(decf selected-pos)))
    (show-all-children)))



(defun switch-to-root-frame (&key (show-later nil))
  "Switch to the root frame"
  (setf *current-root* *root-frame*)
  (unless show-later
    (show-all-children t)))

(defun switch-and-select-root-frame (&key (show-later nil))
  "Switch and select the root frame"
  (setf *current-root* *root-frame*)
  (setf *current-child* *current-root*)
  (unless show-later
    (show-all-children t)))


(defun toggle-show-root-frame ()
  "Show/Hide the root frame"
  (setf *show-root-frame-p* (not *show-root-frame-p*))
  (show-all-children))



(defun prevent-current-*-equal-child (child)
  " Prevent current-root and current-child equal to child"
  (when (child-equal-p child *current-root*)
    (setf *current-root* (find-parent-frame child)))
  (when (child-equal-p child *current-child*)
    (setf *current-child* *current-root*)))


(defun remove-child-in-frame (child frame)
  "Remove the child in frame"
  (when (frame-p frame)
    (setf (frame-child frame) (child-remove child (frame-child frame)))))

(defun remove-child-in-frames (child root)
  "Remove child in the frame root and in all its children"
  (with-all-frames (root frame)
    (remove-child-in-frame child frame)))


(defun remove-child-in-all-frames (child)
  "Remove child in all frames from *root-frame*"
  (prevent-current-*-equal-child child)
  (remove-child-in-frames child *root-frame*))


(defun delete-child-in-frames (child root)
  "Delete child in the frame root and in all its children
Warning:frame window and gc are freeed."
  (with-all-frames (root frame)
    (remove-child-in-frame child frame)
    (unless (find-frame-window (frame-window frame))
      (awhen (frame-gc frame) (xlib:free-gcontext it) (setf it nil))
      (awhen (frame-window frame) (xlib:destroy-window it) (setf it nil))))
  (when (xlib:window-p child)
    (netwm-remove-in-client-list child)))


(defun delete-child-in-all-frames (child)
  "Delete child in all frames from *root-frame*"
  (prevent-current-*-equal-child child)
  (delete-child-in-frames child *root-frame*))

(defun delete-child-and-children-in-frames (child root)
  "Delete child and its children in the frame root and in all its children
Warning:frame window and gc are freeed."
  (when (and (frame-p child) (frame-child child))
    (dolist (ch (frame-child child))
      (delete-child-and-children-in-frames ch root)))
  (delete-child-in-frames child root))

(defun delete-child-and-children-in-all-frames (child &optional (close-methode 'delete-window))
  "Delete child and its children in all frames from *root-frame*"
  (prevent-current-*-equal-child child)
  (delete-child-and-children-in-frames child *root-frame*)
  (when (xlib:window-p child)
    (funcall close-methode child))
  (show-all-children))


(defun clean-windows-in-all-frames ()
  "Remove all xlib:windows present in *root-frame* and not in the xlib tree"
  (let ((x-tree (xlib:query-tree *root*)))
    (with-all-frames (*root-frame* frame)
      (dolist (child (frame-child frame))
        (when (xlib:window-p child)
          (unless (member child x-tree :test #'xlib:window-equal)
            (prevent-current-*-equal-child child)
            (setf (frame-child frame)
                  (child-remove child (frame-child frame)))))))))





(defun place-window-from-hints (window)
  "Place a window from its hints"
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
    (setf (xlib:drawable-x window) (truncate (/ (- (xlib:screen-width *screen*) (+ (xlib:drawable-width window) 2)) 2))
	  (xlib:drawable-y window) (truncate (/ (- (xlib:screen-height *screen*) (+ (xlib:drawable-height window) 2)) 2)))
    (xlib:display-finish-output *display*)))



(defun do-all-frames-nw-hook (window)
  "Call nw-hook of each frame."
  (catch 'nw-hook-loop
    (let ((found nil))
      (with-all-frames (*root-frame* frame)
	(awhen (frame-nw-hook frame)
	  (setf found (call-hook it (list frame window)))))
      found)))



(defun process-new-window (window)
  "When a new window is created (or when we are scanning initial
windows), this function dresses the window up and gets it ready to be
managed."
  (setf (xlib:window-event-mask window) *window-events*)
  (set-window-state window +normal-state+)
  (setf (xlib:drawable-border-width window) (case (window-type window)
					      (:normal *border-size*)
					      (:maxsize *border-size*)
					      (:transient *border-size*)
					      (t *border-size*)))
  (grab-all-buttons window)
  (unless (never-managed-window-p window)
    (unless (do-all-frames-nw-hook window)
      (call-hook *default-nw-hook* (list *root-frame* window))))
  (netwm-add-in-client-list window))




(defun hide-existing-windows (screen)
  "Hide all existing windows in screen"
  (dolist (win (xlib:query-tree (xlib:screen-root screen)))
    (hide-window win)))

(defun process-existing-windows (screen)
  "Windows present when clfswm starts up must be absorbed by clfswm."
  (setf *in-process-existing-windows* t)
  (let ((id-list nil)
	(all-windows (get-all-windows)))
    (dolist (win (xlib:query-tree (xlib:screen-root screen)))
      (unless (child-member win all-windows)
	(let ((map-state (xlib:window-map-state win))
	      (wm-state (window-state win)))
	  (unless (or (eql (xlib:window-override-redirect win) :on)
		      (eql win *no-focus-window*)
                      (is-notify-window-p win))
	    (when (or (eql map-state :viewable)
		      (eql wm-state +iconic-state+))
	      (format t "Processing ~S: type=~A ~S~%" (xlib:wm-name win) (window-type win) win)
	      (unhide-window win)
	      (process-new-window win)
	      (map-window win)
	      (raise-window win)
	      (pushnew (xlib:window-id win) id-list))))))
    (netwm-set-client-list id-list))
  (setf *in-process-existing-windows* nil))


;;; Child order manipulation functions
(defun put-child-on-top (child parent)
  "Put the child on top of its parent children"
  (when (frame-p parent)
    (setf (frame-child parent) (cons child (child-remove child (frame-child parent)))
	  (frame-selected-pos parent) 0)))

(defun put-child-on-bottom (child parent)
  "Put the child at the bottom of its parent children"
  (when (frame-p parent)
    (setf (frame-child parent) (append (child-remove child (frame-child parent)) (list child))
	  (frame-selected-pos parent) 0)))


