;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Layout functions
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


;;; CONFIG - Layout menu
;;;
;;; To add a new layout:
;;;   1- define your own layout: a method returning the real size of the
;;;      child in screen size (integer) as 4 values (rx, ry, rw, rh).
;;;      This method can use the float size of the child (x, y ,w , h).
;;;      It can be specialized for xlib:window or frame
;;;   2- Define a setter function for your layout
;;;   3- Register your new layout with register-layout or create
;;;      a sub menu for it with register-layout-sub-menu.



(defparameter *layout-current-key* (1- (char-code #\a)))


;;; Generic functions
(defun set-layout (layout)
  "Set the layout of the current child"
  (when (frame-p *current-child*)
    (setf (frame-layout *current-child*) layout)
    (leave-second-mode)))

(defun set-layout-dont-leave (layout)
  "Set the layout of the current child"
  (when (frame-p *current-child*)
    (setf (frame-layout *current-child*) layout)))

(defun set-layout-once (layout-name)
  (set-layout-dont-leave layout-name)
  (show-all-children)
  (fixe-real-size-current-child)
  (set-layout-dont-leave #'no-layout))


(defun get-managed-child (parent)
  "Return only the windows that are managed for tiling"
  (when (frame-p parent)
    (remove-if #'(lambda (x)
		   (and (xlib:window-p x) (not (managed-window-p x parent))))
	       (frame-child parent))))


(defun next-layout-key ()
  (code-char (incf *layout-current-key*)))


(defun register-layout (layout)
  (add-menu-key 'frame-layout-menu (next-layout-key) layout))


(defun register-layout-sub-menu (name doc layout-list)
  (add-sub-menu 'frame-layout-menu (next-layout-key) name doc)
  (loop :for item :in layout-list
     :for i :from 0
     :do (typecase item
	   (cons (add-menu-key name (first item) (second item)))
	   (string (add-menu-comment name item))
	   (t (add-menu-key name (number->char i) item)))))




(defun layout-ask-size (msg slot &optional (min 80))
  (when (frame-p *current-child*)
    (let ((new-size (/ (or (query-number msg (* (frame-data-slot *current-child* slot) 100)) min) 100)))
      (setf (frame-data-slot *current-child* slot) (max (min new-size 0.99) 0.01)))))

(defun adjust-layout-size (slot inc)
  (when (frame-p *current-child*)
    (setf (frame-data-slot *current-child* slot)
          (max (min (+ (frame-data-slot *current-child* slot) inc) 0.99) 0.01))))

(defun inc-tile-layout-size ()
  "Increase the tile layout size"
  (adjust-layout-size :tile-size 0.05)
  (show-all-children))

(defun dec-tile-layout-size ()
  "Decrease the tile layout size"
  (adjust-layout-size :tile-size -0.05)
  (show-all-children))

(defun inc-slow-tile-layout-size ()
  "Increase slowly the tile layout size"
  (adjust-layout-size :tile-size 0.01)
  (show-all-children))

(defun dec-slow-tile-layout-size ()
  "Decrease slowly the tile layout size"
  (adjust-layout-size :tile-size -0.01)
  (show-all-children))




(defun fast-layout-switch ()
  "Switch between two layouts"
  (when (frame-p *current-child*)
    (with-slots (layout) *current-child*
      (let* ((layout-list (frame-data-slot *current-child* :fast-layout))
	     (first-layout (ensure-function (first layout-list)))
	     (second-layout (ensure-function (second layout-list))))
	(setf layout (if (eql layout first-layout)
			 second-layout
			 first-layout))
	(leave-second-mode)))))


(defun push-in-fast-layout-list ()
  "Push the current layout in the fast layout list"
  (when (frame-p *current-child*)
    (setf (frame-data-slot *current-child* :fast-layout)
	  (list (frame-layout *current-child*)
		(first (frame-data-slot *current-child* :fast-layout))))
    (leave-second-mode)))



(register-layout-sub-menu 'frame-fast-layout-menu "Frame fast layout menu"
			  '(("s" fast-layout-switch)
			    ("p" push-in-fast-layout-list)))


;;; No layout
(defgeneric no-layout (child parent)
  (:documentation "No layout: Maximize windows in their frame - Leave frames to their original size"))

(defmethod no-layout ((child xlib:window) parent)
  (with-slots (rx ry rw rh) parent
    (values (adj-border-xy rx child)
	    (adj-border-xy ry child)
	    (adj-border-wh rw child)
	    (adj-border-wh rh child))))

(defmethod no-layout ((child frame) parent)
  (values (adj-border-xy (x-fl->px (frame-x child) parent) child)
	  (adj-border-xy (y-fl->px (frame-y child) parent) child)
	  (adj-border-wh (w-fl->px (frame-w child) parent) child)
          (adj-border-wh (h-fl->px (frame-h child) parent) child)))



(defun set-no-layout ()
  "No layout: Maximize windows in their frame - Leave frames to their original size"
  (set-layout #'no-layout))

(register-layout 'set-no-layout)

;;; No layout remember size
(defun set-no-layout-remember-size ()
  "No layout: Maximize windows in their frame - Leave frames to their actual size"
  (fixe-real-size-current-child)
  (set-no-layout))

(register-layout 'set-no-layout-remember-size)



;;; Maximize layout
(defgeneric maximize-layout (child parent)
  (:documentation "Maximize layout: Maximize windows and frames in their parent frame"))

(defmethod maximize-layout (child parent)
  (with-slots (rx ry rw rh) parent
    (values (adj-border-xy rx child)
	    (adj-border-xy ry child)
	    (adj-border-wh rw child)
	    (adj-border-wh rh child))))


(defun set-maximize-layout ()
  "Maximize layout: Maximize windows and frames in their parent frame"
  (set-layout #'maximize-layout))

(register-layout 'set-maximize-layout)




;;; Tile layout
(defun tile-layout-ask-keep-position ()
  (when (frame-p *current-child*)
    (if (query-yes-or-no "Keep frame children positions?")
	(setf (frame-data-slot *current-child* :tile-layout-keep-position) :yes)
	(remove-frame-data-slot *current-child* :tile-layout-keep-position))))



(labels ((set-managed ()
           (setf (frame-data-slot *current-child* :layout-managed-children)
                 (copy-list (get-managed-child *current-child*)))))
  (defun set-layout-managed-children ()
    (when (frame-p *current-child*)
      (set-managed)
      (tile-layout-ask-keep-position)))


  (defun update-layout-managed-children-position ()
    "Update layout managed children position"
    (when (frame-p *current-child*)
      (set-managed)
      (leave-second-mode))))



(defun update-layout-managed-children-keep-position (child parent)
  (let ((managed-children (frame-data-slot parent :layout-managed-children))
	(managed-in-parent (get-managed-child parent)))
    (dolist (ch managed-in-parent)
      (unless (child-member ch managed-children)
	(setf managed-children (append managed-children (list child)))))
    (setf managed-children (remove-if-not (lambda (x)
					    (child-member x managed-in-parent))
					  managed-children))
    (setf (frame-data-slot parent :layout-managed-children) managed-children)
    managed-children))

(defun update-layout-managed-children (child parent)
  (if (eql (frame-data-slot parent :tile-layout-keep-position) :yes)
      (update-layout-managed-children-keep-position child parent)
      (get-managed-child parent)))



(defgeneric tile-layout (child parent)
  (:documentation "Tile child in its frame (vertical)"))

(defmethod tile-layout (child parent)
  (let* ((managed-children (update-layout-managed-children child parent))
	 (pos (child-position child managed-children))
	 (len (length managed-children))
	 (nx (ceiling (sqrt len)))
	 (ny  (ceiling (/ len nx)))
	 (dx (/ (frame-rw parent) nx))
	 (dy (/ (frame-rh parent) ny))
	 (dpos (- (* nx ny) len))
	 (width dx))
    (when (plusp dpos)
      (if (zerop pos)
	  (setf width (* dx (1+ dpos)))
	  (incf pos dpos)))
    (values (round (adj-border-xy (+ (frame-rx parent) (truncate (* (mod pos nx) dx))) child))
	    (round (adj-border-xy (+ (frame-ry parent) (truncate (* (truncate (/ pos nx)) dy))) child))
	    (round (adj-border-wh width child))
	    (round (adj-border-wh dy child)))))

(defun set-tile-layout ()
  "Tile child in its frame (vertical)"
  (set-layout-managed-children)
  (set-layout #'tile-layout))



;; Horizontal tiling layout
(defgeneric tile-horizontal-layout (child parent)
  (:documentation "Tile child in its frame (horizontal)"))

(defmethod tile-horizontal-layout (child parent)
  (let* ((managed-children (update-layout-managed-children child parent))
	 (pos (child-position child managed-children))
	 (len (length managed-children))
	 (ny (ceiling (sqrt len)))
	 (nx (ceiling (/ len ny)))
	 (dx (/ (frame-rw parent) nx))
	 (dy (/ (frame-rh parent) ny))
	 (dpos (- (* nx ny) len))
	 (height dy))
    (when (plusp dpos)
      (if (zerop pos)
	  (setf height (* dy (1+ dpos)))
	  (incf pos dpos)))
    (values (round (adj-border-xy (+ (frame-rx parent) (truncate (* (truncate (/ pos ny)) dx))) child))
	    (round (adj-border-xy (+ (frame-ry parent) (truncate (* (mod pos ny) dy))) child))
	    (round (adj-border-wh dx child))
	    (round (adj-border-wh height child)))))

(defun set-tile-horizontal-layout ()
  "Tile child in its frame (horizontal)"
  (set-layout-managed-children)
  (set-layout #'tile-horizontal-layout))



;; One column layout
(defgeneric one-column-layout (child parent)
  (:documentation "One column layout"))

(defmethod one-column-layout (child parent)
  (let* ((managed-children (update-layout-managed-children child parent))
	 (pos (child-position child managed-children))
	 (len (length managed-children))
	 (dy (/ (frame-rh parent) len)))
    (values (round (adj-border-xy (frame-rx parent) child))
	    (round (adj-border-xy (+ (frame-ry parent) (*  pos dy)) child))
	    (round (adj-border-wh (frame-rw parent) child))
	    (round (adj-border-wh dy child)))))

(defun set-one-column-layout ()
  "One column layout"
  (set-layout-managed-children)
  (set-layout #'one-column-layout))


;; One line layout
(defgeneric one-line-layout (child parent)
  (:documentation "One line layout"))

(defmethod one-line-layout (child parent)
  (let* ((managed-children (update-layout-managed-children child parent))
	 (pos (child-position child managed-children))
	 (len (length managed-children))
	 (dx (/ (frame-rw parent) len)))
    (values (round (adj-border-xy (+ (frame-rx parent) (*  pos dx)) child))
	    (round (adj-border-xy (frame-ry parent) child))
	    (round (adj-border-wh dx child))
	    (round (adj-border-wh (frame-rh parent) child)))))

(defun set-one-line-layout ()
  "One line layout"
  (set-layout-managed-children)
  (set-layout #'one-line-layout))





;;; Space layout
(defun tile-space-layout (child parent)
  "Tile Space: tile child in its frame leaving spaces between them"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (update-layout-managed-children child parent))
	   (pos (child-position child managed-children))
	   (len (length managed-children))
	   (n (ceiling (sqrt len)))
	   (dx (/ rw n))
	   (dy (/ rh (ceiling (/ len n))))
	   (size (or (frame-data-slot parent :tile-space-size) 0.1)))
      (when (> size 0.5) (setf size 0.45))
      (values (round (adj-border-xy (+ rx (truncate (* (mod pos n) dx)) (* dx size)) child))
	      (round (adj-border-xy (+ ry (truncate (* (truncate (/ pos n)) dy)) (* dy size)) child))
	      (round (adj-border-wh (- dx (* dx size 2)) child))
	      (round (adj-border-wh (- dy (* dy size 2)) child))))))




(defun set-tile-space-layout ()
  "Tile Space: tile child in its frame leaving spaces between them"
  (layout-ask-size "Space size in percent (%)" :tile-space-size 0.01)
  (set-layout-managed-children)
  (set-layout #'tile-space-layout))



(register-layout-sub-menu 'frame-tile-layout-menu "Frame tile layout menu"
			  '(("v" set-tile-layout)
			    ("h" set-tile-horizontal-layout)
			    ("c" set-one-column-layout)
			    ("l" set-one-line-layout)
			    ("s" set-tile-space-layout)))



;;; Tile Left
(defun tile-left-layout (child parent)
  "Tile Left: main child on left and others on right"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (update-layout-managed-children child parent))
	   (pos (child-position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (adj-border-xy rx child)
		      (adj-border-xy ry child)
		      (adj-border-wh (round (* rw size)) child)
		      (adj-border-wh rh child))
	      (values (adj-border-xy (round (+ rx (* rw size))) child)
		      (adj-border-xy (round (+ ry (* dy (1- pos)))) child)
		      (adj-border-wh (round (* rw (- 1 size))) child)
		      (adj-border-wh (round dy) child)))
	  (no-layout child parent)))))


(defun set-tile-left-layout ()
  "Tile Left: main child on left and others on right"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout-managed-children)
  (set-layout #'tile-left-layout))



;;; Tile right
(defun tile-right-layout (child parent)
  "Tile Right: main child on right and others on left"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (update-layout-managed-children child parent))
	   (pos (child-position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (adj-border-xy (round (+ rx (* rw (- 1 size)))) child)
		      (adj-border-xy ry child)
		      (adj-border-wh (round (* rw size)) child)
		      (adj-border-wh rh child))
	      (values (adj-border-xy rx child)
		      (adj-border-xy (round (+ ry (* dy (1- pos)))) child)
		      (adj-border-wh (round (* rw (- 1 size))) child)
		      (adj-border-wh (round dy) child)))
	  (no-layout child parent)))))


(defun set-tile-right-layout ()
  "Tile Right: main child on right and others on left"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout-managed-children)
  (set-layout #'tile-right-layout))






;;; Tile Top
(defun tile-top-layout (child parent)
  "Tile Top: main child on top and others on bottom"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (update-layout-managed-children child parent))
	   (pos (child-position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dx (/ rw len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (adj-border-xy rx child)
		      (adj-border-xy ry child)
		      (adj-border-wh rw child)
		      (adj-border-wh (round (* rh size)) child))
	      (values (adj-border-xy (round (+ rx (* dx (1- pos)))) child)
		      (adj-border-xy (round (+ ry (* rh size))) child)
		      (adj-border-wh (round dx) child)
		      (adj-border-wh (round (* rh (- 1 size))) child)))
	  (no-layout child parent)))))


(defun set-tile-top-layout ()
  "Tile Top: main child on top and others on bottom"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout-managed-children)
  (set-layout #'tile-top-layout))




;;; Tile Bottom
(defun tile-bottom-layout (child parent)
  "Tile Bottom: main child on bottom and others on top"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (update-layout-managed-children child parent))
	   (pos (child-position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dx (/ rw len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (adj-border-xy rx child)
		      (adj-border-xy (round (+ ry (* rh (- 1 size)))) child)
		      (adj-border-wh rw child)
		      (adj-border-wh (round (* rh size)) child))
	      (values (adj-border-xy (round (+ rx (* dx (1- pos)))) child)
		      (adj-border-xy ry child)
		      (adj-border-wh (round dx) child)
		      (adj-border-wh (round (* rh (- 1 size))) child)))
	  (no-layout child parent)))))



(defun set-tile-bottom-layout ()
  "Tile Bottom: main child on bottom and others on top"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout-managed-children)
  (set-layout #'tile-bottom-layout))


(register-layout-sub-menu 'frame-tile-dir-layout-menu "Tile in one direction layout menu"
			  '(("l" set-tile-left-layout)
			    ("r" set-tile-right-layout)
			    ("t" set-tile-top-layout)
			    ("b" set-tile-bottom-layout)))






;;; Left and space layout: like left layout but leave a space on the left
(defun layout-ask-space (msg slot &optional (default 100))
  (when (frame-p *current-child*)
    (let ((new-space (or (query-number msg (or (frame-data-slot *current-child* slot) default)) default)))
      (setf (frame-data-slot *current-child* slot) new-space))))


(defun tile-left-space-layout (child parent)
  "Tile Left Space: main child on left and others on right. Leave some space (in pixels) on the left."
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (update-layout-managed-children child parent))
	   (pos (child-position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8))
	   (space (or (frame-data-slot parent :tile-left-space) 100)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (adj-border-xy (+ rx space) child)
		      (adj-border-xy ry child)
		      (adj-border-wh (- (round (* rw size)) space) child)
		      (adj-border-wh rh child))
	      (values (adj-border-xy (round (+ rx (* rw size))) child)
		      (adj-border-xy (round (+ ry (* dy (1- pos)))) child)
		      (adj-border-wh (round (* rw (- 1 size))) child)
		      (adj-border-wh (round dy) child)))
	  (multiple-value-bind (rnx rny rnw rnh)
	      (no-layout child parent)
	    (values (+ rnx space)
		    rny
		    (- rnw space)
		    rnh))))))


(defun set-tile-left-space-layout ()
  "Tile Left Space: main child on left and others on right. Leave some space on the left."
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (layout-ask-space "Tile space (in pixels)" :tile-left-space)
  (set-layout-managed-children)
  (set-layout #'tile-left-space-layout))

(register-layout-sub-menu 'frame-tile-space-layout-menu "Tile with some space on one side menu"
			  '(set-tile-left-space-layout))




;;; Main windows layout - A possible GIMP layout
;;;   The windows in the main list are tiled on the frame
;;;   others windows are on one side of the frame.
(defun main-window-right-layout (child parent)
  "Main window right: Main windows on the right. Others on the left."
  (with-slots (rx ry rw rh) parent
    (let* ((main-windows (frame-data-slot parent :main-window-list))
	   (len (length main-windows))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (zerop len)
	  (no-layout child parent)
	  (if (child-member child main-windows)
	      (let* ((dy (/ rh len))
		     (pos (child-position child main-windows)))
		(values (adj-border-xy (round (+ rx (* rw (- 1 size)))) child)
			(adj-border-xy (round (+ ry (* dy pos))) child)
			(adj-border-wh (round (* rw size)) child)
			(adj-border-wh (round dy) child)))
	      (values (adj-border-xy rx child)
		      (adj-border-xy ry child)
		      (adj-border-wh (round (* rw (- 1 size))) child)
		      (adj-border-wh rh child)))))))

(defun set-main-window-right-layout ()
  "Main window right: Main windows on the right. Others on the left."
  (layout-ask-size "Split size in percent (%)" :tile-size)
  (set-layout #'main-window-right-layout))




(defun main-window-left-layout (child parent)
  "Main window left: Main windows on the left. Others on the right."
  (with-slots (rx ry rw rh) parent
    (let* ((main-windows (frame-data-slot parent :main-window-list))
	   (len (length main-windows))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (zerop len)
	  (no-layout child parent)
	  (if (child-member child main-windows)
	      (let* ((dy (/ rh len))
		     (pos (child-position child main-windows)))
		(values (adj-border-xy rx child)
			(adj-border-xy (round (+ ry (* dy pos))) child)
			(adj-border-wh (round (* rw size)) child)
			(adj-border-wh (round dy) child)))
	      (values (adj-border-xy (round (+ rx (* rw size))) child)
		      (adj-border-xy ry child)
		      (adj-border-wh (round (* rw (- 1 size))) child)
		      (adj-border-wh rh child)))))))

(defun set-main-window-left-layout ()
  "Main window left: Main windows on the left. Others on the right."
  (layout-ask-size "Split size in percent (%)" :tile-size)
  (set-layout #'main-window-left-layout))



(defun main-window-top-layout (child parent)
  "Main window top: Main windows on the top. Others on the bottom."
  (with-slots (rx ry rw rh) parent
    (let* ((main-windows (frame-data-slot parent :main-window-list))
	   (len (length main-windows))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (zerop len)
	  (no-layout child parent)
	  (if (child-member child main-windows)
	      (let* ((dx (/ rw len))
		     (pos (child-position child main-windows)))
		(values (adj-border-xy (round (+ rx (* dx pos))) child)
			(adj-border-xy ry child)
			(adj-border-wh (round dx) child)
			(adj-border-wh (round (* rh size)) child)))
	      (values (adj-border-xy rx child)
		      (adj-border-xy (round (+ ry (* rh size))) child)
		      (adj-border-wh rw child)
		      (adj-border-wh (round (* rh (- 1 size))) child)))))))

(defun set-main-window-top-layout ()
  "Main window top: Main windows on the top. Others on the bottom."
  (layout-ask-size "Split size in percent (%)" :tile-size)
  (set-layout #'main-window-top-layout))



(defun main-window-bottom-layout (child parent)
  "Main window bottom: Main windows on the bottom. Others on the top."
  (with-slots (rx ry rw rh) parent
    (let* ((main-windows (frame-data-slot parent :main-window-list))
	   (len (length main-windows))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (zerop len)
	  (no-layout child parent)
	  (if (child-member child main-windows)
	      (let* ((dx (/ rw len))
		     (pos (child-position child main-windows)))
		(values (adj-border-xy (round (+ rx (* dx pos))) child)
			(adj-border-xy (round (+ ry (* rh (- 1 size)))) child)
			(adj-border-wh (round dx) child)
			(adj-border-wh (round (* rh size)) child)))
	      (values (adj-border-xy rx child)
		      (adj-border-xy ry child)
		      (adj-border-wh rw child)
		      (adj-border-wh (round (* rh (- 1 size))) child)))))))

(defun set-main-window-bottom-layout ()
  "Main window bottom: Main windows on the bottom. Others on the top."
  (layout-ask-size "Split size in percent (%)" :tile-size)
  (set-layout #'main-window-bottom-layout))





(defun add-in-main-window-list ()
  "Add the current window in the main window list"
  (when (frame-p *current-child*)
    (with-current-window
      (when (child-member window (get-managed-child *current-child*))
	(pushnew window (frame-data-slot *current-child* :main-window-list)))))
  (leave-second-mode))


(defun remove-in-main-window-list ()
  "Remove the current window from the main window list"
  (when (frame-p *current-child*)
    (with-current-window
      (when (child-member window (get-managed-child *current-child*))
	(setf (frame-data-slot *current-child* :main-window-list)
	      (child-remove window (frame-data-slot *current-child* :main-window-list))))))
  (leave-second-mode))

(defun clear-main-window-list ()
  "Clear the main window list"
  (when (frame-p *current-child*)
    (setf (frame-data-slot *current-child* :main-window-list) nil))
  (leave-second-mode))




(register-layout-sub-menu 'frame-main-window-layout-menu "Main window layout menu"
			  '(("r" set-main-window-right-layout)
			    ("l" set-main-window-left-layout)
			    ("t" set-main-window-top-layout)
			    ("b" set-main-window-bottom-layout)
			    "-=- Actions on main windows list -=-"
			    ("a" add-in-main-window-list)
			    ("v" remove-in-main-window-list)
			    ("c" clear-main-window-list)))


;;; GIMP layout specifics functions
;;;
(defconfig *gimp-layout-notify-window-delay* 30 'gimp-layout
           "Time to display the GIMP layout notify window help")


(defun select-next/previous-child-no-main-window (fun-rotate)
  "Select the next/previous child - Skip windows in main window list"
  (when (frame-p *current-child*)
    (with-slots (child) *current-child*
      (let* ((main-windows (frame-data-slot *current-child* :main-window-list))
	     (to-skip? (not (= (length main-windows)
			       (length child)))))
	(labels ((rec ()
		   (setf child (funcall fun-rotate child))
		   (when (and to-skip?
			      (child-member (frame-selected-child *current-child*) main-windows))
		     (rec))))
	  (unselect-all-frames)
	  (rec)
	  (show-all-children))))))


(defun select-next-child-no-main-window ()
  "Select the next child - Skip windows in main window list"
  (select-next/previous-child-no-main-window #'rotate-list))

(defun select-previous-child-no-main-window ()
  "Select the previous child - Skip windows in main window list"
  (select-next/previous-child-no-main-window #'anti-rotate-list))


(defun mouse-click-to-focus-and-move-no-main-window (window root-x root-y)
  "Move and focus the current frame or focus the current window parent.
Or do actions on corners - Skip windows in main window list"
  (unless (do-corner-action root-x root-y *corner-main-mode-left-button*)
    (if (and (frame-p *current-child*)
	     (child-member window (frame-data-slot *current-child* :main-window-list)))
	(replay-button-event)
	(mouse-click-to-focus-generic root-x root-y #'move-frame))))



(let ((help-text-list `(("-=- Help on The GIMP layout -=-" ,*info-color-title*)
                        ""
                        "The GIMP layout is a main-window-layout with a sloppy focus policy."
                        "You can change the main windows direction with the layout menu."
                        ""
                        "Press Alt+F8 to add a window to the main windows list."
                        "Press Alt+F9 to remove a window from the main windows list."
                        "Press Alt+F10 to clear the main windows list."
                        ""
                        "You can select a main window with the right mouse button."
                        ""
                        "Use the layout menu to restore the previous layout and keybinding.")))
  (defun help-on-gimp-layout ()
    "Help on the GIMP layout"
    (info-mode help-text-list)
    (leave-second-mode))

  (defun set-gimp-layout ()
    "The GIMP Layout"
    (when (frame-p *current-child*)
      ;; Note: There is no need to ungrab/grab keys because this
      ;; is done when leaving the second mode.
      (define-main-key ("F8" :mod-1) 'add-in-main-window-list)
      (define-main-key ("F9" :mod-1) 'remove-in-main-window-list)
      (define-main-key ("F10" :mod-1) 'clear-main-window-list)
      (define-main-key ("Tab" :mod-1) 'select-next-child-no-main-window)
      (define-main-key ("Tab" :mod-1 :shift) 'select-previous-child-no-main-window)
      (define-main-mouse (1) 'mouse-click-to-focus-and-move-no-main-window)
      (setf (frame-data-slot *current-child* :focus-policy-save)
            (frame-focus-policy *current-child*))
      (setf (frame-focus-policy *current-child*) :sloppy)
      (setf (frame-data-slot *current-child* :layout-save)
            (frame-layout *current-child*))
      (open-notify-window help-text-list)
      (add-timer *gimp-layout-notify-window-delay* #'close-notify-window)
      ;; Set the default layout and leave the second mode.
      (set-main-window-right-layout))))


(defun set-previous-layout ()
  "Restore the previous layout"
  (undefine-main-key ("F8" :mod-1))
  (undefine-main-key ("F9" :mod-1))
  (undefine-main-key ("F10" :mod-1))
  (define-main-key ("Tab" :mod-1) 'select-next-child)
  (define-main-key ("Tab" :mod-1 :shift) 'select-previous-child)
  (define-main-mouse (1) 'mouse-click-to-focus-and-move)
  (setf (frame-focus-policy *current-child*)
	(frame-data-slot *current-child* :focus-policy-save))
  (setf (frame-layout *current-child*)
	(frame-data-slot *current-child* :layout-save))
  (leave-second-mode))




(register-layout-sub-menu 'frame-gimp-layout-menu "The GIMP layout menu"
			  '(("g" set-gimp-layout)
			    ("p" set-previous-layout)
			    ("h" help-on-gimp-layout)
			    "-=- Main window layout -=-"
			    ("r" set-main-window-right-layout)
			    ("l" set-main-window-left-layout)
			    ("t" set-main-window-top-layout)
			    ("b" set-main-window-bottom-layout)
			    "-=- Actions on main windows list -=-"
			    ("a" add-in-main-window-list)
			    ("v" remove-in-main-window-list)
			    ("c" clear-main-window-list)))


