;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Layout functions
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


;;; CONFIG - Layout menu
;;;
;;; To add a new layout:
;;;   1- define your own layout: a method returning the real size of the
;;;      child in screen size (integer) as 5 values (rx, ry, rw, rh).
;;;      This method can use the float size of the child (x, y ,w , h).
;;;      It can be specialised for xlib:window or frame
;;;   2- Define a seter function for your layout
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
  (show-all-children *current-root*)
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
      (when (<= 0 new-size 1)
	(setf (frame-data-slot *current-child* slot) new-size)))))





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
  (:documentation "No layout: Maximize windows in there frame - Leave frame to there size"))

(defmethod no-layout ((child xlib:window) parent)
  (with-slots (rx ry rw rh) parent
    (values (1+ rx)
	    (1+ ry)
	    (- rw 2)
	    (- rh 2))))

(defmethod no-layout ((child frame) parent)
  (values (x-fl->px (frame-x child) parent)
	  (y-fl->px (frame-y child) parent)
	  (w-fl->px (frame-w child) parent)
	  (h-fl->px (frame-h child) parent)))



(defun set-no-layout ()
  "No layout: Maximize windows in there frame - Leave frame to there size"
  (set-layout #'no-layout))

(register-layout 'set-no-layout)




;;; Tile layout
(defgeneric tile-layout (child parent)
  (:documentation "Tile child in its frame (vertical)"))

(defmethod tile-layout (child parent)
  (let* ((managed-children (get-managed-child parent))
	 (pos (position child managed-children))
	 (len (length managed-children))
	 (n (ceiling (sqrt len)))
	 (dx (/ (frame-rw parent) n))
	 (dy (/ (frame-rh parent) (ceiling (/ len n)))))
    (values (round (+ (frame-rx parent) (truncate (* (mod pos n) dx)) 1))
	    (round (+ (frame-ry parent) (truncate (* (truncate (/ pos n)) dy)) 1))
	    (round (- dx 2))
	    (round (- dy 2)))))

(defun set-tile-layout ()
  "Tile child in its frame (vertical)"
  (set-layout #'tile-layout))


(defgeneric tile-horizontal-layout (child parent)
  (:documentation "Tile child in its frame (horizontal)"))

(defmethod tile-horizontal-layout (child parent)
  (let* ((managed-children (get-managed-child parent))
	 (pos (position child managed-children))
	 (len (length managed-children))
	 (n (ceiling (sqrt len)))
	 (dx (/ (frame-rw parent) (ceiling (/ len n))))
	 (dy (/ (frame-rh parent) n)))
    (values (round (+ (frame-rx parent) (truncate (* (truncate (/ pos n)) dx)) 1))
	    (round (+ (frame-ry parent) (truncate (* (mod pos n) dy)) 1))
	    (round (- dx 2))
	    (round (- dy 2)))))

(defun set-tile-horizontal-layout ()
  "Tile child in its frame (horizontal)"
  (set-layout #'tile-horizontal-layout))

;;; Space layout
(defun tile-space-layout (child parent)
  "Tile Space: tile child in its frame leaving spaces between them"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (length managed-children))
	   (n (ceiling (sqrt len)))
	   (dx (/ rw n))
	   (dy (/ rh (ceiling (/ len n))))
	   (size (or (frame-data-slot parent :tile-space-size) 0.1)))
      (when (> size 0.5) (setf size 0.45))
      (values (round (+ rx (truncate (* (mod pos n) dx)) (* dx size) 1))
	      (round (+ ry (truncate (* (truncate (/ pos n)) dy)) (* dy size) 1))
	      (round (- dx (* dx size 2) 2))
	      (round (- dy (* dy size 2) 2))))))




(defun set-tile-space-layout ()
  "Tile Space: tile child in its frame leaving spaces between them"
  (layout-ask-size "Space size in percent (%)" :tile-space-size 10)
  (set-layout #'tile-space-layout))



(register-layout-sub-menu 'frame-tile-layout-menu "Frame tile layout menu"
			  '(("v" set-tile-layout)
			    ("h" set-tile-horizontal-layout)
			    ("s" set-tile-space-layout)))



;;; Tile Left
(defun tile-left-layout (child parent)
  "Tile Left: main child on left and others on right"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (1+ rx)
		      (1+ ry)
		      (- (round (* rw size)) 2)
		      (- rh 2))
	      (values (1+ (round (+ rx (* rw size))))
		      (1+ (round (+ ry (* dy (1- pos)))))
		      (- (round (* rw (- 1 size))) 2)
		      (- (round dy) 2)))
	  (no-layout child parent)))))


(defun set-tile-left-layout ()
  "Tile Left: main child on left and others on right"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout #'tile-left-layout))



;;; Tile right
(defun tile-right-layout (child parent)
  "Tile Right: main child on right and others on left"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (1+ (round (+ rx (* rw (- 1 size)))))
		      (1+ ry)
		      (- (round (* rw size)) 2)
		      (- rh 2))
	      (values (1+ rx)
		      (1+ (round (+ ry (* dy (1- pos)))))
		      (- (round (* rw (- 1 size))) 2)
		      (- (round dy) 2)))
	  (no-layout child parent)))))


(defun set-tile-right-layout ()
  "Tile Right: main child on right and others on left"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout #'tile-right-layout))






;;; Tile Top
(defun tile-top-layout (child parent)
  "Tile Top: main child on top and others on bottom"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dx (/ rw len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (1+ rx)
		      (1+ ry)
		      (- rw 2)
		      (- (round (* rh size)) 2))
	      (values (1+ (round (+ rx (* dx (1- pos)))))
		      (1+ (round (+ ry (* rh size))))
		      (- (round dx) 2)
		      (- (round (* rh (- 1 size))) 2)))
	  (no-layout child parent)))))


(defun set-tile-top-layout ()
  "Tile Top: main child on top and others on bottom"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout #'tile-top-layout))




;;; Tile Bottom
(defun tile-bottom-layout (child parent)
  "Tile Bottom: main child on bottom and others on top"
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dx (/ rw len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (1+ rx)
		      (1+ (round (+ ry (* rh (- 1 size)))))
		      (- rw 2)
		      (- (round (* rh size)) 2))
	      (values (1+ (round (+ rx (* dx (1- pos)))))
		      (1+ ry)
		      (- (round dx) 2)
		      (- (round (* rh (- 1 size))) 2)))
	  (no-layout child parent)))))



(defun set-tile-bottom-layout ()
  "Tile Bottom: main child on bottom and others on top"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
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
  "Tile Left Space: main child on left and others on right. Leave some space on the left."
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8))
	   (space (or (frame-data-slot parent :tile-left-space) 100)))
      (if (> (length managed-children) 1)
	  (if (= pos 0)
	      (values (+ rx space 1)
		      (1+ ry)
		      (- (round (* rw size)) 2 space)
		      (- rh 2))
	      (values (1+ (round (+ rx (* rw size))))
		      (1+ (round (+ ry (* dy (1- pos)))))
		      (- (round (* rw (- 1 size))) 2)
		      (- (round dy) 2)))
	  (multiple-value-bind (rnx rny rnw rnh)
	      (no-layout child parent)
	    (values (+ rnx space)
		    rny
		    (- rnw space)
		    rnh))))))


(defun set-tile-left-space-layout ()
  "Tile Left Space: main child on left and others on right. Leave some space on the left."
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (layout-ask-space "Tile space" :tile-left-space)
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
	  (if (member child main-windows)
	      (let* ((dy (/ rh len))
		     (pos (position child main-windows)))
		(values (1+ (round (+ rx (* rw (- 1 size)))))
			(1+ (round (+ ry (* dy pos))))
			(- (round (* rw size)) 2)
			(- (round dy) 2)))
	      (values (1+ rx)
		      (1+ ry)
		      (- (round (* rw (- 1 size))) 2)
		      (- rh 2)))))))
	  
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
	  (if (member child main-windows)
	      (let* ((dy (/ rh len))
		     (pos (position child main-windows)))
		(values (1+ rx)
			(1+ (round (+ ry (* dy pos))))
			(- (round (* rw size)) 2)
			(- (round dy) 2)))
	      (values (1+ (round (+ rx (* rw size))))
		      (1+ ry)
		      (- (round (* rw (- 1 size))) 2)
		      (- rh 2)))))))
	  
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
	  (if (member child main-windows)
	      (let* ((dx (/ rw len))
		     (pos (position child main-windows)))
		(values (1+ (round (+ rx (* dx pos))))
			(1+ ry)
			(- (round dx) 2)
			(- (round (* rh size)) 2)))
	      (values (1+ rx)
		      (1+ (round (+ ry (* rh size))))
		      (- rw 2)
		      (- (round (* rh (- 1 size))) 2)))))))
	  
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
	  (if (member child main-windows)
	      (let* ((dx (/ rw len))
		     (pos (position child main-windows)))
		(values (1+ (round (+ rx (* dx pos))))
			(1+ (round (+ ry (* rh (- 1 size)))))
			(- (round dx) 2)
			(- (round (* rh size)) 2)))
	      (values (1+ rx)
		      (1+ ry)
		      (- rw 2)
		      (- (round (* rh (- 1 size))) 2)))))))
	  
(defun set-main-window-bottom-layout ()
  "Main window bottom: Main windows on the bottom. Others on the top."
  (layout-ask-size "Split size in percent (%)" :tile-size)
  (set-layout #'main-window-bottom-layout))





(defun add-in-main-window-list ()
  "Add the current window in the main window list"
  (when (frame-p *current-child*)
    (with-current-window
      (when (member window (get-managed-child *current-child*))
	(pushnew window (frame-data-slot *current-child* :main-window-list)))))
  (leave-second-mode))


(defun remove-in-main-window-list ()
  "Remove the current window from the main window list"
  (when (frame-p *current-child*)
    (with-current-window
      (when (member window (get-managed-child *current-child*))
	(setf (frame-data-slot *current-child* :main-window-list)
	      (remove window (frame-data-slot *current-child* :main-window-list))))))
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
