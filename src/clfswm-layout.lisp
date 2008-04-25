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
;;;      child in screen size (integer) as 5 values (rx, ry, rw, rh, raise-p).
;;;      This method can use the float size of the child (x, y ,w , h).
;;;      It can be specialised for xlib:window or frame
;;;      Raise-p is nil or :first-only or t
;;;   2- Define a seter function for your layout
;;;   3- Register your new layout with register-layout.




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


(defun get-managed-child (parent)
  "Return only the windows that are managed for tiling"
  (when (frame-p parent)
    (remove-if #'(lambda (x)
		   (and (xlib:window-p x) (not (managed-window-p x parent))))
	       (frame-child parent))))




(defmacro register-layout (layout)
  `(progn
     (setf *layout-list* (append *layout-list* (list ',layout)))
     (defun ,(intern (format nil "~A-ONCE" layout)) ()
       (set-layout-dont-leave #',(intern (subseq (format nil "~A" layout) 4)))
       (show-all-children *current-root*)
       (fixe-real-size-current-child)
       (set-layout-dont-leave #'no-layout))))


(defun set-layout-once-documentation ()
  (loop :for l :in *layout-list*
     :do (setf (documentation (create-symbol (format nil "~A" l) "-ONCE") 'function)
	       (documentation l 'function))))




(defun layout-ask-size (msg slot &optional (min 80))
  (when (frame-p *current-child*)
    (let ((new-size (/ (or (query-number msg (* (frame-data-slot *current-child* slot) 100)) min) 100)))
      (when (<= 0 new-size 1)
	(setf (frame-data-slot *current-child* slot) new-size)))))



;;; No layout
(defgeneric no-layout (child parent)
  (:documentation "Maximize windows in there frame - leave frame to there size (no layout)"))

(defmethod no-layout ((child xlib:window) parent)
  (with-slots (rx ry rw rh) parent
    (values (1+ rx)
	    (1+ ry)
	    (- rw 2)
	    (- rh 2)
	    :first-only)))

(defmethod no-layout ((child frame) parent)
  (values (x-fl->px (frame-x child) parent)
	  (y-fl->px (frame-y child) parent)
	  (w-fl->px (frame-w child) parent)
	  (h-fl->px (frame-h child) parent)
	  t))



(defun set-no-layout ()
  "Maximize windows in there frame - leave frame to there size (no layout)"
  (set-layout #'no-layout))

(register-layout set-no-layout)




;;; Tile layout
(defgeneric tile-layout (child parent)
  (:documentation "Tile child in its frame"))

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
	    (round (- dy 2))
	    t)))

(defun set-tile-layout ()
  "Tile child in its frame"
  (set-layout #'tile-layout))

(register-layout set-tile-layout)


;;; Tile Left
(defgeneric tile-left-layout (child parent)
  (:documentation "Tile Left: main child on left and others on right"))

(defmethod tile-left-layout (child parent)
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (= pos 0)
	  (values (1+ rx)
		  (1+ ry)
		  (- (round (* rw size)) 2)
		  (- rh 2)
		  t)
	  (values (1+ (round (+ rx (* rw size))))
		  (1+ (round (+ ry (* dy (1- pos)))))
		  (- (round (* rw (- 1 size))) 2)
		  (- (round dy) 2)
		  t)))))


(defun set-tile-left-layout ()
  "Tile Left: main child on left and others on right"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout #'tile-left-layout))

(register-layout set-tile-left-layout)



;;; Tile right
(defgeneric tile-right-layout (child parent)
  (:documentation "Tile Right: main child on right and others on left"))

(defmethod tile-right-layout (child parent)
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dy (/ rh len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (= pos 0)
	  (values (1+ (round (+ rx (* rw (- 1 size)))))
		  (1+ ry)
		  (- (round (* rw size)) 2)
		  (- rh 2)
		  t)
	  (values (1+ rx)
		  (1+ (round (+ ry (* dy (1- pos)))))
		  (- (round (* rw (- 1 size))) 2)
		  (- (round dy) 2)
		  t)))))


(defun set-tile-right-layout ()
  "Tile Right: main child on right and others on left"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout #'tile-right-layout))


(register-layout set-tile-right-layout)




;;; Tile Top
(defgeneric tile-top-layout (child parent)
  (:documentation "Tile Top: main child on top and others on bottom"))

(defmethod tile-top-layout (child parent)
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dx (/ rw len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (= pos 0)
	  (values (1+ rx)
		  (1+ ry)
		  (- rw 2)
		  (- (round (* rh size)) 2)
		  t)
	  (values (1+ (round (+ rx (* dx (1- pos)))))
		  (1+ (round (+ ry (* rh size))))
		  (- (round dx) 2)
		  (- (round (* rh (- 1 size))) 2)
		  t)))))


(defun set-tile-top-layout ()
  "Tile Top: main child on top and others on bottom"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout #'tile-top-layout))

(register-layout set-tile-top-layout)



;;; Tile Bottom
(defgeneric tile-bottom-layout (child parent)
  (:documentation "Tile Bottom: main child on bottom and others on top"))

(defmethod tile-bottom-layout (child parent)
  (with-slots (rx ry rw rh) parent
    (let* ((managed-children (get-managed-child parent))
	   (pos (position child managed-children))
	   (len (max (1- (length managed-children)) 1))
	   (dx (/ rw len))
	   (size (or (frame-data-slot parent :tile-size) 0.8)))
      (if (= pos 0)
	  (values (1+ rx)
		  (1+ (round (+ ry (* rh (- 1 size)))))
		  (- rw 2)
		  (- (round (* rh size)) 2)
		  t)
	  (values (1+ (round (+ rx (* dx (1- pos)))))
		  (1+ ry)
		  (- (round dx) 2)
		  (- (round (* rh (- 1 size))) 2)
		  t)))))



(defun set-tile-bottom-layout ()
  "Tile Bottom: main child on bottom and others on top"
  (layout-ask-size "Tile size in percent (%)" :tile-size)
  (set-layout #'tile-bottom-layout))


(register-layout set-tile-bottom-layout)





;;; Space layout
(defgeneric tile-space-layout (child parent)
  (:documentation "Tile Space: tile child in its frame leaving spaces between them"))

(defmethod tile-space-layout (child parent)
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
	      (round (- dy (* dy size 2) 2))
	      t))))

(defun set-tile-space-layout ()
  "Tile Space: tile child in its frame leaving spaces between them"
  (layout-ask-size "Space size in percent (%)" :tile-space-size 10)
  (set-layout #'tile-space-layout))

(register-layout set-tile-space-layout)
