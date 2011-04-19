;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Tile, pack and fill functions
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


;;;,-----
;;;| Edges functions
;;;`-----
(defun frame-x2 (frame)
  (+ (frame-x frame) (frame-w frame)))

(defun frame-y2 (frame)
  (+ (frame-y frame) (frame-h frame)))


(defun find-edge-up (current-frame parent)
  (let ((y-found 0))
    (dolist (frame (frame-child parent))
      (when (and (frame-p frame)
		 (not (equal frame current-frame))
		 (<= (frame-y2 frame) (frame-y current-frame))
		 (>= (frame-x2 frame) (frame-x current-frame))
		 (<= (frame-x frame) (frame-x2 current-frame)))
	(setf y-found (max y-found (frame-y2 frame)))))
    y-found))

(defun find-edge-down (current-frame parent)
  (let ((y-found 1))
    (dolist (frame (frame-child parent))
      (when (and (frame-p frame)
		 (not (equal frame current-frame))
		 (>= (frame-y frame) (frame-y2 current-frame))
		 (>= (frame-x2 frame) (frame-x current-frame))
		 (<= (frame-x frame) (frame-x2 current-frame)))
	(setf y-found (min y-found (frame-y frame)))))
    y-found))

(defun find-edge-right (current-frame parent)
  (let ((x-found 1))
    (dolist (frame (frame-child parent))
      (when (and (frame-p frame)
		 (not (equal frame current-frame))
		 (>= (frame-x frame) (frame-x2 current-frame))
		 (>= (frame-y2 frame) (frame-y current-frame))
		 (<= (frame-y frame) (frame-y2 current-frame)))
	(setf x-found (min x-found (frame-x frame)))))
    x-found))


(defun find-edge-left (current-frame parent)
  (let ((x-found 0))
    (dolist (frame (frame-child parent))
      (when (and (frame-p frame)
		 (not (equal frame current-frame))
		 (<= (frame-x2 frame) (frame-x current-frame))
		 (>= (frame-y2 frame) (frame-y current-frame))
		 (<= (frame-y frame) (frame-y2 current-frame)))
	(setf x-found (max x-found (frame-x2 frame)))))
    x-found))



;;;,-----
;;;| Pack functions
;;;`-----
(defun pack-frame-up (frame parent)
  "Pack frame to up"
  (let ((y-found (find-edge-up frame parent)))
    (setf (frame-y frame) y-found)))


(defun pack-frame-down (frame parent)
  "Pack frame to down"
  (let ((y-found (find-edge-down frame parent)))
    (setf (frame-y frame) (- y-found (frame-h frame)))))

(defun pack-frame-right (frame parent)
  "Pack frame to right"
  (let ((x-found (find-edge-right frame parent)))
    (setf (frame-x frame) (- x-found (frame-w frame)))))


(defun pack-frame-left (frame parent)
  "Pack frame to left"
  (let ((x-found (find-edge-left frame parent)))
    (setf (frame-x frame) x-found)))



(defun center-frame (frame)
  "Center frame"
  (setf (frame-x frame) (/ (- 1 (frame-w frame)) 2)
	(frame-y frame) (/ (- 1 (frame-h frame)) 2)))

;;;,-----
;;;| Fill functions
;;;`-----
(defun fill-frame-up (frame parent)
  "Fill a frame up"
  (let* ((y-found (find-edge-up frame parent))
	 (dy (- (frame-y frame) y-found)))
    (setf (frame-y frame) y-found
	  (frame-h frame) (+ (frame-h frame) dy))))

(defun fill-frame-down (frame parent)
  "Fill a frame down"
  (let* ((y-found (find-edge-down frame parent))
	 (dy (- y-found (frame-y2 frame))))
    (setf (frame-h frame) (+ (frame-h frame) dy))))


(defun fill-frame-left (frame parent)
  "Fill a frame left"
  (let* ((x-found (find-edge-left frame parent))
	 (dx (- (frame-x frame) x-found)))
    (setf (frame-x frame) x-found
	  (frame-w frame) (+ (frame-w frame) dx))))

(defun fill-frame-right (frame parent)
  "Fill a frame rigth"
  (let* ((x-found (find-edge-right frame parent))
	 (dx (- x-found (frame-x2 frame))))
    (setf (frame-w frame) (+ (frame-w frame) dx))))


;;;,-----
;;;| Lower functions
;;;`-----
(defun resize-frame-down (frame)
  "Resize down a frame"
  (when (> (frame-w frame) 0.1)
    (setf (frame-x frame) (+ (frame-x frame) 0.01)
	  (frame-w frame) (max (- (frame-w frame) 0.02) 0.01)))
  (when (> (frame-h frame) 0.1)
    (setf (frame-y frame) (+ (frame-y frame) 0.01)
	  (frame-h frame) (max (- (frame-h frame) 0.02) 0.01))))


(defun resize-minimal-frame (frame)
  "Resize down a frame to its minimal size"
  (dotimes (i 100)
    (resize-frame-down frame)))





(defun resize-half-width-left (frame)
  (setf (frame-w frame)(/ (frame-w frame) 2)))


(defun resize-half-width-right (frame)
  (let* ((new-size (/ (frame-w frame) 2))
	 (dx (- (frame-w frame) new-size)))
    (setf (frame-w frame) new-size)
    (incf (frame-x frame) (max dx 0))))


(defun resize-half-height-up (frame)
  (setf (frame-h frame) (/ (frame-h frame) 2)))

(defun resize-half-height-down (frame)
  (let* ((new-size (/ (frame-h frame) 2))
	 (dy (- (frame-h frame) new-size)))
    (setf (frame-h frame) new-size)
    (incf (frame-y frame) (max dy 0))))




;;;;;,-----
;;;;;| Explode/Implode functions
;;;;;`-----
(defun explode-frame (frame)
  "Create a new frame for each window in frame"
  (when (frame-p frame)
    (let ((windows (loop :for child :in (frame-child frame)
		      :when (xlib:window-p child)
		      :collect child)))
      (dolist (win windows)
	(add-frame (create-frame :child (list win)) frame)
	(remove-child-in-frame win frame)))))


(defun explode-current-frame ()
  "Create a new frame for each window in frame"
  (explode-frame *current-child*)
  (leave-second-mode))


(defun implode-frame (frame)
  "Absorb all frames subchildren in frame (explode frame opposite)"
  (when (frame-p frame)
    (dolist (child (frame-child frame))
      (when (frame-p child)
        (dolist (subchild (frame-child child))
          (setf (frame-child frame) (append (frame-child frame) (list subchild))))
        (hide-child child)
        (remove-child-in-frame child frame)))))

(defun implode-current-frame ()
  "Absorb all frames subchildren in frame (explode frame opposite)"
  (implode-frame *current-child*)
  (leave-second-mode))




;;;;;,-----
;;;;;| Constrained move/resize frames
;;;;;`-----
(labels ((readjust-all-frames-fl-size (parent)
           (dolist (child (frame-child parent))
             (when (frame-p child)
               (setf (frame-x child) (x-px->fl (xlib:drawable-x (frame-window child)) parent)
                     (frame-y child) (y-px->fl (xlib:drawable-y (frame-window child)) parent)
                     (frame-w child) (w-px->fl (anti-adj-border-wh (xlib:drawable-width (frame-window child)) parent) parent)
                     (frame-h child) (h-px->fl (anti-adj-border-wh (xlib:drawable-height (frame-window child)) parent) parent))))))
  (defun move-frame-constrained (frame parent orig-x orig-y)
    (when (and frame parent (not (child-equal-p frame *current-root*)))
      (hide-all-children frame)
      (with-slots (window) frame
        (let ((lx orig-x)
              (ly orig-y))
          (readjust-all-frames-fl-size parent)
          (move-window window orig-x orig-y
                       (lambda ()
                         (let ((move-x t)
                               (move-y t))
                           (setf (frame-x frame) (x-px->fl (xlib:drawable-x window) parent)
                                 (frame-y frame) (y-px->fl (xlib:drawable-y window) parent))
                           (multiple-value-bind (x y) (xlib:query-pointer *root*)
                             (when (> x lx)
                               (let ((x-found (x-fl->px (find-edge-right frame parent) parent)))
                                 (when (< (abs (- x-found (window-x2 window))) *snap-size*)
                                   (setf (xlib:drawable-x window) (- x-found (adj-border-xy (xlib:drawable-width window) window))
                                         (frame-x frame) (x-px->fl (xlib:drawable-x window) parent)
                                         move-x nil))))
                             (when (< x lx)
                               (let ((x-found (x-fl->px (find-edge-left frame parent) parent)))
                                 (when (< (abs (- x-found (xlib:drawable-x window))) *snap-size*)
                                   (setf (xlib:drawable-x window) (adj-border-xy x-found window)
                                         (frame-x frame) (x-px->fl (xlib:drawable-x window) parent)
                                         move-x nil))))
                             (when (> y ly)
                               (let ((y-found (y-fl->px (find-edge-down frame parent) parent)))
                                 (when (< (abs (- y-found (window-y2 window))) *snap-size*)
                                   (setf (xlib:drawable-y window) (- y-found (adj-border-xy (xlib:drawable-height window) window))
                                         (frame-y frame) (y-px->fl (xlib:drawable-y window) parent)
                                         move-y nil))))
                             (when (< y ly)
                               (let ((y-found (y-fl->px (find-edge-up frame parent) parent)))
                                 (when (< (abs (- y-found (xlib:drawable-y window))) *snap-size*)
                                   (setf (xlib:drawable-y window) (adj-border-xy y-found window)
                                         (frame-y frame) (y-px->fl (xlib:drawable-y window) parent)
                                         move-y nil))))
                             (display-frame-info frame)
                             (when move-x (setf lx x))
                             (when move-y (setf ly y))
                             (values move-x move-y)))))))
      (show-all-children)))


  (defun resize-frame-constrained (frame parent orig-x orig-y)
    (when (and frame parent (not (child-equal-p frame *current-root*)))
      (hide-all-children frame)
      (with-slots (window) frame
        (let ((lx orig-x)
              (ly orig-y))
          (readjust-all-frames-fl-size parent)
          (resize-window window orig-x orig-y
                         (lambda ()
                           (let ((resize-w t)
                                 (resize-h t))
                             (multiple-value-bind (x y) (xlib:query-pointer *root*)
                               (setf (frame-w frame) (w-px->fl (anti-adj-border-wh (xlib:drawable-width window) parent) parent)
                                     (frame-h frame) (h-px->fl (anti-adj-border-wh (xlib:drawable-height window) parent) parent))
                               (when (> x lx)
                                 (let ((x-found (x-fl->px (find-edge-right frame parent) parent)))
                                   (when (< (abs (- x-found (window-x2 window))) *snap-size*)
                                     (setf (xlib:drawable-width window) (+ (xlib:drawable-width window)
                                                                           (- x-found (adj-border-xy (window-x2 window) parent)))
                                           (frame-w frame) (w-px->fl (anti-adj-border-wh (xlib:drawable-width window) parent) parent)
                                           resize-w nil))))
                               (when (> y ly)
                                 (let ((y-found (y-fl->px (find-edge-down frame parent) parent)))
                                   (when (< (abs (- y-found (window-y2 window))) *snap-size*)
                                     (setf (xlib:drawable-height window) (+ (xlib:drawable-height window)
                                                                            (- y-found (adj-border-xy (window-y2 window) parent)))
                                           (frame-h frame) (h-px->fl (anti-adj-border-wh (xlib:drawable-height window) parent) parent)
                                           resize-h nil))))
                               (display-frame-info frame)
                               (when resize-w (setf lx x))
                               (when resize-h (setf ly y))
                               (values resize-w resize-h)))))))
      (show-all-children))))
