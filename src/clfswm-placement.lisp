;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Placement functions
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

(defun get-placement-values (placement &optional (width 0) (height 0))
  (typecase placement
    (list (values (first placement)
		  (second placement)))
    (function (funcall placement width height))
    (symbol
     (if (fboundp placement)
	 (funcall placement width height)
	 (values 0 0)))
    (t (values 0 0))))

(defmacro with-placement ((placement x y &optional (width 0) (height 0)) &body body)
  `(multiple-value-bind (,x ,y)
       (get-placement-values ,placement ,width ,height)
     ,@body))

;;;; Test functions
;;
;;(defun fun-placement (&optional width height)
;;  (declare (ignore width height))
;;  (values 30 40))
;;
;;(defparameter *placement-test* (list 10 20))
;;;;(defparameter *placement-test* #'fun-placement)
;;;;(defparameter *placement-test* 'fun-placement)
;;
;;(defun toto ()
;;  (with-placement (*placement-test* x y)
;;    (format t "X=~A  Y=~A~%" x y)))

;;;
;;; Absolute placement
;;;
(defun top-left-placement (&optional (width 0) (height 0))
  (declare (ignore width height))
  (values 0 0))

(defun top-middle-placement (&optional (width 0) (height 0))
  (declare (ignore height))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  0))

(defun top-right-placement (&optional (width 0) (height 0))
  (declare (ignore height))
  (values (- (xlib:screen-width *screen*) width (* *border-size* 2))
	  0))



(defun middle-left-placement (&optional (width 0) (height 0))
  (declare (ignore width))
  (values 0
	  (truncate (/ (- (xlib:screen-height *screen*) height) 2))))

(defun middle-middle-placement (&optional (width 0) (height 0))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  (truncate (/ (- (xlib:screen-height *screen*) height) 2))))

(defun middle-right-placement (&optional (width 0) (height 0))
  (values (- (xlib:screen-width *screen*) width (* *border-size* 2))
	  (truncate (/ (- (xlib:screen-height *screen*) height) 2))))


(defun bottom-left-placement (&optional (width 0) (height 0))
  (declare (ignore width))
  (values 0
	  (- (xlib:screen-height *screen*) height (* *border-size* 2))))

(defun bottom-middle-placement (&optional (width 0) (height 0))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  (- (xlib:screen-height *screen*) height (* *border-size* 2))))

(defun bottom-right-placement (&optional (width 0) (height 0))
  (values (- (xlib:screen-width *screen*) width (* *border-size* 2))
	  (- (xlib:screen-height *screen*) height (* *border-size* 2))))


;;;
;;; Current child placement
;;;
(defun current-child-coord ()
  (typecase *current-child*
    (xlib:window (values (xlib:drawable-x *current-child*)
			 (xlib:drawable-y *current-child*)
			 (xlib:drawable-width *current-child*)
			 (xlib:drawable-height *current-child*)))
    (frame (values (frame-rx *current-child*)
		   (frame-ry *current-child*)
		   (frame-rw *current-child*)
		   (frame-rh *current-child*)))
    (t (values 0 0 10 10))))

(defmacro with-current-child-coord ((x y w h) &body body)
  `(multiple-value-bind (,x ,y ,w ,h)
       (current-child-coord)
     ,@body))


(defun top-left-child-placement (&optional (width 0) (height 0))
  (declare (ignore width height))
  (with-current-child-coord (x y w h)
    (declare (ignore w h))
    (values (+ x 2)
	    (+ y 2))))

(defun top-middle-child-placement (&optional (width 0) (height 0))
  (declare (ignore height))
  (with-current-child-coord (x y w h)
    (declare (ignore h))
    (values (+ x (truncate (/ (- w width) 2)))
	    (+ y 2))))

(defun top-right-child-placement (&optional (width 0) (height 0))
  (declare (ignore height))
  (with-current-child-coord (x y w h)
    (declare (ignore h))
    (values (+ x (- w width 2))
	    (+ y 2))))



(defun middle-left-child-placement (&optional (width 0) (height 0))
  (declare (ignore width))
  (with-current-child-coord (x y w h)
    (declare (ignore w))
    (values (+ x 2)
	    (+ y (truncate (/ (- h height) 2))))))

(defun middle-middle-child-placement (&optional (width 0) (height 0))
  (with-current-child-coord (x y w h)
    (values (+ x (truncate (/ (- w width) 2)))
	    (+ y (truncate (/ (- h height) 2))))))

(defun middle-right-child-placement (&optional (width 0) (height 0))
  (with-current-child-coord (x y w h)
    (values (+ x (- w width 2))
	    (+ y (truncate (/ (- h height) 2))))))


(defun bottom-left-child-placement (&optional (width 0) (height 0))
  (declare (ignore width))
  (with-current-child-coord (x y w h)
    (declare (ignore w))
    (values (+ x 2)
	    (+ y (- h height 2)))))

(defun bottom-middle-child-placement (&optional (width 0) (height 0))
  (with-current-child-coord (x y w h)
    (values (+ x (truncate (/ (- w width) 2)))
	    (+ y (- h height 2)))))

(defun bottom-right-child-placement (&optional (width 0) (height 0))
  (with-current-child-coord (x y w h)
    (values (+ x (- w width 2))
	    (+ y (- h height 2)))))
