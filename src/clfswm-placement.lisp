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

(defun get-placement-values (placement &optional (width 0) (height 0) (border-size *border-size*))
  (typecase placement
    (list (values-list placement))
    (function (funcall placement width height border-size))
    (symbol
     (if (fboundp placement)
	 (funcall placement width height border-size)
	 (values 0 0 width height)))
    (t (values 0 0 width height))))

(defmacro with-placement ((placement x y &optional (width 0) (height 0) (border-size *border-size*)) &body body)
  `(multiple-value-bind (,x ,y width height)
       (get-placement-values ,placement ,width ,height ,border-size)
     (declare (ignorable width height))
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
(defun top-left-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (values 0 0 width height))

(defun top-middle-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  0
          width height))

(defun top-right-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (values (- (xlib:screen-width *screen*) width (* border-size 2))
	  0
          width height))



(defun middle-left-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (values 0
	  (truncate (/ (- (xlib:screen-height *screen*) height) 2))
          width height))

(defun middle-middle-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  (truncate (/ (- (xlib:screen-height *screen*) height) 2))
          width height))

(defun middle-right-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (values (- (xlib:screen-width *screen*) width (* border-size 2))
	  (truncate (/ (- (xlib:screen-height *screen*) height) 2))
          width height))


(defun bottom-left-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (values 0
	  (- (xlib:screen-height *screen*) height (* border-size 2))
          width height))

(defun bottom-middle-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (values (truncate (/ (- (xlib:screen-width *screen*) width) 2))
	  (- (xlib:screen-height *screen*) height (* border-size 2))
          width height))

(defun bottom-right-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (values (- (xlib:screen-width *screen*) width (* border-size 2))
	  (- (xlib:screen-height *screen*) height (* border-size 2))
          width height))


;;;
;;; Current child placement
;;;
(defun current-child-coord ()
  (typecase (current-child)
    (xlib:window (values (x-drawable-x (current-child))
			 (x-drawable-y (current-child))
			 (x-drawable-width (current-child))
			 (x-drawable-height (current-child))))
    (frame (values (frame-rx (current-child))
		   (frame-ry (current-child))
		   (frame-rw (current-child))
		   (frame-rh (current-child))))
    (t (values 0 0 10 10))))

(defmacro with-current-child-coord ((x y w h) &body body)
  `(multiple-value-bind (,x ,y ,w ,h)
       (current-child-coord)
     ,@body))


(defun top-left-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x 2)
              (+ y 2)
              width height))))

(defun top-middle-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (truncate (/ (- w width) 2)))
              (+ y 2)
              width height))))

(defun top-right-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (- w width 2))
              (+ y 2)
              width height))))



(defun middle-left-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x 2)
              (+ y (truncate (/ (- h height) 2)))
              width height))))

(defun middle-middle-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (truncate (/ (- w width) 2)))
              (+ y (truncate (/ (- h height) 2)))
              width height))))

(defun middle-right-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (- w width 2))
              (+ y (truncate (/ (- h height) 2)))
              width height))))


(defun bottom-left-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x 2)
              (+ y (- h height 2))
              width height))))

(defun bottom-middle-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (truncate (/ (- w width) 2)))
              (+ y (- h height 2))
              width height))))

(defun bottom-right-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-child-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (- w width 2))
              (+ y (- h height 2))
              width height))))


;;;
;;; Current root placement
;;;
(defparameter *get-current-root-fun* (lambda ()
                                       (find-root (current-child))))

(defun current-root-coord ()
  (let ((root (funcall *get-current-root-fun*)))
    (values (root-x root) (root-y root)
            (root-w root) (root-h root))))




(defmacro with-current-root-coord ((x y w h) &body body)
  `(multiple-value-bind (,x ,y ,w ,h)
       (current-root-coord)
     ,@body))


(defun top-left-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x border-size 1)
              (+ y border-size 1)
              width height))))

(defun top-middle-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (truncate (/ (- w width) 2)))
              (+ y border-size 1)
              width height))))

(defun top-right-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (- w width border-size 1))
              (+ y border-size 1)
              width height))))



(defun middle-left-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x border-size 1)
              (+ y (truncate (/ (- h height) 2)))
              width height))))

(defun middle-middle-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
    (values (+ x (truncate (/ (- w width) 2)))
	    (+ y (truncate (/ (- h height) 2)))
            width height))))

(defun middle-right-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (- w width border-size 1))
              (+ y (truncate (/ (- h height) 2)))
              width height))))


(defun bottom-left-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x border-size 1)
              (+ y (- h height border-size 1))
              width height))))

(defun bottom-middle-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (truncate (/ (- w width) 2)))
              (+ y (- h height border-size 1))
              width height))))

(defun bottom-right-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (x y w h)
    (let ((width (min (- w 4) width))
          (height (min (- h 4) height)))
      (values (+ x (- w width border-size 1))
              (+ y (- h height border-size 1))
              width height))))

