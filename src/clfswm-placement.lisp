;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Placement functions
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2012 Philippe Brochard <pbrochard@common-lisp.net>
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

(defmacro with-placement ((placement x y &optional (width 0) (height 0) border-size) &body body)
  `(multiple-value-bind (,x ,y width height)
       ,(if border-size
            `(get-placement-values ,placement ,width ,height ,border-size)
            `(get-placement-values ,placement ,width ,height))
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
(defun root-screen-coord (border-size)
  (values (- (xlib:screen-width *screen*) (* 2 border-size))
          (- (xlib:screen-height *screen*) (* 2 border-size))))

(defmacro with-root-screen-coord ((border-size w h) &body body)
  `(multiple-value-bind (,w ,h)
       (root-screen-coord ,border-size)
     (let ((width (min width ,w))
           (height (min height ,h)))
       ,@body)))


(defun top-left-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values 0 0 width height)))

(defun top-middle-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values (truncate (/ (- w width) 2)) 0 width height)))

(defun top-right-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values (- w width) 0 width height)))



(defun middle-left-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values 0 (truncate (/ (- h height) 2)) width height)))

(defun middle-middle-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values (truncate (/ (- w width) 2)) (truncate (/ (- h height) 2)) width height)))

(defun middle-right-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values (- w width) (truncate (/ (- h height) 2)) width height)))


(defun bottom-left-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values 0 (- h height) width height)))

(defun bottom-middle-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values (truncate (/ (- w width) 2)) (- h height) width height)))

(defun bottom-right-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-root-screen-coord (border-size w h)
    (values (- w width) (- h height) width height)))



;;;
;;; Here placement: Evaluates to current position of pointer.
;;;
(defun here-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (declare (ignore border-size))
  (with-x-pointer
      (values x y width height)))


;;;
;;; Current child placement
;;;
(defun current-child-coord (border-size)
  (typecase (current-child)
    (xlib:window (values (x-drawable-x (current-child))
			 (x-drawable-y (current-child))
			 (- (x-drawable-width (current-child)) (* 2 border-size))
			 (- (x-drawable-height (current-child)) (* 2 border-size))
                         (x-drawable-border-width (current-child))))
    (frame (values (frame-rx (current-child))
		   (frame-ry (current-child))
		   (- (frame-rw (current-child)) (* 2 border-size))
		   (- (frame-rh (current-child)) (* 2 border-size))
                   (x-drawable-border-width (frame-window (current-child)))))
    (t (values 0 0 10 10 1))))

(defmacro with-current-child-coord ((border-size x y w h bds) &body body)
  "Bind x y w h bds to current child coordinates and border size"
  `(multiple-value-bind (,x ,y ,w ,h ,bds)
       (current-child-coord ,border-size)
     (let ((width (min w width))
           (height (min h height)))
       ,@body)))


(defun top-left-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x bds) (+ y bds) width height)))

(defun top-middle-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x (truncate (/ (- w width) 2)) bds) (+ y bds) width height)))

(defun top-right-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x (- w width) bds) (+ y bds) width height)))



(defun middle-left-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x bds) (+ y (truncate (/ (- h height) 2)) bds) width height)))

(defun middle-middle-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x (truncate (/ (- w width) 2)) bds) (+ y (truncate (/ (- h height) 2)) bds)
            width height)))

(defun middle-right-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x (- w width) bds) (+ y (truncate (/ (- h height) 2)) bds)
            width height)))


(defun bottom-left-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x bds) (+ y (- h height) bds) width height)))

(defun bottom-middle-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x (truncate (/ (- w width) 2)) bds) (+ y (- h height) bds) width height)))

(defun bottom-right-child-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-child-coord (border-size x y w h bds)
    (values (+ x (- w width) bds) (+ y (- h height) bds) width height)))


;;;
;;; Current root placement
;;;
(defparameter *get-current-root-fun* (lambda ()
                                       (find-root (current-child))))

(defun current-root-coord (border-size)
  (let ((root (funcall *get-current-root-fun*)))
    (values (root-x root) (root-y root)
            (- (root-w root) (* 2 border-size))
            (- (root-h root) (* 2 border-size)))))


(defmacro with-current-root-coord ((border-size x y w h) &body body)
  `(multiple-value-bind (,x ,y ,w ,h)
       (current-root-coord ,border-size)
     (let ((width (min w width))
           (height (min h height)))
       ,@body)))


(defun top-left-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values x y width height)))

(defun top-middle-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values (+ x (truncate (/ (- w width) 2))) y width height)))

(defun top-right-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values (+ x (- w width)) y width height)))



(defun middle-left-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values x (+ y (truncate (/ (- h height) 2))) width height)))

(defun middle-middle-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values (+ x (truncate (/ (- w width) 2))) (+ y (truncate (/ (- h height) 2))) width height)))

(defun middle-right-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values (+ x (- w width)) (+ y (truncate (/ (- h height) 2))) width height)))


(defun bottom-left-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values x (+ y (- h height)) width height)))

(defun bottom-middle-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values (+ x (truncate  (/ (- w width) 2))) (+ y (- h height)) width height)))

(defun bottom-right-root-placement (&optional (width 0) (height 0) (border-size *border-size*))
  (with-current-root-coord (border-size x y w h)
    (values (+ x (- w width)) (+ y (- h height)) width height)))


;;; Some tests
(defun test-some-placement (placement)
  (setf *second-mode-placement* placement
        *query-mode-placement* placement))

