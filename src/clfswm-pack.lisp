;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Tile, pack and fill functions
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

;;;,-----
;;;| Edges functions
;;;`-----
(defun group-x2 (group)
  (+ (group-x group) (group-w group)))

(defun group-y2 (group)
  (+ (group-y group) (group-h group)))


(defun find-edge-up (current-group father)
  (let ((y-found 0))
    (dolist (group (group-child father))
      (when (and (group-p group)
		 (not (equal group current-group))
		 (<= (group-y2 group) (group-y current-group))
		 (>= (group-x2 group) (group-x current-group))
		 (<= (group-x group) (group-x2 current-group)))
	(setf y-found (max y-found (group-y2 group)))))
    y-found))
	     
(defun find-edge-down (current-group father)
  (let ((y-found 1))
    (dolist (group (group-child father))
      (when (and (group-p group)
		 (not (equal group current-group))
		 (>= (group-y group) (group-y2 current-group))
		 (>= (group-x2 group) (group-x current-group))
		 (<= (group-x group) (group-x2 current-group)))
	(setf y-found (min y-found (group-y group)))))
    y-found))
	     
(defun find-edge-right (current-group father)
  (let ((x-found 1))
    (dolist (group (group-child father))
      (when (and (group-p group)
		 (not (equal group current-group))
		 (>= (group-x group) (group-x2 current-group))
		 (>= (group-y2 group) (group-y current-group))
		 (<= (group-y group) (group-y2 current-group)))
	(setf x-found (min x-found (group-x group)))))
    x-found))
	     

(defun find-edge-left (current-group father)
  (let ((x-found 0))
    (dolist (group (group-child father))
      (when (and (group-p group)
		 (not (equal group current-group))
		 (<= (group-x2 group) (group-x current-group))
		 (>= (group-y2 group) (group-y current-group))
		 (<= (group-y group) (group-y2 current-group)))
	(setf x-found (max x-found (group-x2 group)))))
    x-found))



;;;,-----
;;;| Pack functions
;;;`-----
(defun pack-group-up (group father)
  "Pack group to up"
  (let ((y-found (find-edge-up group father)))
    (setf (group-y group) y-found)))


(defun pack-group-down (group father)
  "Pack group to down"
  (let ((y-found (find-edge-down group father)))
    (setf (group-y group) (- y-found (group-h group)))))

(defun pack-group-right (group father)
  "Pack group to right"
  (let ((x-found (find-edge-right group father)))
    (setf (group-x group) (- x-found (group-w group)))))


(defun pack-group-left (group father)
  "Pack group to left"
  (let ((x-found (find-edge-left group father)))
    (setf (group-x group) x-found)))



(defun center-group (group)
  "Center group"
  (setf (group-x group) (/ (- 1 (group-w group)) 2)
	(group-y group) (/ (- 1 (group-h group)) 2)))

;;;,-----
;;;| Fill functions
;;;`-----
(defun fill-group-up (group father)
  "Fill a group up"
  (let* ((y-found (find-edge-up group father))
	 (dy (- (group-y group) y-found)))
    (setf (group-y group) y-found
	  (group-h group) (+ (group-h group) dy))))

(defun fill-group-down (group father)
  "Fill a group down"
  (let* ((y-found (find-edge-down group father))
	 (dy (- y-found (group-y2 group))))
    (setf (group-h group) (+ (group-h group) dy))))


(defun fill-group-left (group father)
  "Fill a group left"
  (let* ((x-found (find-edge-left group father))
	 (dx (- (group-x group) x-found)))
    (setf (group-x group) x-found
	  (group-w group) (+ (group-w group) dx))))

(defun fill-group-right (group father)
  "Fill a group rigth"
  (let* ((x-found (find-edge-right group father))
	 (dx (- x-found (group-x2 group))))
    (setf (group-w group) (+ (group-w group) dx))))


;;;,-----
;;;| Lower functions
;;;`-----
(defun resize-group-down (group)
  "Resize down a group"
  (when (> (group-w group) 0.1)
    (setf (group-x group) (+ (group-x group) 0.01)
	  (group-w group) (max (- (group-w group) 0.02) 0.01)))
  (when (> (group-h group) 0.1)
    (setf (group-y group) (+ (group-y group) 0.01)
	  (group-h group) (max (- (group-h group) 0.02) 0.01))))


(defun resize-minimal-group (group)
  "Resize down a group to its minimal size"
  (dotimes (i 100)
    (resize-group-down group)))





(defun resize-half-width-left (group)
  (setf (group-w group)(/ (group-w group) 2)))


(defun resize-half-width-right (group)
  (let* ((new-size (/ (group-w group) 2))
	 (dx (- (group-w group) new-size)))
    (setf (group-w group) new-size)
    (incf (group-x group) (max dx 0))))
  

(defun resize-half-height-up (group)
  (setf (group-h group) (/ (group-h group) 2)))

(defun resize-half-height-down (group)
  (let* ((new-size (/ (group-h group) 2))
	 (dy (- (group-h group) new-size)))
    (setf (group-h group) new-size)
    (incf (group-y group) (max dy 0))))
  



;;;;;,-----
;;;;;| Explode/Implode functions
;;;;;`-----
(defun explode-group (group)
  "Create a new group for each window in group"
  (when (group-p group)
    (let ((windows (loop :for child :in (group-child group)
		      :when (xlib:window-p child)
		      :collect child)))
      (dolist (win windows)
	(add-group (create-group :child (list win)) group)
	(remove-child-in-group win group)))))


(defun explode-current-group ()
  "Create a new group for each window in group"
  (explode-group *current-child*)
  (leave-second-mode))
