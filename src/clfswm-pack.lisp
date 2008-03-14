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
;;;| Tile functions
;;;`-----
(defun tile-workspace-vertically (workspace)
  "Tile a workspace vertically"
  (let* ((len (max (length (workspace-group-list workspace)) 1))
	 (n (ceiling (sqrt len)))
	 (dx (/ (xlib:screen-width *screen*) n))
	 (dy (/ (xlib:screen-height *screen*) (ceiling (/ len n)))))
    (loop for group in (workspace-group-list workspace)
       for i from 0 do
       (setf (group-x group) (1+ (truncate (* (mod i n) dx)))
	     (group-y group) (1+ (truncate (* (truncate (/ i n)) dy)))
	     (group-width group) (- (truncate dx) 2)
	     (group-height group) (- (truncate dy) 2)))))


(defun tile-current-workspace-vertically ()
  "Tile the current workspace vertically"
  (minimize-group (current-group))
  (tile-workspace-vertically (current-workspace))
  (show-all-windows-in-workspace (current-workspace)))



(defun tile-workspace-horizontally (workspace)
  "Tile a workspace horizontally"
  (let* ((len (max (length (workspace-group-list workspace)) 1))
	 (n (ceiling (sqrt len)))
	 (dx (/ (xlib:screen-width *screen*) (ceiling (/ len n))))
	 (dy (/ (xlib:screen-height *screen*) n)))
    (loop for group in (workspace-group-list workspace)
       for i from 0 do
       (setf (group-x group) (1+ (truncate (* (truncate (/ i n)) dx)))
	     (group-y group) (1+ (truncate (* (mod i n) dy)))
	     (group-width group) (- (truncate dx) 2)
	     (group-height group) (- (truncate dy) 2)))))


(defun tile-current-workspace-horizontally ()
  "Tile the current workspace horizontally"
  (minimize-group (current-group))
  (tile-workspace-horizontally (current-workspace))
  (show-all-windows-in-workspace (current-workspace)))


(defun tile-workspace-right (workspace)
  "Tile workspace with the current window on the left and others on the right"
  (let ((len (length (workspace-group-list workspace)))
	(group (first (workspace-group-list workspace))))
    (if (<= len 1)
	(setf (group-x group) 0
	      (group-y group) 0
	      (group-width group) (xlib:screen-width *screen*)
	      (group-height group) (xlib:screen-height *screen*))
	(let ((dy (/ (xlib:screen-height *screen*) (1- len))))
	  (setf (group-x group) 1
		(group-y group) 1
		(group-width group) (- (xlib:screen-width *screen*) *tile-border-size* 1)
		(group-height group) (- (xlib:screen-height *screen*) 1))
	  (loop :for i :from 0
	     :for g :in (rest (workspace-group-list workspace))
	     :do (setf (group-x g) (- (xlib:screen-width *screen*) *tile-border-size* -1)
		       (group-y g) (truncate (* i dy))
		       (group-width g) (- *tile-border-size* 2)
		       (group-height g) (truncate (- dy 1))))))))

(defun tile-workspace-left (workspace)
  "Tile workspace with the current window on the right and others on the left"
  (let ((len (length (workspace-group-list workspace)))
	(group (first (workspace-group-list workspace))))
    (if (<= len 1)
	(setf (group-x group) 0
	      (group-y group) 0
	      (group-width group) (xlib:screen-width *screen*)
	      (group-height group) (xlib:screen-height *screen*))
	(let ((dy (/ (xlib:screen-height *screen*) (1- len))))
	  (setf (group-x group) *tile-border-size*
		(group-y group) 1
		(group-width group) (- (xlib:screen-width *screen*) *tile-border-size* 1)
		(group-height group) (- (xlib:screen-height *screen*) 1))
	  (loop :for i :from 0
	     :for g :in (rest (workspace-group-list workspace))
	     :do (setf (group-x g) 0
		       (group-y g) (truncate (* i dy))
		       (group-width g) (- *tile-border-size* 2)
		       (group-height g) (truncate (- dy 1))))))))


(defun tile-workspace-top (workspace)
  "Tile workspace with the current window on the bottom and others on the top"
  (let ((len (length (workspace-group-list workspace)))
	(group (first (workspace-group-list workspace))))
    (if (<= len 1)
	(setf (group-x group) 0
	      (group-y group) 0
	      (group-width group) (xlib:screen-width *screen*)
	      (group-height group) (xlib:screen-height *screen*))
	(let ((dx (/ (xlib:screen-width *screen*) (1- len))))
	  (setf (group-x group) 1
		(group-y group) *tile-border-size*
		(group-width group) (- (xlib:screen-width *screen*) 1)
		(group-height group) (- (xlib:screen-height *screen*) *tile-border-size* 1))
	  (loop :for i :from 0
	     :for g :in (rest (workspace-group-list workspace))
	     :do (setf (group-x g) (truncate (* i dx))
		       (group-y g) 0
		       (group-width g) (truncate (- dx 1))
		       (group-height g) (- *tile-border-size* 2)))))))

(defun tile-workspace-bottom (workspace)
  "Tile workspace with the current window on the top and others on the bottom"
  (let ((len (length (workspace-group-list workspace)))
	(group (first (workspace-group-list workspace))))
    (if (<= len 1)
	(setf (group-x group) 0
	      (group-y group) 0
	      (group-width group) (xlib:screen-width *screen*)
	      (group-height group) (xlib:screen-height *screen*))
	(let ((dx (/ (xlib:screen-width *screen*) (1- len))))
	  (setf (group-x group) 1
		(group-y group) 1
		(group-width group) (- (xlib:screen-width *screen*) 1)
		(group-height group) (- (xlib:screen-height *screen*) *tile-border-size* 1))
	  (loop :for i :from 0
	     :for g :in (rest (workspace-group-list workspace))
	     :do (setf (group-x g) (truncate (* i dx))
		       (group-y g) (- (xlib:screen-height *screen*) *tile-border-size* -1)
		       (group-width g) (truncate (- dx 1))
		       (group-height g) (- *tile-border-size* 2)))))))


(defun tile-current-workspace-to ()
  "Tile the current workspace with the current window on one side and others on the other"
  (funcall *tile-workspace-function* (current-workspace))
  (show-all-windows-in-workspace (current-workspace)))


(defun reconfigure-tile-workspace ()
  "Reconfigure the workspace tiling for the current session"
  (let ((method (loop :for m = (intern (string-upcase
					(query-string "Workspace tiling method (R)ight, (L)eft, (T)op, (B)ottom:"))
				       :keyword)
		   :when (member m '(:r :l :t :b)) :return m))
	(size (loop :for s = (parse-integer (query-string "Workspace tiling border size"
							  (format nil "~A" *tile-border-size*))
					    :junk-allowed t)
		 :when (numberp s) :return s)))
    (setf *tile-workspace-function* (case method
				      (:r 'tile-workspace-right)
				      (:l 'tile-workspace-left)
				      (:t 'tile-workspace-top)
				      (:b 'tile-workspace-bottom))
	  *tile-border-size* size)))




;;;,-----
;;;| Edges functions
;;;`-----
(defun group-x2 (group)
  (+ (group-x group) (group-width group)))

(defun group-y2 (group)
  (+ (group-y group) (group-height group)))


(defun find-edge-up (current-group workspace)
  (let ((y-found 0))
    (dolist (group (workspace-group-list workspace))
      (when (and (not (equal group current-group))
		 (<= (group-y2 group) (group-y current-group))
		 (>= (group-x2 group) (group-x current-group))
		 (<= (group-x group) (group-x2 current-group)))
	(setf y-found (max y-found (+ (group-y2 group) 2)))))
    y-found))
	     
(defun find-edge-down (current-group workspace)
  (let ((y-found (xlib:screen-height *screen*)))
    (dolist (group (workspace-group-list workspace))
      (when (and (not (equal group current-group))
		 (>= (group-y group) (group-y2 current-group))
		 (>= (group-x2 group) (group-x current-group))
		 (<= (group-x group) (group-x2 current-group)))
	(setf y-found (min y-found (- (group-y group) 2)))))
    y-found))
	     
(defun find-edge-right (current-group workspace)
  (let ((x-found (xlib:screen-width *screen*)))
    (dolist (group (workspace-group-list workspace))
      (when (and (not (equal group current-group))
		 (>= (group-x group) (group-x2 current-group))
		 (>= (group-y2 group) (group-y current-group))
		 (<= (group-y group) (group-y2 current-group)))
	(setf x-found (min x-found (- (group-x group) 2)))))
    x-found))
	     

(defun find-edge-left (current-group workspace)
  (let ((x-found 0))
    (dolist (group (workspace-group-list workspace))
      (when (and (not (equal group current-group))
		 (<= (group-x2 group) (group-x current-group))
		 (>= (group-y2 group) (group-y current-group))
		 (<= (group-y group) (group-y2 current-group)))
	(setf x-found (max x-found (+ (group-x2 group) 2)))))
    x-found))



;;;,-----
;;;| Pack functions
;;;`-----



(defun pack-group-up (workspace group)
  "Pack group to up"
  (let ((y-found (find-edge-up group workspace)))
    (setf (group-y group) y-found)))


(defun pack-group-down (workspace group)
  "Pack group to down"
  (let ((y-found (find-edge-down group workspace)))
    (setf (group-y group) (- y-found (group-height group)))))

(defun pack-group-right (workspace group)
  "Pack group to right"
  (let ((x-found (find-edge-right group workspace)))
    (setf (group-x group) (- x-found (group-width group)))))


(defun pack-group-left (workspace group)
  "Pack group to left"
  (let ((x-found (find-edge-left group workspace)))
    (setf (group-x group) x-found)))




(defun pack-current-group-up ()
  "Pack current group to up"
  (pack-group-up (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))


(defun pack-current-group-down ()
  "Pack current group to down"
  (pack-group-down (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))

(defun pack-current-group-right ()
  "Pack current group to right"
  (pack-group-right (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))


(defun pack-current-group-left ()
  "Pack current group to left"
  (pack-group-left (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))


(defun center-group (group)
  "Center group"
  (setf (group-x group) (truncate (/ (- (xlib:screen-width *screen*) (group-width group)) 2))
	(group-y group) (truncate (/ (- (xlib:screen-height *screen*) (group-height group)) 2))))

(defun center-current-group ()
  "Center the current group"
  (center-group (current-group))
  (show-all-windows-in-workspace (current-workspace)))

;;;,-----
;;;| Fill functions
;;;`-----


(defun fill-group-up (workspace group)
  "Fill a group up"
  (let* ((y-found (find-edge-up group workspace))
	 (dy (- (group-y group) y-found)))
    (setf (group-y group) y-found
	  (group-height group) (+ (group-height group) dy))))

(defun fill-group-down (workspace group)
  "Fill a group down"
  (let* ((y-found (find-edge-down group workspace))
	 (dy (- y-found (group-y2 group))))
    (setf (group-height group) (+ (group-height group) dy))))


(defun fill-group-left (workspace group)
  "Fill a group left"
  (let* ((x-found (find-edge-left group workspace))
	 (dx (- (group-x group) x-found)))
    (setf (group-x group) x-found
	  (group-width group) (+ (group-width group) dx))))

(defun fill-group-right (workspace group)
  "Fill a group rigth"
  (let* ((x-found (find-edge-right group workspace))
	 (dx (- x-found (group-x2 group))))
    (setf (group-width group) (+ (group-width group) dx))))


(defun fill-current-group-up ()
  "Fill the current group up"
  (fill-group-up (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))

(defun fill-current-group-down ()
  "Fill the current group down"
  (fill-group-down (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))


(defun fill-current-group-left ()
  "Fill the current group left"
  (fill-group-left (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))

(defun fill-current-group-right ()
  "Fill the current group rigth"
  (fill-group-right (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))



;;;,-----
;;;| Lower functions
;;;`-----

(defun resize-down-group (group)
  "Resize down a group"
  (when (> (group-width group) 100)
    (setf (group-x group) (+ (group-x group) 10)
	  (group-width group) (max (- (group-width group) 20))))
  (when (> (group-height group) 100)
    (setf (group-y group) (+ (group-y group) 10)
	  (group-height group) (max (- (group-height group) 20)))))


(defun resize-minimal-group (group)
  "Resize down a group to its minimal size"
  (loop while (> (group-width group) 100) do
       (setf (group-x group) (+ (group-x group) 10)
	     (group-width group) (max (- (group-width group) 20))))
  (loop while (> (group-height group) 100) do
       (setf (group-y group) (+ (group-y group) 10)
	     (group-height group) (max (- (group-height group) 20)))))



(defun resize-down-current-group ()
  "Resize down the current group"
  (resize-down-group (current-group))
  (show-all-windows-in-workspace (current-workspace)))


(defun resize-minimal-current-group ()
  "Resize down the current group to its minimal size"
  (resize-minimal-group (current-group))
  (show-all-windows-in-workspace (current-workspace)))




(defun resize-half-width-left (group)
  (setf (group-width group)
	(max (truncate (/ (group-width group) 2))
	     100)))

(defun resize-half-width-right (group)
  (let* ((new-size (max (truncate (/ (group-width group) 2)) 100))
	 (dx (- (group-width group) new-size)))
    (setf (group-width group) new-size)
    (incf (group-x group) (max dx 0))))
  

(defun resize-half-height-up (group)
  (setf (group-height group)
	(max (truncate (/ (group-height group) 2))
	     100)))

(defun resize-half-height-down (group)
  (let* ((new-size (max (truncate (/ (group-height group) 2)) 100))
	 (dy (- (group-height group) new-size)))
    (setf (group-height group) new-size)
    (incf (group-y group) (max dy 0))))
  



(defun resize-half-width-left-current-group ()
  "Resize the current group to its half width to left"
  (resize-half-width-left (current-group))
  (show-all-windows-in-workspace (current-workspace)))

(defun resize-half-width-right-current-group ()
  "Resize the current group to its half width to right"
  (resize-half-width-right (current-group))
  (show-all-windows-in-workspace (current-workspace)))


(defun resize-half-height-up-current-group ()
  "Resize the current group to its half height to up"
  (resize-half-height-up (current-group))
  (show-all-windows-in-workspace (current-workspace)))

(defun resize-half-height-down-current-group ()
  "Resize the current group to its half height to down"
  (resize-half-height-down (current-group))
  (show-all-windows-in-workspace (current-workspace)))



;;;,-----
;;;| Explode/Implode functions
;;;`-----
(defun explode-group (workspace group)
  "Create a new group for each window in group"
  (dolist (w (rest (group-window-list group)))
    (add-group-in-workspace (copy-group *default-group*) workspace)
    (add-window-in-group w (first (workspace-group-list workspace)))
    (remove-window-in-group w group)))

(defun implode-group (workspace)
  "Move all windows in workspace to one group and remove other groups"
  (dolist (g (rest (workspace-group-list workspace)))
    (dolist (w (group-window-list g))
      (add-window-in-group w (first (workspace-group-list workspace)))
      (remove-window-in-group w g))
    (remove-group-in-workspace g workspace)))



(defun explode-current-group ()
  "Create a new group for each window in the current group"
  (explode-group (current-workspace) (current-group))
  (show-all-windows-in-workspace (current-workspace)))


(defun implode-current-group ()
  "Move all windows in the current workspace to one group and remove other groups"
  (implode-group (current-workspace))
  (show-all-windows-in-workspace (current-workspace)))

