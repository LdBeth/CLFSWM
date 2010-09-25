;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Expose functions - An expose like.
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2010 Philippe Brochard <hocwp@free.fr>
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

(defun expose-windows-generic (first-restore-frame func)
  (with-all-frames (first-restore-frame frame)
    (setf (frame-data-slot frame :old-layout) (frame-layout frame)
	  (frame-layout frame) #'tile-space-layout))
  (show-all-children *current-root*)
  (wait-no-key-or-button-press)
  (wait-a-key-or-button-press )
  (wait-no-key-or-button-press)
  (multiple-value-bind (x y) (xlib:query-pointer *root*)
    (let* ((child (find-child-under-mouse x y))
	   (parent (find-parent-frame child *root-frame*)))
      (when (and child parent)
	(pfuncall func parent)
	(focus-all-children child parent))))
  (with-all-frames (first-restore-frame frame)
    (setf (frame-layout frame) (frame-data-slot frame :old-layout)
	  (frame-data-slot frame :old-layout) nil))
  (show-all-children *current-root*)
  t)

(defun expose-windows-mode ()
  "Present all windows in the current frame (An expose like)"
  (stop-button-event)
  (expose-windows-generic *current-root* nil))

(defun expose-all-windows-mode ()
  "Present all windows in all frames (An expose like)"
  (stop-button-event)
  (switch-to-root-frame :show-later t)
  (expose-windows-generic *root-frame*
			  (lambda (parent)
			    (hide-all-children *root-frame*)
			    (setf *current-root* parent))))
