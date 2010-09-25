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

(defun leave-expose-mode ()
  "Leave the expose mode"
  (throw 'exit-expose-loop nil))

(defun valid-expose-mode ()
  "Valid the expose mode"
  (throw 'exit-expose-loop t))

(defun mouse-leave-expose-mode (window root-x root-y)
  "Leave the expose mode"
  (declare (ignore window root-x root-y))
  (throw 'exit-expose-loop nil))

(defun mouse-valid-expose-mode (window root-x root-y)
  "Valid the expose mode"
  (declare (ignore window root-x root-y))
  (throw 'exit-expose-loop t))


(define-handler expose-mode :key-press (code state)
  (funcall-key-from-code *expose-keys* code state))

(define-handler expose-mode :button-press (code state window root-x root-y)
  (funcall-button-from-code *expose-mouse* code state window root-x root-y *fun-press*))



(add-hook *binding-hook* 'set-default-expose-keys)

(defun set-default-expose-keys ()
  (define-expose-key ("Escape") 'leave-expose-mode)
  (define-expose-key ("g" :control) 'leave-expose-mode)
  (define-expose-key ("Escape" :alt) 'leave-expose-mode)
  (define-expose-key ("g" :control :alt) 'leave-expose-mode)
  (define-expose-key ("Return") 'valid-expose-mode)
  (define-expose-key ("space") 'valid-expose-mode)
  (define-expose-key ("Tab") 'valid-expose-mode)
  (define-expose-key ("Right") 'speed-mouse-right)
  (define-expose-key ("Left") 'speed-mouse-left)
  (define-expose-key ("Down") 'speed-mouse-down)
  (define-expose-key ("Up") 'speed-mouse-up)
  (define-expose-key ("Left" :control) 'speed-mouse-undo)
  (define-expose-key ("Up" :control) 'speed-mouse-first-history)
  (define-expose-key ("Down" :control) 'speed-mouse-reset)
  (define-expose-mouse (1) 'mouse-valid-expose-mode)
  (define-expose-mouse (2) 'mouse-leave-expose-mode)
  (define-expose-mouse (3) 'mouse-leave-expose-mode))




(defun expose-windows-generic (first-restore-frame body)
  (xlib:warp-pointer *root* (truncate (/ (xlib:screen-width *screen*) 2))
		     (truncate (/ (xlib:screen-height *screen*) 2)))
  (with-all-frames (first-restore-frame frame)
    (setf (frame-data-slot frame :old-layout) (frame-layout frame)
	  (frame-layout frame) #'tile-space-layout))
  (show-all-children *current-root*)
  (dbg 'ici)
  (let ((grab-keyboard-p (xgrab-keyboard-p))
	(grab-pointer-p (xgrab-pointer-p)))
    (xgrab-pointer *root* 92 93)
    (unless grab-keyboard-p
      (ungrab-main-keys)
      (xgrab-keyboard *root*))
    (dbg 'ici-2)
    (when (generic-mode 'expose-mode 'exit-expose-loop
			:original-mode '(main-mode))
      (dbg 'ici-3)
      (multiple-value-bind (x y) (xlib:query-pointer *root*)
	(let* ((child (find-child-under-mouse x y))
	       (parent (find-parent-frame child *root-frame*)))
	  (when (and child parent)
	    (pfuncall body parent)
	    (focus-all-children child parent)))))
    (with-all-frames (first-restore-frame frame)
      (setf (frame-layout frame) (frame-data-slot frame :old-layout)
	    (frame-data-slot frame :old-layout) nil))
    (show-all-children *current-root*)
    (unless grab-keyboard-p
      (xungrab-keyboard)
      (grab-main-keys))
    (if grab-pointer-p
	(xgrab-pointer *root* 66 67)
	(xungrab-pointer)))
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
