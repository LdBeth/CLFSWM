;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: New window Hooks
;;;
;;;  Those hooks can be set for each frame to manage new window when they are
;;;  mapped.
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


;;; CONFIG - New window menu
;;;
;;; To add a new window hook (nw-hook):
;;;   1- define your own nw-hook
;;;   2- Define a seter function for your new hook
;;;   3- Register your new hook with register-nw-hook.



(defun set-nw-hook (hook)
  "Set the hook of the current child"
  (let ((frame (if (xlib:window-p *current-child*)
		   (find-father-frame *current-child*)
		   *current-child*)))
    (setf (frame-nw-hook frame) hook)
    (leave-second-mode)))

(defun register-nw-hook (hook)
  (setf *nw-hook-list* (append *nw-hook-list* (list hook))))


(defun default-window-placement (frame window)
  (case (window-type window)
    (:normal (adapt-child-to-father window frame))
    (t (place-window-from-hints window))))

(defun leave-if-not-frame (child)
  "Leave the child if it's not a frame"
  (when (xlib:window-p child)
    (leave-frame)
    (select-previous-level)))



;;; Default frame new window hook
(defun default-frame-nw-hook (frame window)
  "Open the next window in the current frame"
  (declare (ignore frame))
  (leave-if-not-frame *current-child*)
  (when (frame-p *current-child*)
    (pushnew window (frame-child *current-child*)))
  (default-window-placement *current-child* window))

(defun set-default-frame-nw-hook ()
  "Open the next window in the current frame"
  (set-nw-hook #'default-frame-nw-hook))

(register-nw-hook 'set-default-frame-nw-hook)


;;; Open new window in current root hook
(defun open-in-current-root-nw-hook (frame window)
  "Open the next window in the current root"
  (leave-if-not-frame *current-root*)
  (pushnew window (frame-child *current-root*))
  (setf *current-child* (first (frame-child *current-root*)))
  (default-window-placement *current-root* window)
  (setf (frame-nw-hook frame) nil))

(defun set-open-in-current-root-nw-hook ()
  "Open the next window in the current root"
  (set-nw-hook #'open-in-current-root-nw-hook))

(register-nw-hook 'set-open-in-current-root-nw-hook)


;;; Open new window in a new frame in the current root hook
(defun open-in-new-frame-in-current-root-nw-hook (frame window)
  "Open the next window in a new frame in the current root"
  (leave-if-not-frame *current-root*)
  (let ((new-frame (create-frame)))
    (pushnew new-frame (frame-child *current-root*))
    (pushnew window (frame-child new-frame))
    (setf *current-child* new-frame)
    (default-window-placement new-frame window))
  (setf (frame-nw-hook frame) nil))

(defun set-open-in-new-frame-in-current-root-nw-hook ()
  "Open the next window in a new frame in the current root"
  (set-nw-hook #'open-in-new-frame-in-current-root-nw-hook))

(register-nw-hook 'set-open-in-new-frame-in-current-root-nw-hook)


;;; Open new window in a new frame in the root frame hook
(defun open-in-new-frame-in-root-frame-nw-hook (frame window)
  "Open the next window in a new frame in the root frame"
  (let ((new-frame (create-frame)))
    (pushnew new-frame (frame-child *root-frame*))
    (pushnew window (frame-child new-frame))
    (switch-to-root-frame)
    (setf *current-child* new-frame)
    (default-window-placement new-frame window))
  (setf (frame-nw-hook frame) nil))

(defun set-open-in-new-frame-in-root-frame-nw-hook ()
  "Open the next window in a new frame in the root frame"
  (set-nw-hook #'open-in-new-frame-in-root-frame-nw-hook))

(register-nw-hook 'set-open-in-new-frame-in-root-frame-nw-hook)
