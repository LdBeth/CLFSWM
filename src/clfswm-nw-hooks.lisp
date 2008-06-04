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


(defparameter *nw-hook-current-key* (char-code #\a))


(defun set-nw-hook (hook)
  "Set the hook of the current child"
  (let ((frame (if (xlib:window-p *current-child*)
		   (find-parent-frame *current-child*)
		   *current-child*)))
    (setf (frame-nw-hook frame) hook)
    (leave-second-mode)))

(defun register-nw-hook (hook)
  (setf *nw-hook-list* (append *nw-hook-list* (list hook)))
  (add-menu-key 'frame-nw-hook-menu (code-char *nw-hook-current-key*) hook)
  (incf *nw-hook-current-key*))


(defun default-window-placement (frame window)
  (if (managed-window-p window frame)
      (adapt-child-to-parent window frame)
      (place-window-from-hints window)))

(defun leave-if-not-frame (child)
  "Leave the child if it's not a frame"
  (when (xlib:window-p child)
    (leave-frame)
    (select-previous-level)))

(defun clear-nw-hook (frame)
  "Clear the frame new window hook"
  (setf (frame-nw-hook frame) nil))

(defun clear-all-nw-hooks ()
  "Clear all new window hooks for all frames"
  (with-all-frames (*root-frame* frame)
    (clear-nw-hook frame)))



;;; Default frame new window hook
(defun default-frame-nw-hook (frame window)
  "Open the next window in the current frame"
  (declare (ignore frame))
  (unless (string-equal (xlib:get-wm-class window) "ROX-Pinboard")
    (leave-if-not-frame *current-child*)
    (when (frame-p *current-child*)
      (pushnew window (frame-child *current-child*)))
    (default-window-placement *current-child* window)))

(defun set-default-frame-nw-hook ()
  "Open the next window in the current frame"
  (set-nw-hook #'default-frame-nw-hook))

(register-nw-hook 'set-default-frame-nw-hook)


;;; Open new window in current root hook
(defun open-in-current-root-nw-hook (frame window)
  "Open the next window in the current root"
  (leave-if-not-frame *current-root*)
  (pushnew window (frame-child *current-root*))
  (setf *current-child* (frame-selected-child *current-root*))
  (default-window-placement *current-root* window)
  (clear-nw-hook frame))

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
  (clear-nw-hook frame))

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
    (switch-to-root-frame :show-later t)
    (setf *current-child* *current-root*)
    (set-tile-space-layout-once)
    (setf *current-child* new-frame)
    (default-window-placement new-frame window))
  (clear-nw-hook frame))

(defun set-open-in-new-frame-in-root-frame-nw-hook ()
  "Open the next window in a new frame in the root frame"
  (set-nw-hook #'open-in-new-frame-in-root-frame-nw-hook))

(register-nw-hook 'set-open-in-new-frame-in-root-frame-nw-hook)


;;; Open new window in a new frame in the parent frame hook
(defun open-in-new-frame-in-parent-frame-nw-hook (frame window)
  "Open the next window in a new frame in the parent frame"
  (let ((new-frame (create-frame))
	(parent (find-parent-frame frame)))
    (when parent
      (pushnew new-frame (frame-child parent))
      (pushnew window (frame-child new-frame))
      (hide-all *current-root*)
      (setf *current-root* parent)
      (setf *current-child* new-frame)
      (default-window-placement new-frame window)
      (show-all-children *current-root*)))
  (clear-nw-hook frame))

(defun set-open-in-new-frame-in-parent-frame-nw-hook ()
  "Open the next window in a new frame in the parent frame"
  (set-nw-hook #'open-in-new-frame-in-parent-frame-nw-hook))

(register-nw-hook 'set-open-in-new-frame-in-parent-frame-nw-hook)



;;; Open a new window but leave the focus on the current child
(defun leave-focus-frame-nw-hook (frame window)
  "Open the next window in the current frame and leave the focus on the current child"
  (leave-if-not-frame *current-child*)
  (when (frame-p *current-child*)
    (with-slots (child) *current-child*
      (pushnew window child)
      (setf child (rotate-list child))))
  (default-window-placement *current-child* window)
  (clear-nw-hook frame))

(defun set-leave-focus-frame-nw-hook ()
  "Open the next window in the current frame and leave the focus on the current child"
  (set-nw-hook #'leave-focus-frame-nw-hook))

(register-nw-hook 'set-leave-focus-frame-nw-hook)
