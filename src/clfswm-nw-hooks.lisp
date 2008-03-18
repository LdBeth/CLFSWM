;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: New window Hooks
;;;
;;;  Those hooks can be set for each group to manage new window when they are
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
  (let ((group (if (xlib:window-p *current-child*)
		   (find-father-group *current-child*)
		   *current-child*)))
    (setf (group-nw-hook group) hook)
    (leave-second-mode)))

(defun register-nw-hook (hook)
  (setf *nw-hook-list* (append *nw-hook-list* (list hook))))


(defun default-window-placement (group window)
  (case (window-type window)
    (:normal (adapt-child-to-father window group))
    (t (place-window-from-hints window))))

(defun leave-if-not-group (child)
  "Leave the child if it's not a group"
  (when (xlib:window-p child)
    (leave-group)
    (select-previous-level)))



;;; Default group new window hook
(defun default-group-nw-hook (group window)
  "Open the next window in the current group"
  (declare (ignore group))
  (leave-if-not-group *current-child*)
  (when (group-p *current-child*)
    (pushnew window (group-child *current-child*)))
  (default-window-placement *current-child* window))

(defun set-default-group-nw-hook ()
  "Open the next window in the current group"
  (set-nw-hook #'default-group-nw-hook))

(register-nw-hook 'set-default-group-nw-hook)


;;; Open new window in current root hook
(defun open-in-current-root-nw-hook (group window)
  "Open the next window in the current root"
  (leave-if-not-group *current-root*)
  (pushnew window (group-child *current-root*))
  (setf *current-child* (first (group-child *current-root*)))
  (default-window-placement *current-root* window)
  (setf (group-nw-hook group) nil))

(defun set-open-in-current-root-nw-hook ()
  "Open the next window in the current root"
  (set-nw-hook #'open-in-current-root-nw-hook))

(register-nw-hook 'set-open-in-current-root-nw-hook)


;;; Open new window in a new group in the current root hook
(defun open-in-new-group-in-current-root-nw-hook (group window)
  "Open the next window in a new group in the current root"
  (leave-if-not-group *current-root*)
  (let ((new-group (create-group)))
    (pushnew new-group (group-child *current-root*))
    (pushnew window (group-child new-group))
    (setf *current-child* new-group)
    (default-window-placement new-group window))
  (setf (group-nw-hook group) nil))

(defun set-open-in-new-group-in-current-root-nw-hook ()
  "Open the next window in a new group in the current root"
  (set-nw-hook #'open-in-new-group-in-current-root-nw-hook))

(register-nw-hook 'set-open-in-new-group-in-current-root-nw-hook)


;;; Open new window in a new group in the root group hook
(defun open-in-new-group-in-root-group-nw-hook (group window)
  "Open the next window in a new group in the root group"
  (let ((new-group (create-group)))
    (pushnew new-group (group-child *root-group*))
    (pushnew window (group-child new-group))
    (switch-to-root-group)
    (setf *current-child* new-group)
    (default-window-placement new-group window))
  (setf (group-nw-hook group) nil))

(defun set-open-in-new-group-in-root-group-nw-hook ()
  "Open the next window in a new group in the root group"
  (set-nw-hook #'open-in-new-group-in-root-group-nw-hook))

(register-nw-hook 'set-open-in-new-group-in-root-group-nw-hook)
