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


;;; CONFIG - New window menu
;;;
;;; To add a new window hook (nw-hook):
;;;   1- define your own nw-hook
;;;   2- Define a seter function for your new hook
;;;   3- Register your new hook with register-nw-hook.


(defparameter *nw-hook-current-key* (char-code #\a))
(defparameter *permanent-nw-hook-frames* nil)


(defun set-nw-hook (hook)
  "Set the hook of the current child"
  (let ((frame (if (xlib:window-p *current-child*)
		   (find-parent-frame *current-child*)
		   *current-child*)))
    (unless (child-member frame *permanent-nw-hook-frames*)
      (setf (frame-nw-hook frame) hook)
      (leave-second-mode))))

(defun register-nw-hook (hook)
  (add-menu-key 'frame-nw-hook-menu (code-char *nw-hook-current-key*) hook)
  (incf *nw-hook-current-key*))


(defun default-window-placement (frame window)
  (if (managed-window-p window frame)
      (adapt-child-to-parent window frame)
      (place-window-from-hints window)))

(defun leave-if-not-frame (child)
  "Leave the child if it's not a frame"
  (unless (frame-p child)
    (leave-frame)
    (select-previous-level)))

(defun clear-nw-hook (frame)
  "Clear the frame new window hook"
  (unless (child-member frame *permanent-nw-hook-frames*)
    (setf (frame-nw-hook frame) nil)))


(defun clear-all-nw-hooks ()
  "Clear all new window hooks for all frames"
  (with-all-frames (*root-frame* frame)
    (clear-nw-hook frame)))


(defun make-permanent-nw-hook-frame (frame)
  "Prevent to add or delete a new window hook for this frame"
  (when (frame-p frame)
    (push frame *permanent-nw-hook-frames*)))


;;; Default frame new window hook
(defun default-frame-nw-hook (frame window)
  "Open the next window in the current frame"
  (declare (ignore frame))
  (leave-if-not-frame *current-child*)
  (when (frame-p *current-child*)
    (pushnew window (frame-child *current-child*)))
  (default-window-placement *current-child* window)
  t)

(defun set-default-frame-nw-hook ()
  "Open the next window in the current frame"
  (set-nw-hook #'default-frame-nw-hook))

(register-nw-hook 'set-default-frame-nw-hook)


;;; Open new window in current root hook
(defun open-in-current-root-nw-hook (frame window)
  "Open the next window in the current root"
  (clear-nw-hook frame)
  (leave-if-not-frame *current-root*)
  (pushnew window (frame-child *current-root*))
  (setf *current-child* (frame-selected-child *current-root*))
  (default-window-placement *current-root* window)
  t)

(defun set-open-in-current-root-nw-hook ()
  "Open the next window in the current root"
  (set-nw-hook #'open-in-current-root-nw-hook))

(register-nw-hook 'set-open-in-current-root-nw-hook)


;;; Open new window in a new frame in the current root hook
(defun open-in-new-frame-in-current-root-nw-hook (frame window)
  "Open the next window in a new frame in the current root"
  (clear-nw-hook frame)
  (leave-if-not-frame *current-root*)
  (let ((new-frame (create-frame)))
    (pushnew new-frame (frame-child *current-root*))
    (pushnew window (frame-child new-frame))
    (setf *current-child* new-frame)
    (default-window-placement new-frame window))
  t)

(defun set-open-in-new-frame-in-current-root-nw-hook ()
  "Open the next window in a new frame in the current root"
  (set-nw-hook #'open-in-new-frame-in-current-root-nw-hook))

(register-nw-hook 'set-open-in-new-frame-in-current-root-nw-hook)


;;; Open new window in a new frame in the root frame hook
(defun open-in-new-frame-in-root-frame-nw-hook (frame window)
  "Open the next window in a new frame in the root frame"
  (clear-nw-hook frame)
  (let ((new-frame (create-frame)))
    (pushnew new-frame (frame-child *root-frame*))
    (pushnew window (frame-child new-frame))
    (switch-to-root-frame :show-later t)
    (setf *current-child* *current-root*)
    (set-layout-once #'tile-space-layout)
    (setf *current-child* new-frame)
    (default-window-placement new-frame window))
  t)

(defun set-open-in-new-frame-in-root-frame-nw-hook ()
  "Open the next window in a new frame in the root frame"
  (set-nw-hook #'open-in-new-frame-in-root-frame-nw-hook))

(register-nw-hook 'set-open-in-new-frame-in-root-frame-nw-hook)


;;; Open new window in a new frame in the parent frame hook
(defun open-in-new-frame-in-parent-frame-nw-hook (frame window)
  "Open the next window in a new frame in the parent frame"
  (clear-nw-hook frame)
  (let ((new-frame (create-frame))
	(parent (find-parent-frame frame)))
    (when parent
      (pushnew new-frame (frame-child parent))
      (pushnew window (frame-child new-frame))
      (setf *current-root* parent
	    *current-child* parent)
      (set-layout-once #'tile-space-layout)
      (setf *current-child* new-frame)
      (default-window-placement new-frame window)
      (show-all-children t)
      t)))


(defun set-open-in-new-frame-in-parent-frame-nw-hook ()
  "Open the next window in a new frame in the parent frame"
  (set-nw-hook #'open-in-new-frame-in-parent-frame-nw-hook))

(register-nw-hook 'set-open-in-new-frame-in-parent-frame-nw-hook)



;;; Open a new window but leave the focus on the current child
(defun leave-focus-frame-nw-hook (frame window)
  "Open the next window in the current frame and leave the focus on the current child"
  (clear-nw-hook frame)
  (leave-if-not-frame *current-child*)
  (when (frame-p *current-child*)
    (with-slots (child) *current-child*
      (pushnew window child)
      (setf child (rotate-list child))))
  (default-window-placement *current-child* window)
  t)

(defun set-leave-focus-frame-nw-hook ()
  "Open the next window in the current frame and leave the focus on the current child"
  (set-nw-hook #'leave-focus-frame-nw-hook))

(register-nw-hook 'set-leave-focus-frame-nw-hook)





(defun nw-hook-open-in-frame (window frame)
  (when (frame-p frame)
    (pushnew window (frame-child frame))
    (unless (find-child frame *current-root*)
      (setf *current-root* frame))
    (setf *current-child* frame)
    (focus-all-children window frame)
    (default-window-placement frame window)
    (show-all-children t)
    t))

;;; Open a new window in a named frame
(defun named-frame-nw-hook (frame window)
  (clear-nw-hook frame)
  (let* ((frame-name (ask-frame-name "Open the next window in frame named:"))
	 (new-frame (find-frame-by-name frame-name)))
    (nw-hook-open-in-frame window new-frame))
  t)

(defun set-named-frame-nw-hook ()
  "Open the next window in a named frame"
  (set-nw-hook #'named-frame-nw-hook))

(register-nw-hook 'set-named-frame-nw-hook)


;;; Open a new window in a numbered frame
(defun numbered-frame-nw-hook (frame window)
  (clear-nw-hook frame)
  (let ((new-frame (find-frame-by-number (query-number "Open a new frame in the group numbered:"))))
    (nw-hook-open-in-frame window new-frame))
  t)

(defun set-numbered-frame-nw-hook ()
  "Open the next window in a numbered frame"
  (set-nw-hook #'numbered-frame-nw-hook))

(register-nw-hook 'set-numbered-frame-nw-hook)


;;; Absorb window.
;;; The frame absorb the new window if it match the nw-absorb-test
;;; frame data slot.
(defun absorb-window-nw-hook (frame window)
  (let ((absorb-nw-test (frame-data-slot frame :nw-absorb-test)))
    (when (and absorb-nw-test
	       (funcall absorb-nw-test window))
      (pushnew window (frame-child frame))
      (unless *in-process-existing-windows*
	(unless (find-child frame *current-root*)
	  (setf *current-root* frame))
	(setf *current-child* frame)
	(focus-all-children window frame)
	(default-window-placement frame window)
	(show-all-children t))
      (throw 'nw-hook-loop t)))
  nil)

(defun set-absorb-window-nw-hook ()
  "Open the window in this frame if it match nw-absorb-test"
  (set-nw-hook #'absorb-window-nw-hook))

(register-nw-hook 'set-absorb-window-nw-hook)


(defun nw-absorb-test-class (class-string)
  (lambda (c)
    (and (xlib:window-p c)
	 (string-equal (xlib:get-wm-class c) class-string))))

