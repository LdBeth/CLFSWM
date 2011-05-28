;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Bindings keys and mouse
;;;
;;; Note: Mod-1 is the Alt or Meta key, Mod-2 is the Numlock key.
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

;;;,-----
;;;| CONFIG - Bindings main mode
;;;`-----


(add-hook *binding-hook* 'init-*main-keys* 'init-*main-mouse*)


(defun help-on-clfswm ()
  "Open the help and info window"
  (open-menu (find-menu 'help-menu)))


(defun set-default-main-keys ()
  (define-main-key ("F1" :mod-1) 'help-on-clfswm)
  (define-main-key ("Home" :mod-1 :control :shift) 'exit-clfswm)
  (define-main-key ("Right" :mod-1) 'select-next-brother)
  (define-main-key ("Left" :mod-1) 'select-previous-brother)
  (define-main-key ("Down" :mod-1) 'select-previous-level)
  (define-main-key ("Up" :mod-1) 'select-next-level)
  (define-main-key ("Left" :control :mod-1) 'select-brother-spatial-move-left)
  (define-main-key ("Right" :control :mod-1) 'select-brother-spatial-move-right)
  (define-main-key ("Up" :control :mod-1) 'select-brother-spatial-move-up)
  (define-main-key ("Down" :control :mod-1) 'select-brother-spatial-move-down)
  (define-main-key ("Tab" :mod-1) 'select-next-child)
  (define-main-key ("Tab" :mod-1 :shift) 'select-previous-child)
  (define-main-key ("Tab" :mod-1 :control) 'select-next-subchild)
  (define-main-key ("Return" :mod-1) 'enter-frame)
  (define-main-key ("Return" :mod-1 :shift) 'leave-frame)
  (define-main-key ("Return" :mod-5) 'frame-toggle-maximize)
  (define-main-key ("Page_Up" :mod-1) 'frame-select-previous-child)
  (define-main-key ("Page_Down" :mod-1) 'frame-select-next-child)
  (define-main-key ("Page_Up" :mod-1 :control) 'frame-lower-child)
  (define-main-key ("Page_Down" :mod-1 :control) 'frame-raise-child)
  (define-main-key ("Home" :mod-1) 'switch-to-root-frame)
  (define-main-key ("Home" :mod-1 :shift) 'switch-and-select-root-frame)
  (define-main-key ("F10" :mod-1) 'fast-layout-switch)
  (define-main-key ("F10" :shift :control) 'toggle-show-root-frame)
  (define-main-key ("F10") 'expose-windows-current-child-mode)
  (define-main-key ("F10" :control) 'expose-windows-mode)
  (define-main-key ("F10" :control :shift) 'expose-all-windows-mode)
  (define-main-key ("L2" :control) 'present-clfswm-terminal)
  (define-main-key ("L2" :shift) 'show-all-frames-info-key)
  (define-main-key ("L2" :shift :mod-1) 'show-all-frames-info)
  (define-main-key ("b" :mod-1) 'banish-pointer)
  ;; Escape
  (define-main-key ("Escape" :control) 'ask-close/kill-current-window)
  ;; Second mode
  (define-main-key (#\t :mod-1) 'second-key-mode)
  (define-main-key ("less" :control) 'second-key-mode)
  ;; Bind or jump functions
  (define-main-key ("1" :mod-1) 'bind-or-jump 1)
  (define-main-key ("2" :mod-1) 'bind-or-jump 2)
  (define-main-key ("3" :mod-1) 'bind-or-jump 3)
  (define-main-key ("4" :mod-1) 'bind-or-jump 4)
  (define-main-key ("5" :mod-1) 'bind-or-jump 5)
  (define-main-key ("6" :mod-1) 'bind-or-jump 6)
  (define-main-key ("7" :mod-1) 'bind-or-jump 7)
  (define-main-key ("8" :mod-1) 'bind-or-jump 8)
  (define-main-key ("9" :mod-1) 'bind-or-jump 9)
  (define-main-key ("0" :mod-1) 'bind-or-jump 10))

(add-hook *binding-hook* 'set-default-main-keys)





;;; Mouse actions
(defun mouse-click-to-focus-and-move-window (window root-x root-y)
  "Move and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (stop-button-event)
  (mouse-focus-move/resize-generic root-x root-y #'move-frame t))


(defun mouse-click-to-focus-and-resize-window (window root-x root-y)
  "Resize and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (stop-button-event)
  (mouse-focus-move/resize-generic root-x root-y #'resize-frame t))


(defun mouse-click-to-focus-and-move-window-constrained (window root-x root-y)
  "Move (constrained by other frames) and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (stop-button-event)
  (mouse-focus-move/resize-generic root-x root-y #'move-frame-constrained t))


(defun mouse-click-to-focus-and-resize-window-constrained (window root-x root-y)
  "Resize (constrained by other frames) and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (stop-button-event)
  (mouse-focus-move/resize-generic root-x root-y #'resize-frame-constrained t))



(defun set-default-main-mouse ()
  (define-main-mouse (1) 'mouse-click-to-focus-and-move)
  (define-main-mouse (2) 'mouse-middle-click)
  (define-main-mouse (3) 'mouse-click-to-focus-and-resize)
  (define-main-mouse (1 :mod-1) 'mouse-click-to-focus-and-move-window)
  (define-main-mouse (3 :mod-1) 'mouse-click-to-focus-and-resize-window)
  (define-main-mouse (1 :mod-1 :shift) 'mouse-click-to-focus-and-move-window-constrained)
  (define-main-mouse (3 :mod-1 :shift) 'mouse-click-to-focus-and-resize-window-constrained)
  (define-main-mouse (1 :control :mod-1) 'mouse-move-child-over-frame)
  (define-main-mouse (4) 'mouse-select-next-level)
  (define-main-mouse (5) 'mouse-select-previous-level)
  (define-main-mouse (4 :mod-1) 'mouse-enter-frame)
  (define-main-mouse (5 :mod-1) 'mouse-leave-frame))

(add-hook *binding-hook* 'set-default-main-mouse)


