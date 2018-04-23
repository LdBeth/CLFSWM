;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Bindings keys and mouse
;;;
;;; Note: prefix is the Alt or Meta key, Mod-2 is the Numlock key.
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2005-2015 Philippe Brochard <pbrochard@common-lisp.net>
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
  (define-main-key ("F1" :alt) 'help-on-clfswm)
  (define-main-key ("Home" :prefix :control :shift) 'exit-clfswm)
  (define-main-key ("Right" :prefix) 'select-next-brother)
  (define-main-key ("Left" :prefix) 'select-previous-brother)
  (define-main-key ("Down" :prefix) 'select-previous-level)
  (define-main-key ("Up" :prefix) 'select-next-level)

  (define-main-key ("Right" :prefix :shift) 'select-next-brother-take-current)
  (define-main-key ("Left" :prefix :shift) 'select-previous-brother-take-current)

  (define-main-key ("Left" :control :prefix) 'select-brother-spatial-move-left)
  (define-main-key ("Right" :control :prefix) 'select-brother-spatial-move-right)
  (define-main-key ("Up" :control :prefix) 'select-brother-spatial-move-up)
  (define-main-key ("Down" :control :prefix) 'select-brother-spatial-move-down)

  (define-main-key ("Left" :control :prefix :shift) 'select-brother-spatial-move-left-take-current)
  (define-main-key ("Right" :control :prefix :shift) 'select-brother-spatial-move-right-take-current)
  (define-main-key ("Up" :control :prefix :shift) 'select-brother-spatial-move-up-take-current)
  (define-main-key ("Down" :control :prefix :shift) 'select-brother-spatial-move-down-take-current)

  (define-main-key ("Tab" :prefix) 'select-next-child)
  (define-main-key ("Tab" :prefix :shift) 'select-previous-child)
  (define-main-key ("Tab" :prefix :control) 'select-next-subchild)
  (define-main-key ("Return" :prefix) 'enter-frame)
  (define-main-key ("Return" :prefix :shift) 'leave-frame)
  (define-main-key ("Return" :prefix :control) 'frame-toggle-maximize)
  (define-main-key ("Return" :mod-5) 'frame-toggle-maximize)
  (define-main-key ("Page_Up" :prefix) 'frame-select-previous-child)
  (define-main-key ("Page_Down" :prefix) 'frame-select-next-child)
  (define-main-key ("Page_Up" :prefix :control) 'frame-lower-child)
  (define-main-key ("Page_Down" :prefix :control) 'frame-raise-child)
  (define-main-key ("Home" :prefix) 'switch-to-root-frame)
  (define-main-key ("Home" :prefix :shift) 'switch-and-select-root-frame)
  (define-main-key ("Menu") 'fastswitch-mode)
  (define-main-key ("F4") 'fastswitch-mode)
  (define-main-key ("Menu" :control) 'fastswitch-move-mode)
  (define-main-key ("Menu" :mod-5) 'expose-current-child-mode)
  (define-main-key ("F10" :alt) 'fast-layout-switch)
  (define-main-key ("F10" :shift :control) 'toggle-show-root-frame)
  (define-main-key ("F10") 'expose-windows-mode)
  (define-main-key ("F10" :control) 'expose-all-windows-mode)
  (define-main-key ("F12" :control) 'present-clfswm-terminal)
  (define-main-key ("F12" :shift) 'show-all-frames-info-key)
  (define-main-key ("F12" :shift :prefix) 'show-all-frames-info)
  (define-main-key ("b" :prefix) 'banish-pointer)
  ;; Escape
  (define-main-key ("Escape" :control) 'ask-close/kill-current-window)
  ;; Second mode
  (define-main-key (#\t :prefix) 'second-key-mode)
  (define-main-key ("less" :control) 'second-key-mode)
  ;; Bind or jump functions
  (define-main-key ("1" :prefix) 'bind-or-jump 1)
  (define-main-key ("2" :prefix) 'bind-or-jump 2)
  (define-main-key ("3" :prefix) 'bind-or-jump 3)
  (define-main-key ("4" :prefix) 'bind-or-jump 4)
  (define-main-key ("5" :prefix) 'bind-or-jump 5)
  (define-main-key ("6" :prefix) 'bind-or-jump 6)
  (define-main-key ("7" :prefix) 'bind-or-jump 7)
  (define-main-key ("8" :prefix) 'bind-or-jump 8)
  (define-main-key ("9" :prefix) 'bind-or-jump 9)
  (define-main-key ("0" :prefix) 'bind-or-jump 10))

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
  (define-main-mouse (1 :prefix) 'mouse-click-to-focus-and-move-window)
  (define-main-mouse (3 :prefix) 'mouse-click-to-focus-and-resize-window)
  (define-main-mouse (1 :prefix :shift) 'mouse-click-to-focus-and-move-window-constrained)
  (define-main-mouse (3 :prefix :shift) 'mouse-click-to-focus-and-resize-window-constrained)
  (define-main-mouse (1 :control :prefix) 'mouse-move-child-over-frame)
  (define-main-mouse (4) 'mouse-select-next-level)
  (define-main-mouse (5) 'mouse-select-previous-level)
  (define-main-mouse (4 :prefix) 'mouse-enter-frame)
  (define-main-mouse (5 :prefix) 'mouse-leave-frame)
  (define-main-mouse (4 :prefix :control) 'dec-transparency)
  (define-main-mouse (5 :prefix :control) 'inc-transparency)
  (define-main-mouse (4 :prefix :control :shift) 'dec-transparency-slow)
  (define-main-mouse (5 :prefix :control :shift) 'inc-transparency-slow))

(add-hook *binding-hook* 'set-default-main-mouse)
