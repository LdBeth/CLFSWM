;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Bindings keys and mouse
;;;
;;; Note: Mod-1 is the Alt or Meta key, Mod-2 is the Numlock key.
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
;;;| CONFIG - Bindings main mode
;;;`-----


(add-hook *binding-hook* 'init-*main-keys* 'init-*main-mouse*)

(defun set-default-main-keys ()
  (define-main-key ("F1" :mod-1) 'help-on-clfswm)
  (define-main-key ("Home" :mod-1 :control :shift) 'exit-clfswm)
  (define-main-key ("Right" :mod-1) 'select-next-brother)
  (define-main-key ("Left" :mod-1) 'select-previous-brother)
  (define-main-key ("Down" :mod-1) 'select-previous-level)
  (define-main-key ("Up" :mod-1) 'select-next-level)
  (define-main-key ("Tab" :mod-1) 'select-next-child)
  (define-main-key ("Tab" :mod-1 :shift) 'select-previous-child)
  (define-main-key ("Tab" :shift) 'switch-to-last-child)
  (define-main-key ("Return" :mod-1) 'enter-frame)
  (define-main-key ("Return" :mod-1 :shift) 'leave-frame)
  (define-main-key ("Return" :mod-5) 'frame-toggle-maximize)
  (define-main-key ("Page_Up" :mod-1) 'frame-lower-child)
  (define-main-key ("Page_Down" :mod-1) 'frame-raise-child)
  (define-main-key ("Home" :mod-1) 'switch-to-root-frame)
  (define-main-key ("Home" :mod-1 :shift) 'switch-and-select-root-frame)
  (define-main-key ("Menu") 'fast-layout-switch)
  (define-main-key ("Menu" :mod-1) 'show-all-frames-info-key)
  (define-main-key ("Menu" :shift) 'show-all-frames-info)
  (define-main-key ("Menu" :control) 'toggle-show-root-frame)
  (define-main-key (#\b :mod-1) 'banish-pointer)
  ;; Escape
  (define-main-key ("Escape" :control :shift) 'delete-focus-window)
  (define-main-key ("Escape" :mod-1 :control :shift) 'destroy-focus-window)
  (define-main-key ("Escape" :control) 'remove-focus-window)
  (define-main-key ("Escape" :shift) 'unhide-all-windows-in-current-child)
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


;; For an azery keyboard:
;;(undefine-main-multi-keys (#\1 :mod-1) (#\2 :mod-1) (#\3 :mod-1)
;;			  (#\4 :mod-1) (#\5 :mod-1) (#\6 :mod-1)
;;			  (#\7 :mod-1) (#\8 :mod-1) (#\9 :mod-1) (#\0 :mod-1))
;;(define-main-key ("ampersand" :mod-1) 'bind-or-jump 1)
;;(define-main-key ("eacute" :mod-1) 'bind-or-jump 2)
;;(define-main-key ("quotedbl" :mod-1) 'bind-or-jump 3)
;;(define-main-key ("quoteright" :mod-1) 'bind-or-jump 4)
;;(define-main-key ("parenleft" :mod-1) 'bind-or-jump 5)
;;(define-main-key ("minus" :mod-1) 'bind-or-jump 6)
;;(define-main-key ("egrave" :mod-1) 'bind-or-jump 7)
;;(define-main-key ("underscore" :mod-1) 'bind-or-jump 8)
;;(define-main-key ("ccedilla" :mod-1) 'bind-or-jump 9)
;;(define-main-key ("agrave" :mod-1) 'bind-or-jump 10)





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

(defun set-default-main-mouse ()
  (define-main-mouse (1) 'mouse-click-to-focus-and-move)
  (define-main-mouse (2) 'mouse-middle-click)
  (define-main-mouse (3) 'mouse-click-to-focus-and-resize)
  (define-main-mouse (1 :mod-1) 'mouse-click-to-focus-and-move-window)
  (define-main-mouse (3 :mod-1) 'mouse-click-to-focus-and-resize-window)
  (define-main-mouse (1 :control :mod-1) 'mouse-move-window-over-frame)
  (define-main-mouse (4) 'mouse-select-next-level)
  (define-main-mouse (5) 'mouse-select-previous-level)
  (define-main-mouse (4 :mod-1) 'mouse-enter-frame)
  (define-main-mouse (5 :mod-1) 'mouse-leave-frame))

(add-hook *binding-hook* 'set-default-main-mouse)


