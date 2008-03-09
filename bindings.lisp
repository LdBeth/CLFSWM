;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; #Date#: Sat Mar  8 21:13:30 2008
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


(define-main-key ("F1" :mod-1) 'help-on-clfswm)

(defun quit-clfswm ()
  "Quit clfswm"
  (throw 'exit-main-loop nil))

(define-main-key ("Home" :mod-1 :control :shift) 'quit-clfswm)

(define-main-key ("Right" :mod-1) 'select-next-brother)
(define-main-key ("Left" :mod-1) 'select-previous-brother)

(define-main-key ("Down" :mod-1) 'select-next-level)
(define-main-key ("Up" :mod-1) 'select-previous-level)

(define-main-key ("Tab" :mod-1) 'select-next-child)
(define-main-key ("Tab" :mod-1 :shift) 'select-previous-child)

(define-main-key ("Return" :mod-1) 'enter-group)
(define-main-key ("Return" :mod-1 :shift) 'leave-group)

(define-main-key ("Home" :mod-1) 'switch-to-root-group)
(define-main-key ("Home" :mod-1 :shift) 'switch-and-select-root-group)

(define-main-key ("Menu") 'toggle-show-root-group)

(define-main-key (#\b :mod-1) 'banish-pointer)


;;;; Escape
(define-main-key ("Escape" :control :shift) 'delete-focus-window)
(define-main-key ("Escape" :mod-1 :control :shift) 'destroy-focus-window)
(define-main-key ("Escape" :control) 'remove-focus-window)
(define-main-key ("Escape" :shift) 'unhide-all-windows-in-current-child)


(define-main-key (#\t :mod-1) 'second-key-mode)
(define-main-key ("less" :control) 'second-key-mode)






;;; Mouse actions

(defun mouse-click-to-focus (window root-x root-y)
  "Focus the current group or the current window father"
  (declare (ignore root-x root-y))
  (let ((to-replay t)
	(child window)
	(father (find-father-group window *current-root*)))
    (unless father
      (setf child (find-group-window window *current-root*)
	    father (find-father-group child *current-root*)))
    (when (and child father (focus-all-childs child father))
      (show-all-childs)
      (setf to-replay nil))
    (if to-replay
	(replay-button-event)
	(stop-button-event))))


(defun test-mouse-binding (window root-x root-y)
  (dbg window root-x root-y))



(defun mouse-select-next-level (window root-x root-y)
  "Select the next level in group"
  (declare (ignore root-x root-y))
  (let ((group (find-group-window window)))
    (when (or group (xlib:window-equal window *root*))
      (select-next-level))
    (replay-button-event)))



(defun mouse-select-previous-level (window root-x root-y)
  "Select the previous level in group"
  (declare (ignore root-x root-y))
  (let ((group (find-group-window window)))
    (when (or group (xlib:window-equal window *root*))
      (select-previous-level))
    (replay-button-event)))



(defun mouse-enter-group (window root-x root-y)
  "Enter in the selected group - ie make it the root group"
  (declare (ignore root-x root-y))
  (let ((group (find-group-window window)))
    (when (or group (xlib:window-equal window *root*))
      (enter-group))
    (replay-button-event)))



(defun mouse-leave-group (window root-x root-y)
  "Leave the selected group - ie make its father the root group"
  (declare (ignore root-x root-y))
  (let ((group (find-group-window window)))
    (when (or group (xlib:window-equal window *root*))
      (leave-group))
    (replay-button-event)))


(define-main-mouse (1) nil 'mouse-click-to-focus)


(define-main-mouse (4) 'mouse-select-next-level)
(define-main-mouse (5) 'mouse-select-previous-level)

(define-main-mouse (4 :mod-1) 'mouse-enter-group)
(define-main-mouse (5 :mod-1) 'mouse-leave-group)

;;(define-main-mouse (1) 'handle-click-to-focus 'test-mouse-binding)
;;(define-main-mouse ('motion) 'test-mouse-binding)


;;(define-main-key ("a") (lambda ()
;;			 (dbg 'key-a)
;;			 (show-all-childs *root-group*)))
;;
;;(define-main-key ("b") (lambda ()
;;			 (dbg 'key-b)
;;			   (let* ((window (xlib:create-window :parent *root*
;;							 :x 300
;;							 :y 200
;;							 :width 400
;;							 :height 300
;;							 :background (get-color "Black")
;;							 :colormap (xlib:screen-default-colormap *screen*)
;;							 :border-width 1
;;							 :border (get-color "Red")
;;							 :class :input-output
;;							 :event-mask '(:exposure)))
;;				  (gc (xlib:create-gcontext :drawable window
;;						       :foreground (get-color "Green")
;;						       :background (get-color "Red")
;;						       :font *default-font*
;;						       :line-style :solid)))
;;			     (xlib:map-window window)
;;			     (draw-line window gc 10 10 200 200)
;;			     (xlib:display-finish-output *display*)
;;			     (xlib:draw-glyphs window gc 10 10 (format nil "~A" 10))
;;			     (dbg 'ici))))
;;    
;;
;;;;(define-main-key ("F1" :mod-1) 'help-on-clfswm)
;;;;
;;(defun quit-clfswm ()
;;  "Quit clfswm"
;;  (throw 'exit-main-loop nil))
;;
;;
;;
;;(define-main-key ("Home" :mod-1 :control :shift) 'quit-clfswm)
;;
;;(define-main-key (#\t :mod-1) 'second-key-mode)
;;(define-main-key ("less" :control) 'second-key-mode)
;;
;;(define-main-key ("Tab" :mod-1) 'rotate-window-up)
;;(define-main-key ("Tab" :mod-1 :shift) 'rotate-window-down)
;;
;;(define-main-key (#\b :mod-1) 'banish-pointer)
;;(define-main-key (#\b :mod-1 :control) 'toggle-maximize-current-group)
;;
;;;; Escape
;;(define-main-key ("Escape" :control :shift) 'delete-current-window)
;;(define-main-key ("Escape" :mod-1 :control :shift) 'destroy-current-window)
;;(define-main-key ("Escape" :control) 'remove-current-window)
;;(define-main-key ("Escape" :shift) 'unhide-all-windows-in-current-group)
;;
;;
;;;; Up
;;(define-main-key ("Up" :mod-1) 'circulate-group-up)
;;(define-main-key ("Up" :mod-1 :shift) 'circulate-group-up-move-window)
;;(define-main-key ("Up" :mod-1 :shift :control) 'circulate-group-up-copy-window)
;;
;;
;;;; Down
;;(define-main-key ("Down" :mod-1) 'circulate-group-down)
;;(define-main-key ("Down" :mod-1 :shift) 'circulate-group-down-move-window)
;;(define-main-key ("Down" :mod-1 :shift :control) 'circulate-group-down-copy-window)
;;
;;
;;;; Right
;;(define-main-key ("Right" :mod-1) 'circulate-workspace-up)
;;(define-main-key ("Right" :mod-1 :shift) 'circulate-workspace-up-move-group)
;;(define-main-key ("Right" :mod-1 :shift :control) 'circulate-workspace-up-copy-group)
;;
;;
;;;; Left
;;(define-main-key ("Left" :mod-1) 'circulate-workspace-down)
;;(define-main-key ("Left" :mod-1 :shift) 'circulate-workspace-down-move-group)
;;(define-main-key ("Left" :mod-1 :shift :control) 'circulate-workspace-down-copy-group)
;;
;;
;;
;;(defmacro define-main-focus-workspace-by-number (key number)
;;  "Define a main key to focus a workspace by its number"
;;  `(define-main-key ,key
;;    (defun ,(create-symbol (format nil "b-main-focus-workspace-~A" number)) ()
;;      ,(format nil "Focus workspace ~A" number)
;;      (circulate-workspace-by-number ,number))))
;;
;;(define-main-focus-workspace-by-number (#\1 :mod-1) 1)
;;(define-main-focus-workspace-by-number (#\2 :mod-1) 2)
;;(define-main-focus-workspace-by-number (#\3 :mod-1) 3)
;;(define-main-focus-workspace-by-number (#\4 :mod-1) 4)
;;(define-main-focus-workspace-by-number (#\5 :mod-1) 5)
;;(define-main-focus-workspace-by-number (#\6 :mod-1) 6)
;;(define-main-focus-workspace-by-number (#\7 :mod-1) 7)
;;(define-main-focus-workspace-by-number (#\8 :mod-1) 8)
;;(define-main-focus-workspace-by-number (#\9 :mod-1) 9)
;;(define-main-focus-workspace-by-number (#\0 :mod-1) 10)

