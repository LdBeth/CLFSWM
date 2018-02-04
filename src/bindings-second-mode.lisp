;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Bindings keys and mouse for second mode
;;;
;;; Note: Mod-1 is the Alt or Meta key, Mod-2 is the Numlock key.
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
;;;| Second keys
;;;|
;;;| CONFIG - Second mode bindings
;;;`-----
(add-hook *binding-hook* 'init-*second-keys* 'init-*second-mouse*)


(defun open-frame-menu ()
  "Open the frame menu"
  (open-menu (find-menu 'frame-menu)))

(defun open-window-menu ()
  "Open the window menu"
  (open-menu (find-menu 'window-menu)))

(defun open-action-by-name-menu ()
  "Open the action by name menu"
  (open-menu (find-menu 'action-by-name-menu)))

(defun open-action-by-number-menu ()
  "Open the action by number menu"
  (open-menu (find-menu 'action-by-number-menu)))

(defun open-frame-movement-menu ()
  "Open the frame movement menu (pack/fill/resize)"
  (open-menu (find-menu 'frame-movement-menu)))

(defun open-root-menu ()
  "Open the root menu"
  (open-menu (find-menu 'root-menu)))

(defun open-child-menu ()
  "Open the child menu"
  (open-menu (find-menu 'child-menu)))

(defun tile-current-frame ()
  "Tile the current frame"
  (set-layout-once #'tile-layout)
  (leave-second-mode))


(defun stop-all-pending-actions ()
  "Stop all pending actions"
  (clear-all-nw-hooks)
  (leave-second-mode))


;;; default shell programs
(defmacro define-shell (key name docstring cmd)
  "Define a second key to start a shell command"
  `(define-second-key ,key
       (defun ,name ()
	 ,docstring
	 (setf *second-mode-leave-function* (let ((cmd ,cmd))
					      (lambda ()
						(do-shell cmd))))
	 (leave-second-mode))))


(defun set-default-second-keys ()
  (define-second-key ("F1" :prefix) 'help-on-clfswm)
  (define-second-key ("m") 'open-menu)
  (define-second-key ("less") 'open-menu)
  (define-second-key ("less" :control) 'open-menu)
  (define-second-key ("f") 'open-frame-menu)
  (define-second-key ("w") 'open-window-menu)
  (define-second-key ("n") 'open-action-by-name-menu)
  (define-second-key ("u") 'open-action-by-number-menu)
  (define-second-key ("p") 'open-frame-movement-menu)
  (define-second-key ("r") 'open-root-menu)
  (define-second-key ("c") 'open-child-menu)
  (define-second-key ("x") 'update-layout-managed-children-position)
  (define-second-key ("g" :control) 'stop-all-pending-actions)
  (define-second-key ("q") 'sm-delete-focus-window)
  (define-second-key ("k") 'sm-ask-close/kill-current-window)
  (define-second-key ("i") 'identify-key)
  (define-second-key ("colon") 'eval-from-query-string)
  (define-second-key ("exclam") 'run-program-from-query-string)
  (define-second-key ("Return") 'leave-second-mode)
  (define-second-key ("Escape") 'leave-second-mode)
  (define-second-key ("t" :shift) 'tile-current-frame)
  (define-second-key ("Home" :prefix :control :shift) 'exit-clfswm)
  (define-second-key ("Right" :prefix) 'select-next-brother)
  (define-second-key ("Left" :prefix) 'select-previous-brother)

  (define-second-key ("Right" :prefix :shift) 'select-next-brother-take-current)
  (define-second-key ("Left" :prefix :shift) 'select-previous-brother-take-current)

  (define-second-key ("Down" :prefix) 'select-previous-level)
  (define-second-key ("Up" :prefix) 'select-next-level)

  (define-second-key ("Left" :control :prefix) 'select-brother-spatial-move-left)
  (define-second-key ("Right" :control :prefix) 'select-brother-spatial-move-right)
  (define-second-key ("Up" :control :prefix) 'select-brother-spatial-move-up)
  (define-second-key ("Down" :control :prefix) 'select-brother-spatial-move-down)

  (define-second-key ("Left" :control :prefix :shift) 'select-brother-spatial-move-left-take-current)
  (define-second-key ("Right" :control :prefix :shift) 'select-brother-spatial-move-right-take-current)
  (define-second-key ("Up" :control :prefix :shift) 'select-brother-spatial-move-up-take-current)
  (define-second-key ("Down" :control :prefix :shift) 'select-brother-spatial-move-down-take-current)

  (define-second-key ("j") 'swap-frame-geometry)
  (define-second-key ("h") 'rotate-frame-geometry)
  (define-second-key ("h" :shift) 'anti-rotate-frame-geometry)

  (define-second-key ("Page_Up") 'select-next-root)
  (define-second-key ("Page_Down") 'select-previous-root)
  (define-second-key ("Page_Up" :control) 'rotate-root-geometry-next)
  (define-second-key ("Page_Down" :control) 'rotate-root-geometry-previous)

  (define-second-key ("Right") 'speed-mouse-right)
  (define-second-key ("Left") 'speed-mouse-left)
  (define-second-key ("Down") 'speed-mouse-down)
  (define-second-key ("Up") 'speed-mouse-up)
  (define-second-key ("Left" :control) 'speed-mouse-undo)
  (define-second-key ("Up" :control) 'speed-mouse-first-history)
  (define-second-key ("Down" :control) 'speed-mouse-reset)

  (define-second-key ("Tab" :prefix) 'select-next-child)
  (define-second-key ("Tab" :prefix :shift) 'select-previous-child)
  (define-second-key ("Tab" :prefix :control) 'select-next-subchild)
  (define-second-key ("Tab") 'switch-to-last-child)
  (define-second-key ("Return" :prefix) 'enter-frame)
  (define-second-key ("Return" :prefix :shift) 'leave-frame)
  (define-second-key ("Return" :prefix :control) 'frame-toggle-maximize)
  (define-second-key ("Return" :mod-5) 'frame-toggle-maximize)
  (define-second-key ("Page_Up" :prefix) 'frame-lower-child)
  (define-second-key ("Page_Down" :prefix) 'frame-raise-child)
  (define-second-key ("Home" :prefix) 'switch-to-root-frame)
  (define-second-key ("Home" :prefix :shift) 'switch-and-select-root-frame)
  (define-second-key ("Menu") 'toggle-show-root-frame)
  (define-second-key ("b" :prefix) 'banish-pointer)
  (define-second-key ("o") 'set-open-in-new-frame-in-parent-frame-nw-hook)
  (define-second-key ("o" :control) 'set-open-in-new-frame-in-root-frame-nw-hook)
  (define-second-key ("a") 'add-default-frame)
  (define-second-key ("a" :control) 'add-frame-in-parent-frame)
  (define-second-key ("plus") 'inc-tile-layout-size)
  (define-second-key ("minus") 'dec-tile-layout-size)
  (define-second-key ("plus" :control) 'inc-slow-tile-layout-size)
  (define-second-key ("minus" :control) 'dec-slow-tile-layout-size)
  ;; Escape
  (define-second-key ("Escape" :control) 'ask-close/kill-current-window)
  ;; Selection
  (define-second-key ("x" :control) 'cut-current-child)
  (define-second-key ("x" :control :prefix) 'clear-selection)
  (define-second-key ("c" :control) 'copy-current-child)
  (define-second-key ("v" :control) 'paste-selection)
  (define-second-key ("v" :control :shift) 'paste-selection-no-clear)
  (define-second-key ("Delete" :control) 'remove-current-child)
  (define-second-key ("Delete") 'delete-current-child)
  (define-shell ("t") b-start-xterm "start an xterm" "cd $HOME && exec alacritty")
  (define-shell ("e") b-start-emacs "start emacs" "cd $HOME && exec emacs")
  (define-shell ("e" :control) b-start-emacsremote
    "start an emacs for another user"
    "exec xterm -e emacsremote")
  (define-second-key ("F10" :prefix) 'fast-layout-switch)
  (define-second-key ("F10" :shift :control) 'toggle-show-root-frame)
  (define-second-key ("F10") 'expose-windows-mode)
  (define-second-key ("F10" :control) 'expose-all-windows-mode)
  (define-second-key ("F12" :shift) 'show-all-frames-info-key)
  (define-second-key ("F12" :shift :prefix) 'show-all-frames-info)
  ;; Bind or jump functions
  (define-second-key ("1" :prefix) 'bind-or-jump 1)
  (define-second-key ("2" :prefix) 'bind-or-jump 2)
  (define-second-key ("3" :prefix) 'bind-or-jump 3)
  (define-second-key ("4" :prefix) 'bind-or-jump 4)
  (define-second-key ("5" :prefix) 'bind-or-jump 5)
  (define-second-key ("6" :prefix) 'bind-or-jump 6)
  (define-second-key ("7" :prefix) 'bind-or-jump 7)
  (define-second-key ("8" :prefix) 'bind-or-jump 8)
  (define-second-key ("9" :prefix) 'bind-or-jump 9)
  (define-second-key ("0" :prefix) 'bind-or-jump 10)
  ;;; Transparency
  (define-second-key ("t" :control :shift) 'key-inc-transparency)
  (define-second-key ("t" :control) 'key-dec-transparency))

(add-hook *binding-hook* 'set-default-second-keys)




;;; Mouse action
(defun sm-mouse-click-to-focus-and-move (window root-x root-y)
  "Move and focus the current child - Create a new frame on the root window.
Or do corners actions"
  (declare (ignore window))
  (or (do-corner-action root-x root-y *corner-second-mode-left-button*)
      (mouse-focus-move/resize-generic root-x root-y #'move-frame nil)))

(defun sm-mouse-click-to-focus-and-resize (window root-x root-y)
  "Resize and focus the current child - Create a new frame on the root window.
Or do corners actions"
  (declare (ignore window))
  (or (do-corner-action root-x root-y *corner-second-mode-right-button*)
      (mouse-focus-move/resize-generic root-x root-y #'resize-frame nil)))


(defun sm-mouse-middle-click (window root-x root-y)
  "Do actions on corners"
  (declare (ignore window))
  (or (do-corner-action root-x root-y *corner-second-mode-middle-button*)
      (replay-button-event)))




(defun sm-mouse-select-next-level (window root-x root-y)
  "Select the next level in frame"
  (declare (ignore window root-x root-y))
  (select-next-level))




(defun sm-mouse-select-previous-level (window root-x root-y)
  "Select the previous level in frame"
  (declare (ignore window root-x root-y))
  (select-previous-level))



(defun sm-mouse-enter-frame (window root-x root-y)
  "Enter in the selected frame - ie make it the root frame"
  (declare (ignore window root-x root-y))
  (enter-frame))



(defun sm-mouse-leave-frame (window root-x root-y)
  "Leave the selected frame - ie make its parent the root frame"
  (declare (ignore window root-x root-y))
  (leave-frame))


(defun sm-mouse-click-to-focus-and-move-window (window root-x root-y)
  "Move and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (mouse-focus-move/resize-generic root-x root-y #'move-frame t))


(defun sm-mouse-click-to-focus-and-resize-window (window root-x root-y)
  "Resize and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (mouse-focus-move/resize-generic root-x root-y #'resize-frame t))


(defun sm-mouse-click-to-focus-and-move-window-constrained (window root-x root-y)
  "Move (constrained by other frames) and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (mouse-focus-move/resize-generic root-x root-y #'move-frame-constrained t))


(defun sm-mouse-click-to-focus-and-resize-window-constrained (window root-x root-y)
  "Resize (constrained by other frames) and focus the current child - Create a new frame on the root window"
  (declare (ignore window))
  (mouse-focus-move/resize-generic root-x root-y #'resize-frame-constrained t))



(defun set-default-second-mouse ()
  (define-second-mouse (1) 'sm-mouse-click-to-focus-and-move)
  (define-second-mouse (2) 'sm-mouse-middle-click)
  (define-second-mouse (3) 'sm-mouse-click-to-focus-and-resize)
  (define-second-mouse (1 :prefix) 'sm-mouse-click-to-focus-and-move-window)
  (define-second-mouse (3 :prefix) 'sm-mouse-click-to-focus-and-resize-window)
  (define-second-mouse (1 :prefix :shift) 'sm-mouse-click-to-focus-and-move-window-constrained)
  (define-second-mouse (3 :prefix :shift) 'sm-mouse-click-to-focus-and-resize-window-constrained)
  (define-second-mouse (1 :control :prefix) 'mouse-move-child-over-frame)
  (define-second-mouse (4) 'sm-mouse-select-next-level)
  (define-second-mouse (5) 'sm-mouse-select-previous-level)
  (define-second-mouse (4 :prefix) 'sm-mouse-enter-frame)
  (define-second-mouse (5 :prefix) 'sm-mouse-leave-frame))

(add-hook *binding-hook* 'set-default-second-mouse)
