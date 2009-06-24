;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Bindings keys and mouse for second mode
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

(defun open-frame-pack-menu ()
  "Open the frame pack menu"
  (open-menu (find-menu 'frame-pack-menu)))

(defun open-frame-fill-menu ()
  "Open the frame fill menu"
  (open-menu (find-menu 'frame-fill-menu)))

(defun open-frame-resize-menu ()
  "Open the frame resize menu"
  (open-menu (find-menu 'frame-resize-menu)))

(defun tile-current-frame ()
  "Tile the current frame"
  (set-layout-once #'tile-layout)
  (leave-second-mode))

;;; default shell programs
(defmacro define-shell (key name docstring cmd)
  "Define a second key to start a shell command"
  `(define-second-key ,key
       (defun ,name ()
	 ,docstring
	 (setf *second-mode-program* ,cmd)
	 (leave-second-mode))))



(defun set-default-second-keys ()
  (define-second-key ("F1" :mod-1) 'help-on-clfswm)
  (define-second-key ("m") 'open-menu)
  (define-second-key ("less") 'open-menu)
  (define-second-key ("less" :control) 'open-menu)
  (define-second-key ("f") 'open-frame-menu)
  (define-second-key ("w") 'open-window-menu)
  (define-second-key ("n") 'open-action-by-name-menu)
  (define-second-key ("u") 'open-action-by-number-menu)
  (define-second-key ("p") 'open-frame-pack-menu)
  (define-second-key ("l") 'open-frame-fill-menu)
  (define-second-key ("r") 'open-frame-resize-menu)
  ;;(define-second-key (#\g :control) 'stop-all-pending-actions)
  (define-second-key ("i") 'identify-key)
  (define-second-key ("colon") 'eval-from-query-string)
  (define-second-key ("exclam") 'run-program-from-query-string)
  (define-second-key ("Return") 'leave-second-mode)
  (define-second-key ("Escape") 'leave-second-mode)
  (define-second-key ("t") 'tile-current-frame)
  (define-second-key ("Home" :mod-1 :control :shift) 'exit-clfswm)
  (define-second-key ("Right" :mod-1) 'select-next-brother)
  (define-second-key ("Left" :mod-1) 'select-previous-brother)
  (define-second-key ("Down" :mod-1) 'select-previous-level)
  (define-second-key ("Up" :mod-1) 'select-next-level)
  (define-second-key ("Tab" :mod-1) 'select-next-child)
  (define-second-key ("Tab" :mod-1 :shift) 'select-previous-child)
  (define-second-key (#\Tab :shift) 'switch-to-last-child)
  (define-second-key ("Return" :mod-1) 'enter-frame)
  (define-second-key ("Return" :mod-1 :shift) 'leave-frame)
  (define-second-key ("Return" :mod-5) 'frame-toggle-maximize)
  (define-second-key ("Page_Up" :mod-1) 'frame-lower-child)
  (define-second-key ("Page_Down" :mod-1) 'frame-raise-child)
  (define-second-key ("Home" :mod-1) 'switch-to-root-frame)
  (define-second-key ("Home" :mod-1 :shift) 'switch-and-select-root-frame)
  (define-second-key ("Menu") 'toggle-show-root-frame)
  (define-second-key (#\b :mod-1) 'banish-pointer)
  (define-second-key (#\o) 'set-open-in-new-frame-in-root-frame-nw-hook)
  (define-second-key (#\o :control) 'set-open-in-new-frame-in-parent-frame-nw-hook)
  (define-second-key (#\a) 'add-default-frame)
  ;; Escape
  (define-second-key ("Escape" :control :shift) 'delete-focus-window)
  (define-second-key ("Escape" :mod-1 :control :shift) 'destroy-focus-window)
  (define-second-key ("Escape" :control) 'remove-focus-window)
  (define-second-key ("Escape" :shift) 'unhide-all-windows-in-current-child)
  ;; Selection
  (define-second-key ("x" :control) 'cut-current-child)
  (define-second-key ("x" :control :mod-1) 'clear-selection)
  (define-second-key ("c" :control) 'copy-current-child)
  (define-second-key ("v" :control) 'paste-selection)
  (define-second-key ("v" :control :shift) 'paste-selection-no-clear)
  (define-second-key ("Delete") 'remove-current-child)
  (define-shell (#\c) b-start-xterm "start an xterm" "exec xterm")
  (define-shell (#\e) b-start-emacs "start emacs" "exec emacs")
  (define-shell (#\e :control) b-start-emacsremote
    "start an emacs for another user"
    "exec xterm -e emacsremote")
  (define-shell (#\h) b-start-xclock "start an xclock" "exec xclock -d")
  (define-second-key ("Menu") 'show-all-frames-info-key)
  (define-second-key ("Menu" :shift) 'show-all-frames-info)
  (define-second-key ("Menu" :control) 'toggle-show-root-frame)
  ;; Bind or jump functions
  (define-second-key ("1" :mod-1) 'bind-or-jump 1)
  (define-second-key ("2" :mod-1) 'bind-or-jump 2)
  (define-second-key ("3" :mod-1) 'bind-or-jump 3)
  (define-second-key ("4" :mod-1) 'bind-or-jump 4)
  (define-second-key ("5" :mod-1) 'bind-or-jump 5)
  (define-second-key ("6" :mod-1) 'bind-or-jump 6)
  (define-second-key ("7" :mod-1) 'bind-or-jump 7)
  (define-second-key ("8" :mod-1) 'bind-or-jump 8)
  (define-second-key ("9" :mod-1) 'bind-or-jump 9)
  (define-second-key ("0" :mod-1) 'bind-or-jump 10))

(add-hook *binding-hook* 'set-default-second-keys)


;; For a French azery keyboard:
;;(undefine-second-multi-keys (#\1 :mod-1) (#\2 :mod-1) (#\3 :mod-1)
;;			    (#\4 :mod-1) (#\5 :mod-1) (#\6 :mod-1)
;;			    (#\7 :mod-1) (#\8 :mod-1) (#\9 :mod-1) (#\0 :mod-1))
;;(define-second-key ("ampersand" :mod-1) 'bind-or-jump 1)
;;(define-second-key ("eacute" :mod-1) 'bind-or-jump 2)
;;(define-second-key ("quotedbl" :mod-1) 'bind-or-jump 3)
;;(define-second-key ("quoteright" :mod-1) 'bind-or-jump 4)
;;(define-second-key ("parenleft" :mod-1) 'bind-or-jump 5)
;;(define-second-key ("minus" :mod-1) 'bind-or-jump 6)
;;(define-second-key ("egrave" :mod-1) 'bind-or-jump 7)
;;(define-second-key ("underscore" :mod-1) 'bind-or-jump 8)
;;(define-second-key ("ccedilla" :mod-1) 'bind-or-jump 9)
;;(define-second-key ("agrave" :mod-1) 'bind-or-jump 10)



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



(defun set-default-second-mouse ()
  (define-second-mouse (1) 'sm-mouse-click-to-focus-and-move)
  (define-second-mouse (2) 'sm-mouse-middle-click)
  (define-second-mouse (3) 'sm-mouse-click-to-focus-and-resize)
  (define-second-mouse (1 :mod-1) 'sm-mouse-click-to-focus-and-move-window)
  (define-second-mouse (3 :mod-1) 'sm-mouse-click-to-focus-and-resize-window)
  (define-second-mouse (1 :control :mod-1) 'mouse-move-window-over-frame)
  (define-second-mouse (4) 'sm-mouse-select-next-level)
  (define-second-mouse (5) 'sm-mouse-select-previous-level)
  (define-second-mouse (4 :mod-1) 'sm-mouse-enter-frame)
  (define-second-mouse (5 :mod-1) 'sm-mouse-leave-frame))

(add-hook *binding-hook* 'set-default-second-mouse)
