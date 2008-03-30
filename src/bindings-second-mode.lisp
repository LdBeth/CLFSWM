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



;;;;;;;;;;;;;;;
;; Menu entry
;;;;;;;;;;;;;;;
(defun frame-adding-menu ()
  "Adding frame menu"
  (info-mode-menu '((#\a add-default-frame)
		    (#\p add-placed-frame))))

(defun frame-layout-menu ()
  "Frame layout menu"
  (info-mode-menu (keys-from-list *layout-list*)))

(defun frame-nw-hook-menu ()
  "Frame new window hook menu"
  (info-mode-menu (keys-from-list *nw-hook-list*)))


  

(defun frame-pack-menu ()
  "Frame pack menu"
  (info-mode-menu '(("Up" current-frame-pack-up)
		    ("Down" current-frame-pack-down)
		    ("Left" current-frame-pack-left)
		    ("Right" current-frame-pack-right))))


(defun frame-fill-menu ()
  "Frame fill menu"
  (info-mode-menu '(("Up" current-frame-fill-up)
		    ("Down" current-frame-fill-down)
		    ("Left" current-frame-fill-left)
		    ("Right" current-frame-fill-right)
		    (#\a current-frame-fill-all-dir)
		    (#\v current-frame-fill-vertical)
		    (#\h current-frame-fill-horizontal))))

(defun frame-resize-menu ()
  "Frame resize menu"
  (info-mode-menu '(("Up" current-frame-resize-up)
		    ("Down" current-frame-resize-down)
		    ("Left" current-frame-resize-left)
		    ("Right" current-frame-resize-right)
		    (#\d current-frame-resize-all-dir)
		    (#\a current-frame-resize-all-dir-minimal))))


(defun frame-movement-menu ()
  "Frame movement menu"
  (info-mode-menu '((#\p frame-pack-menu)
		    (#\f frame-fill-menu)
		    (#\r frame-resize-menu)
		    (#\c center-current-frame)))
  (leave-second-mode))


(defmacro with-movement (&body body)
  `(when (frame-p *current-child*)
     ,@body
     (show-all-children)
     (draw-second-mode-window)
     (frame-movement-menu)))


;;; Pack
(defun current-frame-pack-up ()
  "Pack the current frame up"
  (with-movement (pack-frame-up *current-child* (find-father-frame *current-child* *current-root*))))

(defun current-frame-pack-down ()
  "Pack the current frame down"
  (with-movement (pack-frame-down *current-child* (find-father-frame *current-child* *current-root*))))

(defun current-frame-pack-left ()
  "Pack the current frame left"
  (with-movement (pack-frame-left *current-child* (find-father-frame *current-child* *current-root*))))

(defun current-frame-pack-right ()
  "Pack the current frame right"
  (with-movement (pack-frame-right *current-child* (find-father-frame *current-child* *current-root*))))

;;; Center
(defun center-current-frame ()
  "Center the current frame"
  (with-movement (center-frame *current-child*)))

;;; Fill
(defun current-frame-fill-up ()
  "Fill the current frame up"
  (with-movement (fill-frame-up *current-child* (find-father-frame *current-child* *current-root*))))

(defun current-frame-fill-down ()
  "Fill the current frame down"
  (with-movement (fill-frame-down *current-child* (find-father-frame *current-child* *current-root*))))

(defun current-frame-fill-left ()
  "Fill the current frame left"
  (with-movement (fill-frame-left *current-child* (find-father-frame *current-child* *current-root*))))

(defun current-frame-fill-right ()
  "Fill the current frame right"
  (with-movement (fill-frame-right *current-child* (find-father-frame *current-child* *current-root*))))

(defun current-frame-fill-all-dir ()
  "Fill the current frame in all directions"
  (with-movement
    (let ((father (find-father-frame *current-child* *current-root*)))
      (fill-frame-up *current-child* father)
      (fill-frame-down *current-child* father)
      (fill-frame-left *current-child* father)
      (fill-frame-right *current-child* father))))

(defun current-frame-fill-vertical ()
  "Fill the current frame vertically"
  (with-movement
    (let ((father (find-father-frame *current-child* *current-root*)))
      (fill-frame-up *current-child* father)
      (fill-frame-down *current-child* father))))

(defun current-frame-fill-horizontal ()
  "Fill the current frame horizontally"
  (with-movement
    (let ((father (find-father-frame *current-child* *current-root*)))
      (fill-frame-left *current-child* father)
      (fill-frame-right *current-child* father))))
    

;;; Resize
(defun current-frame-resize-up ()
  "Resize the current frame up to its half height"
  (with-movement (resize-half-height-up *current-child*)))

(defun current-frame-resize-down ()
  "Resize the current frame down to its half height"
  (with-movement (resize-half-height-down *current-child*)))

(defun current-frame-resize-left ()
  "Resize the current frame left to its half width"
  (with-movement (resize-half-width-left *current-child*)))

(defun current-frame-resize-right ()
  "Resize the current frame right to its half width"
  (with-movement (resize-half-width-right *current-child*)))

(defun current-frame-resize-all-dir ()
  "Resize down the current frame"
  (with-movement (resize-frame-down *current-child*)))

(defun current-frame-resize-all-dir-minimal ()
  "Resize down the current frame to its minimal size"
  (with-movement (resize-minimal-frame *current-child*)))







(defun action-by-name-menu ()
  "Actions by name menu"
  (info-mode-menu '((#\f focus-frame-by-name)
		    (#\o open-frame-by-name)
		    (#\d delete-frame-by-name)
		    (#\m move-current-child-by-name)
		    (#\c copy-current-child-by-name))))

(defun action-by-number-menu ()
  "Actions by number menu"
  (info-mode-menu '((#\f focus-frame-by-number)
		    (#\o open-frame-by-number)
		    (#\d delete-frame-by-number)
		    (#\m move-current-child-by-number)
		    (#\c copy-current-child-by-number))))


(defun frame-info-menu ()
  "Frame information menu"
  (info-mode-menu '((#\s show-all-frames-info)
		    (#\h hide-all-frames-info))))


(defun frame-menu ()
  "Frame menu"
  (info-mode-menu '((#\a frame-adding-menu)
		    (#\l frame-layout-menu)
		    (#\n frame-nw-hook-menu)
		    (#\m frame-movement-menu)
		    (#\r rename-current-child)
		    (#\u renumber-current-frame)
		    (#\i frame-info-menu)
		    (#\x explode-current-frame))))

(defun window-menu ()
  "Window menu"
  (info-mode-menu '((#\i force-window-in-frame)
		    (#\c force-window-center-in-frame))))



(defun selection-menu ()
  "Selection menu"
  (info-mode-menu '((#\x cut-current-child)
		    (#\c copy-current-child)
		    (#\v paste-selection)
		    (#\p paste-selection-no-clear)
		    ("Delete" remove-current-child)
		    (#\z clear-selection))))


(defun utility-menu ()
  "Utility menu"
  (info-mode-menu '((#\i identify-key)
		    (#\: eval-from-query-string)
		    (#\! run-program-from-query-string))))
  
(defun main-menu ()
  "Open the main menu"
  (info-mode-menu '((#\f frame-menu)
		    (#\w window-menu)
		    (#\s selection-menu)
		    (#\n action-by-name-menu)
		    (#\u action-by-number-menu)
		    (#\y utility-menu))))






(define-second-key ("F1" :mod-1) 'help-on-second-mode)

(define-second-key ("m") 'main-menu)
(define-second-key ("f") 'frame-menu)
(define-second-key ("n") 'action-by-name-menu)
(define-second-key ("u") 'action-by-number-menu)


;;(define-second-key (#\g :control) 'stop-all-pending-actions)

(define-second-key (#\i) 'identify-key)
(define-second-key (#\:) 'eval-from-query-string)

(define-second-key (#\!) 'run-program-from-query-string)


(define-second-key (#\t) 'leave-second-mode)
(define-second-key ("Return") 'leave-second-mode)
(define-second-key ("Escape") 'leave-second-mode)


(define-second-key (#\< :control) 'leave-second-mode)




(define-second-key ("Home" :mod-1 :control :shift) 'quit-clfswm)

(define-second-key ("Right" :mod-1) 'select-next-brother)
(define-second-key ("Left" :mod-1) 'select-previous-brother)

(define-second-key ("Down" :mod-1) 'select-next-level)
(define-second-key ("Up" :mod-1) 'select-previous-level)

(define-second-key ("Tab" :mod-1) 'select-next-child)
(define-second-key ("ISO_Left_Tab" :mod-1 :shift) 'select-previous-child)

(define-second-key ("Return" :mod-1) 'enter-frame)
(define-second-key ("Return" :mod-1 :shift) 'leave-frame)

(define-second-key ("Home" :mod-1) 'switch-to-root-frame)
(define-second-key ("Home" :mod-1 :shift) 'switch-and-select-root-frame)

(define-second-key ("Menu") 'toggle-show-root-frame)

(define-second-key (#\b :mod-1) 'banish-pointer)

(define-second-key (#\o) 'set-open-in-new-frame-in-root-frame-nw-hook)


;;;; Escape
(define-second-key ("Escape" :control :shift) 'delete-focus-window)
(define-second-key ("Escape" :mod-1 :control :shift) 'destroy-focus-window)
(define-second-key ("Escape" :control) 'remove-focus-window)
(define-second-key ("Escape" :shift) 'unhide-all-windows-in-current-child)


;;; Selection
(define-second-key ("x" :control) 'cut-current-child)
(define-second-key ("x" :control :mod-1) 'clear-selection)
(define-second-key ("c" :control) 'copy-current-child)
(define-second-key ("v" :control) 'paste-selection)
(define-second-key ("v" :control :shift) 'paste-selection-no-clear)
(define-second-key ("Delete") 'remove-current-child)



;;; default shell programs
(defmacro define-shell (key name docstring cmd)
  "Define a second key to start a shell command"
  `(define-second-key ,key
       (defun ,name ()
	 ,docstring
	 (setf *second-mode-program* ,cmd)
	 (leave-second-mode))))

(define-shell (#\c) b-start-xterm "start an xterm" "exec xterm")
(define-shell (#\e) b-start-emacs "start emacs" "exec emacs")
(define-shell (#\e :control) b-start-emacsremote
  "start an emacs for another user"
  "exec emacsremote-Eterm")
(define-shell (#\h) b-start-xclock "start an xclock" "exec xclock -d")


(define-second-key ("Menu") 'show-all-frames-info-key)
(define-second-key ("Menu" :shift) 'show-all-frames-info)
(define-second-key ("Menu" :control) 'toggle-show-root-frame)







;;; Mouse action
(defun sm-mouse-click-to-focus-generic (window root-x root-y mouse-fn)
  (declare (ignore window))
  (let* ((child (find-child-under-mouse root-x root-y))
	 (father (find-father-frame child)))
    (when (equal child *current-root*)
      (setf child (create-frame)
	    father *current-root*
	    mouse-fn #'resize-frame)
      (place-frame child father root-x root-y 10 10)
	    (xlib:map-window (frame-window child))
	    (pushnew child (frame-child *current-root*)))
    (typecase child
      (xlib:window (funcall mouse-fn father (find-father-frame father) root-x root-y))
      (frame (funcall mouse-fn child father root-x root-y)))
    (focus-all-children child father nil)
    (show-all-children)))

(defun sm-mouse-click-to-focus-and-move (window root-x root-y)
  "Move and focus the current child - Create a new frame on the root window"
  (sm-mouse-click-to-focus-generic window root-x root-y #'move-frame))

(defun sm-mouse-click-to-focus-and-resize (window root-x root-y)
  "Resize and focus the current child - Create a new frame on the root window"
  (sm-mouse-click-to-focus-generic window root-x root-y #'resize-frame))



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
  "Leave the selected frame - ie make its father the root frame"
  (declare (ignore window root-x root-y))
  (leave-frame))




(define-second-mouse (1) 'sm-mouse-click-to-focus-and-move)
(define-second-mouse (3) 'sm-mouse-click-to-focus-and-resize)

(define-second-mouse (4) 'sm-mouse-select-next-level)
(define-second-mouse (5) 'sm-mouse-select-previous-level)

(define-second-mouse (4 :mod-1) 'sm-mouse-enter-frame)
(define-second-mouse (5 :mod-1) 'sm-mouse-leave-frame)






;;;; Escape
;;(define-second-key ("Escape" :control :shift) 'delete-current-window)
;;(define-second-key ("Escape" :mod-1 :control :shift) 'destroy-current-window)
;;(define-second-key ("Escape" :control) 'remove-current-window)
;;(define-second-key ("Escape" :shift) 'unhide-all-windows-in-current-frame)
;;
;;
;;;; Up
;;(define-second-key ("Up" :mod-1) 'circulate-frame-up)
;;(define-second-key ("Up" :mod-1 :shift) 'circulate-frame-up-move-window)
;;(define-second-key ("Up" :mod-1 :shift :control) 'circulate-frame-up-copy-window)
;;
;;
;;;; Down
;;(define-second-key ("Down" :mod-1) 'circulate-frame-down)
;;(define-second-key ("Down" :mod-1 :shift) 'circulate-frame-down-move-window)
;;(define-second-key ("Down" :mod-1 :shift :control) 'circulate-frame-down-copy-window)
;;
;;
;;;; Right
;;(define-second-key ("Right" :mod-1) 'circulate-workspace-up)
;;(define-second-key ("Right" :mod-1 :shift) 'circulate-workspace-up-move-frame)
;;(define-second-key ("Right" :mod-1 :shift :control) 'circulate-workspace-up-copy-frame)
;;
;;
;;;; Left
;;(define-second-key ("Left" :mod-1) 'circulate-workspace-down)
;;(define-second-key ("Left" :mod-1 :shift) 'circulate-workspace-down-move-frame)
;;(define-second-key ("Left" :mod-1 :shift :control) 'circulate-workspace-down-copy-frame)
;;
;;
;;(defmacro define-second-focus-workspace-by-number (key number)
;;  "Define a second key to focus a workspace by its number"
;;  `(define-second-key ,key
;;       (defun ,(create-symbol (format nil "b-second-focus-workspace-~A" number)) ()
;;	 ,(format nil "Focus workspace ~A" number)
;;	 (circulate-workspace-by-number ,number))))
;;
;;(define-second-focus-workspace-by-number (#\1 :mod-1) 1)
;;(define-second-focus-workspace-by-number (#\2 :mod-1) 2)
;;(define-second-focus-workspace-by-number (#\3 :mod-1) 3)
;;(define-second-focus-workspace-by-number (#\4 :mod-1) 4)
;;(define-second-focus-workspace-by-number (#\5 :mod-1) 5)
;;(define-second-focus-workspace-by-number (#\6 :mod-1) 6)
;;(define-second-focus-workspace-by-number (#\7 :mod-1) 7)
;;(define-second-focus-workspace-by-number (#\8 :mod-1) 8)
;;(define-second-focus-workspace-by-number (#\9 :mod-1) 9)
;;(define-second-focus-workspace-by-number (#\0 :mod-1) 10)
;;
;;(define-second-key (#\1 :control :mod-1) 'renumber-workspaces)
;;(define-second-key (#\2 :control :mod-1) 'sort-workspaces)
;;
;;
;;
;;
;;
;;(define-second-key ("Tab" :mod-1) 'rotate-window-up)
;;(define-second-key ("Tab" :mod-1 :shift) 'rotate-window-down)
;;
;;(define-second-key (#\b) 'banish-pointer)
;;
;;(define-second-key (#\b :mod-1) 'toggle-maximize-current-frame)
;;
;;(define-second-key (#\x) 'pager-mode)
;;
;;
;;(define-second-key (#\k :mod-1) 'destroy-current-window)
;;(define-second-key (#\k) 'remove-current-window)
;;
;;
;;(define-second-key (#\g) 'create-new-default-frame)
;;(define-second-key (#\g :mod-1) 'remove-current-frame)
;;
;;(define-second-key (#\w) 'create-new-default-workspace)
;;(define-second-key (#\w :mod-1) 'remove-current-workspace)
;;
;;(define-second-key (#\o)
;;    (defun b-open-next-window-in-new-workspace ()
;;      "Open the next window in a new workspace"
;;      (setf *open-next-window-in-new-workspace* t)
;;      (leave-second-mode)))
;;
;;(define-second-key (#\o :control)
;;    (defun b-open-next-window-in-workspace-numbered ()
;;      "Open the next window in a numbered workspace"
;;      (let ((number (parse-integer (or (query-string "Open next window in workspace:") "")
;;				   :junk-allowed t)))
;;	(when (numberp number)
;;	  (setf *open-next-window-in-new-workspace* number)))
;;      (leave-second-mode)))
;;
;;
;;(define-second-key (#\o :mod-1)
;;    (defun b-open-next-window-in-new-frame-once ()
;;      "Open the next window in a new frame and all others in the same frame"
;;      (setf *open-next-window-in-new-frame* :once)
;;      (leave-second-mode)))
;;
;;(define-second-key (#\o :mod-1 :control)
;;    (defun b-open-next-window-in-new-frame ()
;;      "Open each next window in a new frame"
;;      (setf *open-next-window-in-new-frame* t)
;;      (leave-second-mode)))
;;
;;
;;
;;(defmacro define-shell (key name docstring cmd)
;;  "Define a second key to start a shell command"
;;  `(define-second-key ,key
;;       (defun ,name ()
;;	 ,docstring
;;	 (setf *second-mode-program* ,cmd)
;;	 (leave-second-mode))))
;;
;;(define-shell (#\c) b-start-xterm "start an xterm" "exec xterm")
;;(define-shell (#\e) b-start-emacs "start emacs" "exec emacs")
;;(define-shell (#\e :control) b-start-emacsremote
;;  "start an emacs for another user"
;;  "exec emacsremote-Eterm")
;;(define-shell (#\h) b-start-xclock "start an xclock" "exec xclock -d")
;;
;;
;;(define-second-key (#\a) 'force-window-center-in-frame)
;;(define-second-key (#\a :mod-1) 'force-window-in-frame)
;;
;;
;;(define-second-key (#\d :mod-1)
;;    (defun b-show-debuging-info ()
;;      "Show debuging info"
;;      (dbg *workspace-list*)
;;      (dbg *screen*)
;;      (dbg (xlib:query-tree *root*))))
;;
;;(define-second-key (#\t :control) 'tile-current-workspace-vertically)
;;(define-second-key (#\t :shift :control) 'tile-current-workspace-horizontally)
;;
;;(define-second-key (#\y) 'tile-current-workspace-to)
;;(define-second-key (#\y :mod-1) 'reconfigure-tile-workspace)
;;(define-second-key (#\y :control) 'explode-current-frame)
;;(define-second-key (#\y :control :shift) 'implode-current-frame)
;;    
;;;;;,-----
;;;;;| Moving/Resizing frames
;;;;;`-----
;;(define-second-key (#\p)
;;    (defun b-pack-frame-on-next-arrow ()
;;      "Pack frame on next arrow action"
;;      (setf *arrow-action* :pack)))
;;
;;
;;(defun fill-frame-in-all-directions ()
;;  "Fill frame in all directions"
;;  (fill-current-frame-up)
;;  (fill-current-frame-left)
;;  (fill-current-frame-right)
;;  (fill-current-frame-down))
;;
;;
;;(define-second-key (#\f)
;;    (defun b-fill-frame ()
;;      "Fill frame on next arrow action (fill in all directions on second f keypress)"
;;      (case *arrow-action*
;;	(:fill (fill-frame-in-all-directions)
;;	       (setf *arrow-action* nil))
;;	(t (setf *arrow-action* :fill)))))
;;
;;(define-second-key (#\f :mod-1) 'fill-frame-in-all-directions)
;;
;;(define-second-key (#\f :shift)
;;    (defun b-fill-frame-vert ()
;;      "Fill frame vertically"
;;      (fill-current-frame-up)
;;      (fill-current-frame-down)))
;;
;;(define-second-key (#\f :control)
;;    (defun b-fill-frame-horiz ()
;;      "Fill frame horizontally"
;;      (fill-current-frame-left)
;;      (fill-current-frame-right)))
;;
;;
;;(define-second-key (#\r)
;;    (defun b-resize-half ()
;;      "Resize frame to its half width or heigth on next arraw action"
;;      (setf *arrow-action* :resize-half)))
;;
;;
;;(define-second-key (#\l) 'resize-minimal-current-frame)
;;(define-second-key (#\l :mod-1) 'resize-down-current-frame)
;;
;;
;;(define-second-key (#\m) 'center-current-frame)
;;   
;;
;;(define-second-key ("Up")
;;    (defun b-move-or-pack-up ()
;;      "Move, pack, fill or resize frame up"
;;      (case *arrow-action*
;;	(:pack (pack-current-frame-up))
;;	(:fill (fill-current-frame-up))
;;	(:resize-half (resize-half-height-up-current-frame))
;;	(t (move-frame (current-frame) 0 -10)))
;;      (setf *arrow-action* nil)))
;;
;;(define-second-key ("Down")
;;    (defun b-move-or-pack-down ()
;;      "Move, pack, fill or resize frame down"
;;      (case *arrow-action*
;;	(:pack (pack-current-frame-down))
;;	(:fill (fill-current-frame-down))
;;	(:resize-half (resize-half-height-down-current-frame))
;;	(t (move-frame (current-frame) 0 +10)))
;;      (setf *arrow-action* nil)))
;;
;;(define-second-key ("Right")
;;    (defun b-move-or-pack-right ()
;;      "Move, pack, fill or resize frame right"
;;      (case *arrow-action*
;;	(:pack (pack-current-frame-right))
;;	(:fill (fill-current-frame-right))
;;	(:resize-half (resize-half-width-right-current-frame))
;;	(t (move-frame (current-frame) +10 0)))
;;      (setf *arrow-action* nil)))
;;
;;(define-second-key ("Left")
;;    (defun b-move-or-pack-left ()
;;      "Move, pack, fill or resize frame left"
;;      (case *arrow-action*
;;	(:pack (pack-current-frame-left))
;;	(:fill (fill-current-frame-left))
;;	(:resize-half (resize-half-width-left-current-frame))
;;	(t (move-frame (current-frame) -10 0)))
;;      (setf *arrow-action* nil)))
;;
;;
;;(define-second-key ("Up" :shift)
;;    (defun b-resize-up ()
;;      "Resize frame up"
;;      (resize-frame (current-frame) 0 -10)))
;;
;;(define-second-key ("Down" :shift)
;;    (defun b-resize-down ()
;;      "Resize frame down"
;;      (resize-frame (current-frame) 0 +10)))
;;
;;(define-second-key ("Right" :shift)
;;    (defun b-resize-right ()
;;      "Resize frame right"
;;      (resize-frame (current-frame) +10 0)))
;;
;;(define-second-key ("Left" :shift)
;;    (defun b-resize-left ()
;;      "Resize frame left"
;;      (resize-frame (current-frame) -10 0)))
;;
;;
;;;;;,-----
;;;;;| Mouse second mode functions
;;;;;`-----
;;(defun select-frame-under-mouse (root-x root-y)
;;  (let ((frame (find-frame-under-mouse root-x root-y)))
;;    (when frame
;;      (no-focus)
;;      (focus-frame frame (current-workspace))
;;      (focus-window (current-window))
;;      (show-all-frame (current-workspace) nil))))
;;
;;(defun mouse-leave-second-mode-maximize (root-x root-y)
;;  "Leave second mode and maximize current frame"
;;  (select-frame-under-mouse root-x root-y)
;;  (maximize-frame (current-frame))
;;  (show-all-windows-in-workspace (current-workspace))
;;  (throw 'exit-second-loop nil))
;;
;;(defun mouse-leave-second-mode (root-x root-y)
;;  "Leave second mode"
;;  (select-frame-under-mouse root-x root-y)
;;  (show-all-windows-in-workspace (current-workspace))
;;  (throw 'exit-second-loop nil))
;;
;;
;;
;;
;;(defun mouse-circulate-window-up (root-x root-y)
;;  "Rotate window up" 
;;  (declare (ignore root-x root-y))
;;  (rotate-window-up))
;;
;;
;;(defun mouse-circulate-window-down (root-x root-y)
;;  "Rotate window down" 
;;  (declare (ignore root-x root-y))
;;  (rotate-window-down))
;;
;;
;;
;;(defun mouse-circulate-workspace-up (root-x root-y)
;;  "Circulate up in workspaces" 
;;  (declare (ignore root-x root-y))
;;  (circulate-workspace-up))
;;
;;
;;(defun mouse-circulate-workspace-down (root-x root-y)
;;  "Circulate down in workspaces" 
;;  (declare (ignore root-x root-y))
;;  (circulate-workspace-down))
;;
;;
;;
;;
;;(defun init-motion-vars ()
;;  (setf *motion-action* nil
;;	*motion-object* nil
;;	*motion-start-frame* nil
;;	*motion-dx* nil
;;	*motion-dy* nil))
;;
;;
;;(let ((accept-motion t)
;;      (selected-frame nil))
;;  (defun mouse-motion (root-x root-y)
;;    "Move or resize frame. Move window from a frame to another.
;;Go to top left or rigth corner to change workspaces."
;;    (let ((frame (find-frame-under-mouse root-x root-y)))
;;      (unless (equal selected-frame frame)
;;	(select-frame-under-mouse root-x root-y)
;;	(setf selected-frame frame)))
;;    (if (<= root-y 5)
;;	(cond ((and accept-motion (<= root-x 5))
;;	       (case *motion-action*
;;		 (:move-frame
;;		  (remove-frame-in-workspace *motion-object* (current-workspace))))
;;	       (circulate-workspace-down)
;;	       (minimize-frame (current-frame))
;;	       (case *motion-action*
;;		 (:move-frame
;;		  (add-frame-in-workspace *motion-object* (current-workspace))))
;;	       (warp-pointer *root* (1- (xlib:screen-width *screen*)) 100)
;;	       (setf accept-motion nil))
;;	      ((and accept-motion (>= root-x (- (xlib:screen-width *screen*) 5)))
;;	       (case *motion-action*
;;		 (:move-frame
;;		  (remove-frame-in-workspace *motion-object* (current-workspace))))
;;	       (circulate-workspace-up)
;;	       (minimize-frame (current-frame))
;;	       (case *motion-action*
;;		 (:move-frame
;;		  (add-frame-in-workspace *motion-object* (current-workspace))))
;;	       (warp-pointer *root* 0 100)
;;	       (setf accept-motion nil))
;;	      (t (setf accept-motion t)))
;;	(setf accept-motion t))
;;    (case *motion-action*
;;      (:move-frame
;;       (hide-frame *root* *motion-object*)
;;       (setf (frame-x *motion-object*) (+ root-x *motion-dx*)
;;	     (frame-y *motion-object*) (+ root-y *motion-dy*))
;;       (show-frame *root* *root-gc* *motion-object*)
;;       (adapt-all-window-in-frame *motion-object*)
;;       (show-all-frame (current-workspace) nil))
;;      (:resize-frame
;;       (hide-frame *root* *motion-object*)
;;       (setf (frame-width *motion-object*) (max (+ (frame-width *motion-object*) (- root-x *motion-dx*)) 100)
;;	     (frame-height *motion-object*) (max (+ (frame-height *motion-object*) (- root-y *motion-dy*)) 100)
;;	     *motion-dx* root-x *motion-dy* root-y)
;;       (show-frame *root* *root-gc* *motion-object*)
;;       (adapt-all-window-in-frame *motion-object*)
;;       (show-all-frame (current-workspace) nil)))))
;;
;;
;;
;;(defun move-selected-frame (root-x root-y)
;;  "Move selected frame or create a new frame on the root window"
;;  (select-frame-under-mouse root-x root-y)
;;  (setf *motion-object* (find-frame-under-mouse root-x root-y))
;;  (if *motion-object*
;;      (setf *motion-action* :move-frame
;;	    *motion-dx* (- (frame-x *motion-object*) root-x)
;;	    *motion-dy* (- (frame-y *motion-object*) root-y))
;;      (progn
;;	(setf *motion-object* (make-frame :x root-x :y root-y :width 100 :height 100 :fullscreenp nil))
;;	(warp-pointer *root* (+ root-x 100) (+ root-y 100))
;;	(add-frame-in-workspace *motion-object* (current-workspace))
;;	(show-all-frame (current-workspace))
;;	(setf *motion-action* :resize-frame
;;	      *motion-dx* (+ root-x 100)
;;	      *motion-dy* (+ root-y 100)))))
;;
;;
;;
;;(defun copy-selected-frame (root-x root-y)
;;  "Copy selected frame"
;;  (xgrab-pointer *root* 50 51)
;;  (select-frame-under-mouse root-x root-y)
;;  (setf *motion-object* (find-frame-under-mouse root-x root-y))
;;  (when *motion-object*
;;    (setf *motion-action* :copy-frame
;;	  *motion-object* (copy-frame *motion-object*)
;;	  *motion-dx* (- (frame-x *motion-object*) root-x)
;;	  *motion-dy* (- (frame-y *motion-object*) root-y))))
;;;;    (add-frame-in-workspace *motion-object* (current-workspace))))
;;
;;
;;
;;(defun release-move-selected-frame (root-x root-y)
;;  "Release button"
;;  (when *motion-object*
;;    (case *motion-action*
;;      (:move-frame
;;       (move-frame-to *motion-object* (+ root-x *motion-dx*) (+ root-y *motion-dy*)))
;;      (:resize-frame
;;       (resize-frame *motion-object* 0 0))))
;;  (init-motion-vars)
;;  (select-frame-under-mouse root-x root-y))
;;
;;
;;(defun release-copy-selected-frame (root-x root-y)
;;  "Release button"
;;  (xgrab-pointer *root* 66 67)
;;  (when *motion-object*
;;    (unless (frame-windows-already-in-workspace *motion-object* (current-workspace))
;;      (add-frame-in-workspace *motion-object* (current-workspace))
;;      (move-frame-to *motion-object* (+ root-x *motion-dx*) (+ root-y *motion-dy*))))
;;  (init-motion-vars)
;;  (select-frame-under-mouse root-x root-y)
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;(defun resize-selected-frame (root-x root-y)
;;  "Resize selected frame"
;;  (select-frame-under-mouse root-x root-y)
;;  (setf *motion-object* (find-frame-under-mouse root-x root-y))
;;  (when *motion-object*
;;    (setf *motion-action* :resize-frame
;;	  *motion-dx* root-x
;;	  *motion-dy* root-y)))
;;
;;
;;(defun release-resize-selected-frame (root-x root-y)
;;  "Release button"
;;  (when *motion-object*
;;    (resize-frame *motion-object* 0 0))
;;  (init-motion-vars)
;;  (select-frame-under-mouse root-x root-y))
;;
;;
;;
;;(defun move-selected-window (root-x root-y)
;;  "Move selected window"
;;  (xgrab-pointer *root* 50 51)
;;  (select-frame-under-mouse root-x root-y)
;;  (setf *motion-object* (current-window)
;;	*motion-action* :move-window)
;;  (when *motion-object*
;;    (setf *motion-start-frame* (current-frame))))
;;
;;
;;(defun release-move-selected-window (root-x root-y)
;;  "Release button"
;;  (xgrab-pointer *root* 66 67)
;;  (select-frame-under-mouse root-x root-y)
;;  (when *motion-object*
;;    (remove-window-in-frame *motion-object* *motion-start-frame*)
;;    (add-window-in-frame *motion-object* (current-frame)))
;;  (init-motion-vars)
;;  (select-frame-under-mouse root-x root-y)
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;(defun copy-selected-window (root-x root-y)
;;  "Copy selected window"
;;  (move-selected-window root-x root-y)
;;  (setf *motion-action* :copy-window))
;;
;;(defun release-copy-selected-window (root-x root-y)
;;  "Release button"
;;  (xgrab-pointer *root* 66 67)
;;  (select-frame-under-mouse root-x root-y)
;;  (when *motion-object*
;;    (unless (window-already-in-workspace *motion-object* (current-workspace))
;;      (add-window-in-frame *motion-object* (current-frame))))
;;  (init-motion-vars)
;;  (select-frame-under-mouse root-x root-y)
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;
;;
;;
;;(define-second-mouse (1) 'move-selected-frame 'release-move-selected-frame)
;;(define-second-mouse (1 :mod-1) 'resize-selected-frame 'release-resize-selected-frame)
;;(define-second-mouse (1 :control) 'copy-selected-frame 'release-copy-selected-frame)
;;
;;(define-second-mouse (2) nil 'mouse-leave-second-mode-maximize)
;;(define-second-mouse (2 :control) nil 'mouse-leave-second-mode)
;;
;;(define-second-mouse (3) 'move-selected-window 'release-move-selected-window)
;;(define-second-mouse (3  :control) 'copy-selected-window 'release-copy-selected-window)
;;
;;
;;(define-second-mouse (4) 'mouse-circulate-window-up nil)
;;(define-second-mouse (5) 'mouse-circulate-window-down nil)
;;
;;(define-second-mouse (4 :mod-1) 'mouse-circulate-workspace-up nil)
;;(define-second-mouse (5 :mod-1) 'mouse-circulate-workspace-down nil)
;;
;;(define-second-mouse ('Motion) 'mouse-motion nil)

