;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; #Date#: Thu Feb 28 21:38:00 2008
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
(defun group-adding-menu ()
  "Adding group menu"
  (info-mode-menu '((#\a add-default-group)
		    (#\p add-placed-group))))

(defun group-layout-menu ()
  "Group layout menu"
  (info-mode-menu (loop for l in *layout-list*
		     for i from 0
		     collect (list (code-char (+ (char-code #\a) i)) l))))


  


(defun group-pack-menu ()
  "Group pack menu"
  (info-mode-menu '(("Up" group-pack-up)
		    ("Down" group-pack-down))))


(defun group-movement-menu ()
  "Group movement menu"
  (info-mode-menu '((#\p group-pack-menu)
		    (#\f group-fill-menu)
		    (#\r group-resize-menu))))


(defun group-pack-up ()
  "Pack group up"
  (print 'pack-up)
  (group-movement-menu))

(defun group-pack-down ()
  "Pack group down"
  (print 'pack-down)
  (group-movement-menu))







(defun action-by-name-menu ()
  "Actions by name menu"
  (info-mode-menu '((#\f focus-group-by-name)
		    (#\o open-group-by-name)
		    (#\d delete-group-by-name)
		    (#\m move-current-child-by-name)
		    (#\c copy-current-child-by-name))))

(defun action-by-number-menu ()
  "Actions by number menu"
  (info-mode-menu '((#\f focus-group-by-number)
		    (#\o open-group-by-number)
		    (#\d delete-group-by-number)
		    (#\m move-current-child-by-number)
		    (#\c copy-current-child-by-number))))


(defun group-menu ()
  "Group menu"
  (info-mode-menu '((#\a group-adding-menu)
		    (#\l group-layout-menu)
		    (#\m group-movement-menu))))



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
  (info-mode-menu '((#\g group-menu)
		    ;;(#\w window-menu)
		    (#\s selection-menu)
		    (#\n action-by-name-menu)
		    (#\u action-by-number-menu)
		    (#\y utility-menu))))






(define-second-key ("F1" :mod-1) 'help-on-second-mode)

(define-second-key ("m") 'main-menu)
(define-second-key ("g") 'group-menu)
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
(define-second-key ("Tab" :mod-1 :shift) 'select-previous-child)

(define-second-key ("Return" :mod-1) 'enter-group)
(define-second-key ("Return" :mod-1 :shift) 'leave-group)

(define-second-key ("Home" :mod-1) 'switch-to-root-group)
(define-second-key ("Home" :mod-1 :shift) 'switch-and-select-root-group)

(define-second-key ("Menu") 'toggle-show-root-group)

(define-second-key (#\b :mod-1) 'banish-pointer)


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





(defun sm-handle-click-to-focus (root-x root-y)
  "Give the focus to the clicked child"
  (let ((win (find-child-under-mouse root-x root-y)))
    (handle-click-to-focus win)))

(define-mouse-action (1) 'sm-handle-click-to-focus)






;;;; Escape
;;(define-second-key ("Escape" :control :shift) 'delete-current-window)
;;(define-second-key ("Escape" :mod-1 :control :shift) 'destroy-current-window)
;;(define-second-key ("Escape" :control) 'remove-current-window)
;;(define-second-key ("Escape" :shift) 'unhide-all-windows-in-current-group)
;;
;;
;;;; Up
;;(define-second-key ("Up" :mod-1) 'circulate-group-up)
;;(define-second-key ("Up" :mod-1 :shift) 'circulate-group-up-move-window)
;;(define-second-key ("Up" :mod-1 :shift :control) 'circulate-group-up-copy-window)
;;
;;
;;;; Down
;;(define-second-key ("Down" :mod-1) 'circulate-group-down)
;;(define-second-key ("Down" :mod-1 :shift) 'circulate-group-down-move-window)
;;(define-second-key ("Down" :mod-1 :shift :control) 'circulate-group-down-copy-window)
;;
;;
;;;; Right
;;(define-second-key ("Right" :mod-1) 'circulate-workspace-up)
;;(define-second-key ("Right" :mod-1 :shift) 'circulate-workspace-up-move-group)
;;(define-second-key ("Right" :mod-1 :shift :control) 'circulate-workspace-up-copy-group)
;;
;;
;;;; Left
;;(define-second-key ("Left" :mod-1) 'circulate-workspace-down)
;;(define-second-key ("Left" :mod-1 :shift) 'circulate-workspace-down-move-group)
;;(define-second-key ("Left" :mod-1 :shift :control) 'circulate-workspace-down-copy-group)
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
;;(define-second-key (#\b :mod-1) 'toggle-maximize-current-group)
;;
;;(define-second-key (#\x) 'pager-mode)
;;
;;
;;(define-second-key (#\k :mod-1) 'destroy-current-window)
;;(define-second-key (#\k) 'remove-current-window)
;;
;;
;;(define-second-key (#\g) 'create-new-default-group)
;;(define-second-key (#\g :mod-1) 'remove-current-group)
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
;;    (defun b-open-next-window-in-new-group-once ()
;;      "Open the next window in a new group and all others in the same group"
;;      (setf *open-next-window-in-new-group* :once)
;;      (leave-second-mode)))
;;
;;(define-second-key (#\o :mod-1 :control)
;;    (defun b-open-next-window-in-new-group ()
;;      "Open each next window in a new group"
;;      (setf *open-next-window-in-new-group* t)
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
;;(define-second-key (#\a) 'force-window-center-in-group)
;;(define-second-key (#\a :mod-1) 'force-window-in-group)
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
;;(define-second-key (#\y :control) 'explode-current-group)
;;(define-second-key (#\y :control :shift) 'implode-current-group)
;;    
;;;;;,-----
;;;;;| Moving/Resizing groups
;;;;;`-----
;;(define-second-key (#\p)
;;    (defun b-pack-group-on-next-arrow ()
;;      "Pack group on next arrow action"
;;      (setf *arrow-action* :pack)))
;;
;;
;;(defun fill-group-in-all-directions ()
;;  "Fill group in all directions"
;;  (fill-current-group-up)
;;  (fill-current-group-left)
;;  (fill-current-group-right)
;;  (fill-current-group-down))
;;
;;
;;(define-second-key (#\f)
;;    (defun b-fill-group ()
;;      "Fill group on next arrow action (fill in all directions on second f keypress)"
;;      (case *arrow-action*
;;	(:fill (fill-group-in-all-directions)
;;	       (setf *arrow-action* nil))
;;	(t (setf *arrow-action* :fill)))))
;;
;;(define-second-key (#\f :mod-1) 'fill-group-in-all-directions)
;;
;;(define-second-key (#\f :shift)
;;    (defun b-fill-group-vert ()
;;      "Fill group vertically"
;;      (fill-current-group-up)
;;      (fill-current-group-down)))
;;
;;(define-second-key (#\f :control)
;;    (defun b-fill-group-horiz ()
;;      "Fill group horizontally"
;;      (fill-current-group-left)
;;      (fill-current-group-right)))
;;
;;
;;(define-second-key (#\r)
;;    (defun b-resize-half ()
;;      "Resize group to its half width or heigth on next arraw action"
;;      (setf *arrow-action* :resize-half)))
;;
;;
;;(define-second-key (#\l) 'resize-minimal-current-group)
;;(define-second-key (#\l :mod-1) 'resize-down-current-group)
;;
;;
;;(define-second-key (#\m) 'center-current-group)
;;   
;;
;;(define-second-key ("Up")
;;    (defun b-move-or-pack-up ()
;;      "Move, pack, fill or resize group up"
;;      (case *arrow-action*
;;	(:pack (pack-current-group-up))
;;	(:fill (fill-current-group-up))
;;	(:resize-half (resize-half-height-up-current-group))
;;	(t (move-group (current-group) 0 -10)))
;;      (setf *arrow-action* nil)))
;;
;;(define-second-key ("Down")
;;    (defun b-move-or-pack-down ()
;;      "Move, pack, fill or resize group down"
;;      (case *arrow-action*
;;	(:pack (pack-current-group-down))
;;	(:fill (fill-current-group-down))
;;	(:resize-half (resize-half-height-down-current-group))
;;	(t (move-group (current-group) 0 +10)))
;;      (setf *arrow-action* nil)))
;;
;;(define-second-key ("Right")
;;    (defun b-move-or-pack-right ()
;;      "Move, pack, fill or resize group right"
;;      (case *arrow-action*
;;	(:pack (pack-current-group-right))
;;	(:fill (fill-current-group-right))
;;	(:resize-half (resize-half-width-right-current-group))
;;	(t (move-group (current-group) +10 0)))
;;      (setf *arrow-action* nil)))
;;
;;(define-second-key ("Left")
;;    (defun b-move-or-pack-left ()
;;      "Move, pack, fill or resize group left"
;;      (case *arrow-action*
;;	(:pack (pack-current-group-left))
;;	(:fill (fill-current-group-left))
;;	(:resize-half (resize-half-width-left-current-group))
;;	(t (move-group (current-group) -10 0)))
;;      (setf *arrow-action* nil)))
;;
;;
;;(define-second-key ("Up" :shift)
;;    (defun b-resize-up ()
;;      "Resize group up"
;;      (resize-group (current-group) 0 -10)))
;;
;;(define-second-key ("Down" :shift)
;;    (defun b-resize-down ()
;;      "Resize group down"
;;      (resize-group (current-group) 0 +10)))
;;
;;(define-second-key ("Right" :shift)
;;    (defun b-resize-right ()
;;      "Resize group right"
;;      (resize-group (current-group) +10 0)))
;;
;;(define-second-key ("Left" :shift)
;;    (defun b-resize-left ()
;;      "Resize group left"
;;      (resize-group (current-group) -10 0)))
;;
;;
;;;;;,-----
;;;;;| Mouse second mode functions
;;;;;`-----
;;(defun select-group-under-mouse (root-x root-y)
;;  (let ((group (find-group-under-mouse root-x root-y)))
;;    (when group
;;      (no-focus)
;;      (focus-group group (current-workspace))
;;      (focus-window (current-window))
;;      (show-all-group (current-workspace) nil))))
;;
;;(defun mouse-leave-second-mode-maximize (root-x root-y)
;;  "Leave second mode and maximize current group"
;;  (select-group-under-mouse root-x root-y)
;;  (maximize-group (current-group))
;;  (show-all-windows-in-workspace (current-workspace))
;;  (throw 'exit-second-loop nil))
;;
;;(defun mouse-leave-second-mode (root-x root-y)
;;  "Leave second mode"
;;  (select-group-under-mouse root-x root-y)
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
;;	*motion-start-group* nil
;;	*motion-dx* nil
;;	*motion-dy* nil))
;;
;;
;;(let ((accept-motion t)
;;      (selected-group nil))
;;  (defun mouse-motion (root-x root-y)
;;    "Move or resize group. Move window from a group to another.
;;Go to top left or rigth corner to change workspaces."
;;    (let ((group (find-group-under-mouse root-x root-y)))
;;      (unless (equal selected-group group)
;;	(select-group-under-mouse root-x root-y)
;;	(setf selected-group group)))
;;    (if (<= root-y 5)
;;	(cond ((and accept-motion (<= root-x 5))
;;	       (case *motion-action*
;;		 (:move-group
;;		  (remove-group-in-workspace *motion-object* (current-workspace))))
;;	       (circulate-workspace-down)
;;	       (minimize-group (current-group))
;;	       (case *motion-action*
;;		 (:move-group
;;		  (add-group-in-workspace *motion-object* (current-workspace))))
;;	       (warp-pointer *root* (1- (xlib:screen-width *screen*)) 100)
;;	       (setf accept-motion nil))
;;	      ((and accept-motion (>= root-x (- (xlib:screen-width *screen*) 5)))
;;	       (case *motion-action*
;;		 (:move-group
;;		  (remove-group-in-workspace *motion-object* (current-workspace))))
;;	       (circulate-workspace-up)
;;	       (minimize-group (current-group))
;;	       (case *motion-action*
;;		 (:move-group
;;		  (add-group-in-workspace *motion-object* (current-workspace))))
;;	       (warp-pointer *root* 0 100)
;;	       (setf accept-motion nil))
;;	      (t (setf accept-motion t)))
;;	(setf accept-motion t))
;;    (case *motion-action*
;;      (:move-group
;;       (hide-group *root* *motion-object*)
;;       (setf (group-x *motion-object*) (+ root-x *motion-dx*)
;;	     (group-y *motion-object*) (+ root-y *motion-dy*))
;;       (show-group *root* *root-gc* *motion-object*)
;;       (adapt-all-window-in-group *motion-object*)
;;       (show-all-group (current-workspace) nil))
;;      (:resize-group
;;       (hide-group *root* *motion-object*)
;;       (setf (group-width *motion-object*) (max (+ (group-width *motion-object*) (- root-x *motion-dx*)) 100)
;;	     (group-height *motion-object*) (max (+ (group-height *motion-object*) (- root-y *motion-dy*)) 100)
;;	     *motion-dx* root-x *motion-dy* root-y)
;;       (show-group *root* *root-gc* *motion-object*)
;;       (adapt-all-window-in-group *motion-object*)
;;       (show-all-group (current-workspace) nil)))))
;;
;;
;;
;;(defun move-selected-group (root-x root-y)
;;  "Move selected group or create a new group on the root window"
;;  (select-group-under-mouse root-x root-y)
;;  (setf *motion-object* (find-group-under-mouse root-x root-y))
;;  (if *motion-object*
;;      (setf *motion-action* :move-group
;;	    *motion-dx* (- (group-x *motion-object*) root-x)
;;	    *motion-dy* (- (group-y *motion-object*) root-y))
;;      (progn
;;	(setf *motion-object* (make-group :x root-x :y root-y :width 100 :height 100 :fullscreenp nil))
;;	(warp-pointer *root* (+ root-x 100) (+ root-y 100))
;;	(add-group-in-workspace *motion-object* (current-workspace))
;;	(show-all-group (current-workspace))
;;	(setf *motion-action* :resize-group
;;	      *motion-dx* (+ root-x 100)
;;	      *motion-dy* (+ root-y 100)))))
;;
;;
;;
;;(defun copy-selected-group (root-x root-y)
;;  "Copy selected group"
;;  (xgrab-pointer *root* 50 51)
;;  (select-group-under-mouse root-x root-y)
;;  (setf *motion-object* (find-group-under-mouse root-x root-y))
;;  (when *motion-object*
;;    (setf *motion-action* :copy-group
;;	  *motion-object* (copy-group *motion-object*)
;;	  *motion-dx* (- (group-x *motion-object*) root-x)
;;	  *motion-dy* (- (group-y *motion-object*) root-y))))
;;;;    (add-group-in-workspace *motion-object* (current-workspace))))
;;
;;
;;
;;(defun release-move-selected-group (root-x root-y)
;;  "Release button"
;;  (when *motion-object*
;;    (case *motion-action*
;;      (:move-group
;;       (move-group-to *motion-object* (+ root-x *motion-dx*) (+ root-y *motion-dy*)))
;;      (:resize-group
;;       (resize-group *motion-object* 0 0))))
;;  (init-motion-vars)
;;  (select-group-under-mouse root-x root-y))
;;
;;
;;(defun release-copy-selected-group (root-x root-y)
;;  "Release button"
;;  (xgrab-pointer *root* 66 67)
;;  (when *motion-object*
;;    (unless (group-windows-already-in-workspace *motion-object* (current-workspace))
;;      (add-group-in-workspace *motion-object* (current-workspace))
;;      (move-group-to *motion-object* (+ root-x *motion-dx*) (+ root-y *motion-dy*))))
;;  (init-motion-vars)
;;  (select-group-under-mouse root-x root-y)
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;(defun resize-selected-group (root-x root-y)
;;  "Resize selected group"
;;  (select-group-under-mouse root-x root-y)
;;  (setf *motion-object* (find-group-under-mouse root-x root-y))
;;  (when *motion-object*
;;    (setf *motion-action* :resize-group
;;	  *motion-dx* root-x
;;	  *motion-dy* root-y)))
;;
;;
;;(defun release-resize-selected-group (root-x root-y)
;;  "Release button"
;;  (when *motion-object*
;;    (resize-group *motion-object* 0 0))
;;  (init-motion-vars)
;;  (select-group-under-mouse root-x root-y))
;;
;;
;;
;;(defun move-selected-window (root-x root-y)
;;  "Move selected window"
;;  (xgrab-pointer *root* 50 51)
;;  (select-group-under-mouse root-x root-y)
;;  (setf *motion-object* (current-window)
;;	*motion-action* :move-window)
;;  (when *motion-object*
;;    (setf *motion-start-group* (current-group))))
;;
;;
;;(defun release-move-selected-window (root-x root-y)
;;  "Release button"
;;  (xgrab-pointer *root* 66 67)
;;  (select-group-under-mouse root-x root-y)
;;  (when *motion-object*
;;    (remove-window-in-group *motion-object* *motion-start-group*)
;;    (add-window-in-group *motion-object* (current-group)))
;;  (init-motion-vars)
;;  (select-group-under-mouse root-x root-y)
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
;;  (select-group-under-mouse root-x root-y)
;;  (when *motion-object*
;;    (unless (window-already-in-workspace *motion-object* (current-workspace))
;;      (add-window-in-group *motion-object* (current-group))))
;;  (init-motion-vars)
;;  (select-group-under-mouse root-x root-y)
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;
;;
;;
;;(define-mouse-action (1) 'move-selected-group 'release-move-selected-group)
;;(define-mouse-action (1 :mod-1) 'resize-selected-group 'release-resize-selected-group)
;;(define-mouse-action (1 :control) 'copy-selected-group 'release-copy-selected-group)
;;
;;(define-mouse-action (2) nil 'mouse-leave-second-mode-maximize)
;;(define-mouse-action (2 :control) nil 'mouse-leave-second-mode)
;;
;;(define-mouse-action (3) 'move-selected-window 'release-move-selected-window)
;;(define-mouse-action (3  :control) 'copy-selected-window 'release-copy-selected-window)
;;
;;
;;(define-mouse-action (4) 'mouse-circulate-window-up nil)
;;(define-mouse-action (5) 'mouse-circulate-window-down nil)
;;
;;(define-mouse-action (4 :mod-1) 'mouse-circulate-workspace-up nil)
;;(define-mouse-action (5 :mod-1) 'mouse-circulate-workspace-down nil)
;;
;;(define-mouse-action ('Motion) 'mouse-motion nil)

