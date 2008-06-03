;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Utility
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

(defun load-contrib (file)
  "Load a file in the contrib directory"
  (let ((truename (concatenate 'string *contrib-dir* "contrib/" file)))
    (format t "Loading contribution file: ~A~%" truename)
    (when (probe-file truename)
      (load truename))))




(defun rename-current-child ()
  "Rename the current child"
  (let ((name (query-string (format nil "New child name: (last: ~A)" (child-name *current-child*))
			    (child-name *current-child*))))
    (rename-child *current-child* name)
    (leave-second-mode)))


(defun renumber-current-frame ()
  "Renumber the current frame"
  (when (frame-p *current-child*)
    (let ((number (query-number (format nil "New child number: (last: ~A)" (frame-number *current-child*))
				(frame-number *current-child*))))
      (setf (frame-number *current-child*) number)
      (leave-second-mode))))

    


(defun add-default-frame ()
  "Add a default frame in the current frame"
  (when (frame-p *current-child*)
    (let ((name (query-string "Frame name")))
      (push (create-frame :name name) (frame-child *current-child*))))
  (leave-second-mode))
    

(defun add-placed-frame ()
  "Add a placed frame in the current frame"
  (when (frame-p *current-child*)
    (let ((name (query-string "Frame name"))
	  (x (/ (query-number "Frame x in percent (%)") 100))
	  (y (/ (query-number "Frame y in percent (%)") 100))
	  (w (/ (query-number "Frame width in percent (%)") 100))
	  (h (/ (query-number "Frame height in percent (%)") 100)))
      (push (create-frame :name name :x x :y y :w w :h h)
	    (frame-child *current-child*))))
  (leave-second-mode))



(defun delete-focus-window ()
  "Delete the focus window in all frames and workspaces"
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (setf *current-child* *current-root*)
      (remove-child-in-all-frames window)
      (send-client-message window :WM_PROTOCOLS
			   (xlib:intern-atom *display* "WM_DELETE_WINDOW"))
      (show-all-children))))

(defun destroy-focus-window ()
  "Destroy the focus window in all frames and workspaces"
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (setf *current-child* *current-root*)
      (remove-child-in-all-frames window)
      (xlib:kill-client *display* (xlib:window-id window))
      (show-all-children))))

(defun remove-focus-window ()
  "Remove the focus window in the current frame"
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (setf *current-child* *current-root*)
      (hide-child window)
      (remove-child-in-frame window (find-parent-frame window))
      (show-all-children))))


(defun unhide-all-windows-in-current-child ()
  "Unhide all hidden windows into the current child"
  (with-xlib-protect
    (dolist (window (get-hidden-windows))
      (unhide-window window)
      (process-new-window window)
      (xlib:map-window window)))
  (show-all-children))




(defun find-window-under-mouse (x y)
  "Return the child window under the mouse"
  (with-xlib-protect
    (let ((win *root*))
      (with-all-windows-frames-and-parent (*current-root* child parent)
	(when (and (or (managed-window-p child parent) (equal parent *current-child*))
		   (<= (xlib:drawable-x child) x (+ (xlib:drawable-x child) (xlib:drawable-width child)))
		   (<= (xlib:drawable-y child) y (+ (xlib:drawable-y child) (xlib:drawable-height child))))
	  (setf win child))
	(when (and (<= (frame-rx child) x (+ (frame-rx child) (frame-rw child)))
		   (<= (frame-ry child) y (+ (frame-ry child) (frame-rh child))))
	  (setf win (frame-window child))))
      win)))


(defun find-child-under-mouse (x y)
  "Return the child under the mouse"
  (with-xlib-protect
    (let ((ret nil))
      (with-all-windows-frames-and-parent (*current-root* child parent)
	(when (and (or (managed-window-p child parent) (equal parent *current-child*))
		   (<= (xlib:drawable-x child) x (+ (xlib:drawable-x child) (xlib:drawable-width child)))
		   (<= (xlib:drawable-y child) y (+ (xlib:drawable-y child) (xlib:drawable-height child))))
	  (setf ret child))
	(when (and (<= (frame-rx child) x (+ (frame-rx child) (frame-rw child)))
		   (<= (frame-ry child) y (+ (frame-ry child) (frame-rh child))))
	  (setf ret child)))
      ret)))






;;; Selection functions
(defun clear-selection ()
  "Clear the current selection"
  (setf *child-selection* nil)
  (display-frame-info *current-root*))

(defun copy-current-child ()
  "Copy the current child to the selection"
  (pushnew *current-child* *child-selection*)
  (display-frame-info *current-root*))


(defun cut-current-child ()
  "Cut the current child to the selection"
  (copy-current-child)
  (hide-all *current-child*)
  (remove-child-in-frame *current-child* (find-parent-frame *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (show-all-children))

(defun remove-current-child ()
  "Remove the current child from its parent frame"
  (hide-all *current-child*)
  (remove-child-in-frame *current-child* (find-parent-frame *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (leave-second-mode))


(defun remove-current-child-from-tree ()
  "Remove the current child from the CLFSWM tree"
  (remove-child-in-frame *current-child* (find-parent-frame *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (leave-second-mode))



(defun paste-selection-no-clear ()
  "Paste the selection in the current frame - Do not clear the selection after paste"
  (let ((frame-dest (typecase *current-child*
		      (xlib:window (find-parent-frame *current-child* *current-root*))
		      (frame *current-child*))))
    (when frame-dest
      (dolist (child *child-selection*)
	(unless (find-child-in-parent child frame-dest)
	  (pushnew child (frame-child frame-dest))))
      (show-all-children))))

(defun paste-selection ()
  "Paste the selection in the current frame"
  (paste-selection-no-clear)
  (setf *child-selection* nil)
  (display-frame-info *current-root*))



  



;;; CONFIG - Identify mode
(defun identify-key ()
  "Identify a key"
  (let* ((done nil)
	 (font (xlib:open-font *display* *identify-font-string*))
	 (window (xlib:create-window :parent *root*
				     :x 0 :y 0
				     :width (- (xlib:screen-width *screen*) 2)
				     :height (* 5 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font)))
				     :background (get-color *identify-background*)
				     :border-width 1
				     :border (get-color *identify-border*)
				     :colormap (xlib:screen-default-colormap *screen*)
				     :event-mask '(:exposure)))
	 (gc (xlib:create-gcontext :drawable window
				   :foreground (get-color *identify-foreground*)
				   :background (get-color *identify-background*)
				   :font font
				   :line-style :solid)))
    (labels ((print-doc (msg hash-table-key pos code state)
	       (let ((function (find-key-from-code hash-table-key code state)))
		 (when (and function (fboundp (first function)))
		   (xlib:draw-glyphs *pixmap-buffer* gc 10 (+ (* pos (+ (xlib:max-char-ascent font) (xlib:max-char-descent font))) 5)
				     (format nil "~A ~A" msg (documentation (first function) 'function))))))
	     (print-key (code state keysym key modifiers)
	       (clear-pixmap-buffer window gc)
	       (setf (xlib:gcontext-foreground gc) (get-color *identify-foreground*))
	       (xlib:draw-glyphs *pixmap-buffer* gc 5 (+ (xlib:max-char-ascent font) 5)
				 (format nil "Press a key to identify. Press 'q' to stop the identify loop."))
	       (when code
		 (xlib:draw-glyphs *pixmap-buffer* gc 10 (+ (* 2 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font))) 5)
				   (format nil "Code=~A  KeySym=~S  Key=~S  Modifiers=~A"
					   code keysym key modifiers))
		 (print-doc "Main mode  : " *main-keys* 3 code state)
		 (print-doc "Second mode: " *second-keys* 4 code state))
	       (copy-pixmap-buffer window gc))
	     (handle-identify-key (&rest event-slots &key root code state &allow-other-keys)
	       (declare (ignore event-slots root))
	       (let* ((modifiers (state->modifiers state))
		      (key (keycode->char code state))
		      (keysym (keysym->keysym-name (xlib:keycode->keysym *display* code (cond  ((member :shift modifiers) 1)
											       ((member :mod-5 modifiers) 2)
											       (t 0))))))
		 (setf done (and (equal key #\q) (null modifiers)))
		 (dbg code keysym key modifiers)
		 (print-key code state keysym key modifiers)
		 (force-output)))
	     (handle-identify (&rest event-slots &key display event-key &allow-other-keys)
	       (declare (ignore display))
	       (case event-key
		 (:key-press (apply #'handle-identify-key event-slots) t)
		 (:exposure (print-key nil nil nil nil nil)))
	       t))
      (xgrab-pointer *root* 92 93)
      (xlib:map-window window)
      (format t "~&Press 'q' to stop the identify loop~%")
      (print-key nil nil nil nil nil)
      (force-output)
      (unwind-protect
	   (loop until done do
		(xlib:display-finish-output *display*)
		(xlib:process-event *display* :handler #'handle-identify))
	(xlib:destroy-window window)
	(xlib:close-font font)
	(xgrab-pointer *root* 66 67)))))






(defun eval-from-query-string ()
  "Eval a lisp form from the query input"
  (let ((form (query-string "Eval:"))
	(result nil))
    (when (and form (not (equal form "")))
      (let ((printed-result
	     (with-output-to-string (*standard-output*)
	       (setf result (handler-case
				(loop for i in (multiple-value-list
						(eval (read-from-string form)))
				   collect (format nil "~S" i))
			      (error (condition)
				(format nil "~A" condition)))))))
	(info-mode (expand-newline (append (ensure-list (format nil "> ~A" form))
					   (ensure-list printed-result)
					   (ensure-list result)))
		   :width (- (xlib:screen-width *screen*) 2))
	(eval-from-query-string)))))




(defun run-program-from-query-string ()
  "Run a program from the query input"
  (let ((program (query-string "Run:")))
    (when (and program (not (equal program "")))
      (setf *second-mode-program* program)
      (leave-second-mode))))




;;; Frame name actions
;;;(loop :for str :in '("The Gimp" "The klm" "klm" "abc")  ;; Test
;;;   :when (zerop (or (search "ThE" str :test #'string-equal) -1))
;;;   :collect str)
(defun ask-frame-name (msg)
  "Ask a frame name"
  (let ((all-frame-name nil)
	(name ""))
    (with-all-frames (*root-frame* frame)
      (awhen (frame-name frame) (push it all-frame-name)))
    (labels ((selected-names ()
	       (loop :for str :in all-frame-name
		  :when (zerop (or (search name str :test #'string-equal) -1))
		  :collect str))
	     (complet-alone (req sel)
	       (if (= 1 (length sel)) (first sel) req))
	     (ask ()
	       (let* ((selected (selected-names))
		      (default (complet-alone name selected)))
		 (multiple-value-bind (str done)
		     (query-string (format nil "~A: ~{~A~^, ~}" msg selected) default)
		   (setf name str)
		   (when (or (not (string-equal name default)) (eql done :complet))
		     (ask))))))
      (ask))
    name))



;;; Focus by functions
(defun focus-frame-by (frame)
  (when (frame-p frame)
    (hide-all *current-root*)
    (focus-all-children frame (or (find-parent-frame frame *current-root*)
				  (find-parent-frame frame)
				  *root-frame*))
    (show-all-children *current-root*)))


(defun focus-frame-by-name ()
  "Focus a frame by name"
  (focus-frame-by (find-frame-by-name (ask-frame-name "Focus frame")))
  (leave-second-mode))

(defun focus-frame-by-number ()
  "Focus a frame by number"
  (focus-frame-by (find-frame-by-number (query-number "Focus frame by number:")))
  (leave-second-mode))


;;; Open by functions
(defun open-frame-by (frame)
  (when (frame-p frame)
    (push (create-frame :name (query-string "Frame name")) (frame-child frame))
    (show-all-children *current-root*)))



(defun open-frame-by-name ()
  "Open a new frame in a named frame"
  (open-frame-by (find-frame-by-name (ask-frame-name "Open a new frame in")))
  (leave-second-mode))

(defun open-frame-by-number ()
  "Open a new frame in a numbered frame"
  (open-frame-by (find-frame-by-number (query-number "Open a new frame in the group numbered:")))
  (leave-second-mode))


;;; Delete by functions
(defun delete-frame-by (frame)
  (hide-all *current-root*)
  (unless (equal frame *root-frame*)
    (when (equal frame *current-root*)
      (setf *current-root* *root-frame*))
    (when (equal frame *current-child*)
      (setf *current-child* *current-root*))
    (remove-child-in-frame frame (find-parent-frame frame)))
  (show-all-children *current-root*))


(defun delete-frame-by-name ()
  "Delete a frame by name"
  (delete-frame-by (find-frame-by-name (ask-frame-name "Delete frame")))
  (leave-second-mode))

(defun delete-frame-by-number ()
  "Delete a frame by number"
  (delete-frame-by (find-frame-by-number (query-number "Delete frame by number:")))
  (leave-second-mode))


;;; Move by function
(defun move-child-to (child frame-dest)
  (when (and child (frame-p frame-dest))
    (hide-all *current-root*)
    (remove-child-in-frame child (find-parent-frame child))
    (pushnew child (frame-child frame-dest))
    (focus-all-children child frame-dest)
    (show-all-children *current-root*)))

(defun move-current-child-by-name ()
  "Move current child in a named frame"
  (move-child-to *current-child*
		 (find-frame-by-name
		  (ask-frame-name (format nil "Move '~A' to frame" (child-name *current-child*)))))
  (leave-second-mode))

(defun move-current-child-by-number ()
  "Move current child in a numbered frame"
  (move-child-to *current-child*
		 (find-frame-by-number
		  (query-number (format nil "Move '~A' to frame numbered:" (child-name *current-child*)))))
  (leave-second-mode))


;;; Copy by function
(defun copy-child-to (child frame-dest)
  (when (and child (frame-p frame-dest))
    (hide-all *current-root*)
    (pushnew child (frame-child frame-dest))
    (focus-all-children child frame-dest)
    (show-all-children *current-root*)))

(defun copy-current-child-by-name ()
  "Copy current child in a named frame"
  (copy-child-to *current-child*
		 (find-frame-by-name
		  (ask-frame-name (format nil "Copy '~A' to frame" (child-name *current-child*)))))
  (leave-second-mode))

(defun copy-current-child-by-number ()
  "Copy current child in a numbered frame"
  (copy-child-to *current-child*
		 (find-frame-by-number
		  (query-number (format nil "Copy '~A' to frame numbered:" (child-name *current-child*)))))
  (leave-second-mode))




;;; Show frame info
(defun show-all-frames-info ()
  "Show all frames info windows"
  (let ((*show-root-frame-p* t))
    (show-all-children)
    (with-all-frames (*current-root* frame)
      (raise-window (frame-window frame))
      (display-frame-info frame))))

(defun hide-all-frames-info ()
  "Hide all frames info windows"
  (with-all-windows (*current-root* window)
    (raise-window window))
  (hide-child *current-root*)
  (show-all-children))

(defun show-all-frames-info-key ()
  "Show all frames info windows until a key is release"
  (show-all-frames-info)
  (wait-no-key-or-button-press)
  (hide-all-frames-info))





;;; Mouse utilities
(defmacro present-windows-generic ((first-restore-frame) &body body)
  `(progn
     (with-all-frames (,first-restore-frame frame)
       (setf (frame-data-slot frame :old-layout) (frame-layout frame)
	     (frame-layout frame) #'tile-space-layout))
     (show-all-children *current-root*)
     (wait-no-key-or-button-press)
     (wait-a-key-or-button-press )
     (wait-no-key-or-button-press)
     (multiple-value-bind (x y) (xlib:query-pointer *root*)
       (let* ((child (find-child-under-mouse x y))
	      (parent (find-parent-frame child *root-frame*)))
	 (when (and child parent)
	   ,@body
	   (focus-all-children child parent))))
     (with-all-frames (,first-restore-frame frame)
       (setf (frame-layout frame) (frame-data-slot frame :old-layout)
	     (frame-data-slot frame :old-layout) nil))
     (show-all-children *current-root*)))

(defun have-to-present-windows (root-x root-y)
  (when (and (frame-p *current-root*)
	     (in-corner *present-windows-corner* root-x root-y))
    (stop-button-event)
    (present-windows-generic (*current-root*))
    t))

(defun have-to-present-all-windows (root-x root-y)
  (when (and (frame-p *current-root*)
	     (in-corner *present-all-windows-corner* root-x root-y))
    (stop-button-event)
    (switch-to-root-frame)
    (present-windows-generic (*root-frame*)
      (hide-all-children *root-frame*)
      (setf *current-root* parent))
    t))




(defun move-frame (frame parent orig-x orig-y)
  (when frame
    (hide-all-children frame)
    (with-slots (window) frame
      (move-window window orig-x orig-y #'display-frame-info (list frame))
      (setf (frame-x frame) (x-px->fl (xlib:drawable-x window) parent)
	    (frame-y frame) (y-px->fl (xlib:drawable-y window) parent)))
    (show-all-children frame)))


(defun resize-frame (frame parent orig-x orig-y)
  (when frame
    (hide-all-children frame)
    (with-slots (window) frame
      (resize-window window orig-x orig-y #'display-frame-info (list frame))
      (setf (frame-w frame) (w-px->fl (xlib:drawable-width window) parent)
	    (frame-h frame) (h-px->fl (xlib:drawable-height window) parent)))
    (show-all-children frame)))

	   

(defun mouse-click-to-focus-generic (window root-x root-y mouse-fn)
  "Focus the current frame or focus the current window parent
mouse-fun is #'move-frame or #'resize-frame"
  (let* ((to-replay t)
	 (child window)
	 (parent (find-parent-frame child *current-root*))
	 (root-p (or (equal window *root*)
		     (and (frame-p *current-root*)
			  (equal child (frame-window *current-root*))))))
    (when (or (not root-p) *create-frame-on-root*)
      (unless parent
	(if root-p
	    (progn
	      (setf child (create-frame)
		    parent *current-root*
		    mouse-fn #'resize-frame)
	      (place-frame child parent root-x root-y 10 10)
	      (xlib:map-window (frame-window child))
	      (pushnew child (frame-child *current-root*)))
	    (setf child (find-frame-window window *current-root*)
		  parent (find-parent-frame child *current-root*)))
	(when child
	  (funcall mouse-fn child parent root-x root-y)))
      (when (and child parent (focus-all-children child parent))
	(when (show-all-children)
	  (setf to-replay nil))))
    (if to-replay
	(replay-button-event)
	(stop-button-event))))

(defun mouse-click-to-focus-and-move (window root-x root-y)
  "Move and focus the current frame or focus the current window parent.
On *present-windows-corner*: Present windows in the current root.
On *present-all-windows-corner*: Present all windows in all frames."
  (or (have-to-present-windows root-x root-y)
      (have-to-present-all-windows root-x root-y)
      (mouse-click-to-focus-generic window root-x root-y #'move-frame)))

(defun mouse-click-to-focus-and-resize (window root-x root-y)
  "Resize and focus the current frame or focus the current window parent.
On *present-windows-corner*: Present windows in the current root.
On *present-all-windows-corner*: Present all windows in all frames."
  (or (have-to-present-windows root-x root-y)
      (have-to-present-all-windows root-x root-y)
      (mouse-click-to-focus-generic window root-x root-y #'resize-frame)))




(defun mouse-focus-move/resize-generic (root-x root-y mouse-fn window-parent)
  "Focus the current frame or focus the current window parent
mouse-fun is #'move-frame or #'resize-frame.
Focus child and its parents -
For window: set current child to window or its parent according to window-parent"
  (let* ((child (find-child-under-mouse root-x root-y))
	 (parent (find-parent-frame child)))
    (when (and (equal child *current-root*)
	       (frame-p *current-root*))
      (setf child (create-frame)
	    parent *current-root*
	    mouse-fn #'resize-frame)
      (place-frame child parent root-x root-y 10 10)
      (xlib:map-window (frame-window child))
      (pushnew child (frame-child *current-root*)))
    (typecase child
      (xlib:window
       (if (managed-window-p child parent)
	   (funcall mouse-fn parent (find-parent-frame parent) root-x root-y)
	   (funcall(cond ((eql mouse-fn #'move-frame) #'move-window)
			 ((eql mouse-fn #'resize-frame) #'resize-window))
		   child root-x root-y)))
      (frame (funcall mouse-fn child parent root-x root-y)))
    (focus-all-children child parent window-parent)
    (show-all-children)))




(defun test-mouse-binding (window root-x root-y)
  (dbg window root-x root-y)
  (replay-button-event))



(defun mouse-select-next-level (window root-x root-y)
  "Select the next level in frame"
  (declare (ignore root-x root-y))
  (let ((frame (find-frame-window window)))
    (when (or frame (xlib:window-equal window *root*))
      (select-next-level))
    (replay-button-event)))



(defun mouse-select-previous-level (window root-x root-y)
  "Select the previous level in frame"
  (declare (ignore root-x root-y))
  (let ((frame (find-frame-window window)))
    (when (or frame (xlib:window-equal window *root*))
      (select-previous-level))
    (replay-button-event)))



(defun mouse-enter-frame (window root-x root-y)
  "Enter in the selected frame - ie make it the root frame"
  (declare (ignore root-x root-y))
  (let ((frame (find-frame-window window)))
    (when (or frame (xlib:window-equal window *root*))
      (enter-frame))
    (replay-button-event)))



(defun mouse-leave-frame (window root-x root-y)
  "Leave the selected frame - ie make its parent the root frame"
  (declare (ignore root-x root-y))
  (let ((frame (find-frame-window window)))
    (when (or frame (xlib:window-equal window *root*))
      (leave-frame))
    (replay-button-event)))



;;;;;,-----
;;;;;| Various definitions
;;;;;`-----
;;(defun stop-all-pending-actions ()
;;  "Stop all pending actions (actions like open in new workspace/frame)"
;;  (setf *open-next-window-in-new-workspace* nil
;;	*open-next-window-in-new-frame* nil
;;	*arrow-action* nil
;;	*pager-arrow-action* nil))
;;

(defun show-help (&optional (browser "dillo") (tempfile "/tmp/clfswm.html"))
  "Show current keys and buttons bindings"
  (ignore-errors
    (produce-doc-html-in-file tempfile))
  (sleep 1)
  (do-shell (format nil "~A ~A" browser tempfile)))



;;;  Bind or jump functions
(let ((key-slots (make-array 10 :initial-element nil))
      (current-slot 1))
  (defun bind-on-slot ()
    "Bind current child to slot"
    (setf (aref key-slots current-slot) *current-child*))

  (defun remove-binding-on-slot ()
    "Remove binding on slot"
    (setf (aref key-slots current-slot) nil))

  (defun jump-to-slot ()
    "Jump to slot"
    (hide-all *current-root*)
    (setf *current-root* (aref key-slots current-slot)
	  *current-child* *current-root*)
    (focus-all-children *current-child* *current-child*)
    (show-all-children *current-root*))
  
  (defun bind-or-jump (n)
    "Bind or jump to a slot"
    (setf current-slot (- n 1))
    (let ((default-bind `("b" bind-on-slot
			      ,(format nil "Bind slot ~A on child: ~A" n (child-fullname *current-child*)))))
      (info-mode-menu (aif (aref key-slots current-slot)
			   `(,default-bind
				("BackSpace" remove-binding-on-slot
					     ,(format nil "Remove slot ~A binding on child: ~A" n (child-fullname *current-child*)))
				("   -  " nil " -")
			      ("Tab" jump-to-slot
				     ,(format nil "Jump to child: ~A" (aif (aref key-slots current-slot)
									   (child-fullname it)
									   "Not set - Please, bind it with 'b'")))
			      ("Return" jump-to-slot "Same thing")
			      ("space" jump-to-slot "Same thing"))
			   (list default-bind))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful function for the second mode ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-movement (&body body)
  `(when (frame-p *current-child*)
     ,@body
     (show-all-children)
     (display-all-frame-info)
     (draw-second-mode-window)
     (open-menu (find-menu 'frame-movement-menu))))


;;; Pack
(defun current-frame-pack-up ()
  "Pack the current frame up"
  (with-movement (pack-frame-up *current-child* (find-parent-frame *current-child* *current-root*))))

(defun current-frame-pack-down ()
  "Pack the current frame down"
  (with-movement (pack-frame-down *current-child* (find-parent-frame *current-child* *current-root*))))

(defun current-frame-pack-left ()
  "Pack the current frame left"
  (with-movement (pack-frame-left *current-child* (find-parent-frame *current-child* *current-root*))))

(defun current-frame-pack-right ()
  "Pack the current frame right"
  (with-movement (pack-frame-right *current-child* (find-parent-frame *current-child* *current-root*))))

;;; Center
(defun center-current-frame ()
  "Center the current frame"
  (with-movement (center-frame *current-child*)))

;;; Fill
(defun current-frame-fill-up ()
  "Fill the current frame up"
  (with-movement (fill-frame-up *current-child* (find-parent-frame *current-child* *current-root*))))

(defun current-frame-fill-down ()
  "Fill the current frame down"
  (with-movement (fill-frame-down *current-child* (find-parent-frame *current-child* *current-root*))))

(defun current-frame-fill-left ()
  "Fill the current frame left"
  (with-movement (fill-frame-left *current-child* (find-parent-frame *current-child* *current-root*))))

(defun current-frame-fill-right ()
  "Fill the current frame right"
  (with-movement (fill-frame-right *current-child* (find-parent-frame *current-child* *current-root*))))

(defun current-frame-fill-all-dir ()
  "Fill the current frame in all directions"
  (with-movement
    (let ((parent (find-parent-frame *current-child* *current-root*)))
      (fill-frame-up *current-child* parent)
      (fill-frame-down *current-child* parent)
      (fill-frame-left *current-child* parent)
      (fill-frame-right *current-child* parent))))

(defun current-frame-fill-vertical ()
  "Fill the current frame vertically"
  (with-movement
    (let ((parent (find-parent-frame *current-child* *current-root*)))
      (fill-frame-up *current-child* parent)
      (fill-frame-down *current-child* parent))))

(defun current-frame-fill-horizontal ()
  "Fill the current frame horizontally"
  (with-movement
    (let ((parent (find-parent-frame *current-child* *current-root*)))
      (fill-frame-left *current-child* parent)
      (fill-frame-right *current-child* parent))))
    

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



;;; Adapt frame functions
(defun adapt-current-frame-to-window-hints-generic (width-p height-p)
  "Adapt the current frame to the current window minimal size hints"
  (when (frame-p *current-child*)
    (let ((window (first (frame-child *current-child*))))
      (when (xlib:window-p window)
	(let* ((hints (xlib:wm-normal-hints window))
	       (min-width (and hints (xlib:wm-size-hints-min-width hints)))
	       (min-height (and hints (xlib:wm-size-hints-min-height hints))))
	  (when (and width-p min-width)
	    (setf (frame-rw *current-child*) min-width))
	  (when (and height-p min-height)
	    (setf (frame-rh *current-child*) min-height))
	  (fixe-real-size *current-child* (find-parent-frame *current-child*))
	  (leave-second-mode))))))

(defun adapt-current-frame-to-window-hints ()
  "Adapt the current frame to the current window minimal size hints"
  (adapt-current-frame-to-window-hints-generic t t))

(defun adapt-current-frame-to-window-width-hint ()
  "Adapt the current frame to the current window minimal width hint"
  (adapt-current-frame-to-window-hints-generic t nil))

(defun adapt-current-frame-to-window-height-hint ()
  "Adapt the current frame to the current window minimal height hint"
  (adapt-current-frame-to-window-hints-generic nil t))




;;; Managed window type functions
(defun current-frame-manage-window-type-generic (type-list)
  (when (frame-p *current-child*)
    (setf (frame-managed-type *current-child*) type-list
	  (frame-forced-managed-window *current-child*) nil
	  (frame-forced-unmanaged-window *current-child*) nil))
  (leave-second-mode))


(defun current-frame-manage-window-type ()
  "Change window types to be managed by a frame"
  (when (frame-p *current-child*)
    (let* ((type-str (query-string "Managed window type: (all, normal, transient, maxsize, desktop, dock, toolbar, menu, utility, splash, dialog)"
				   (format nil "~{~:(~A~) ~}" (frame-managed-type *current-child*))))
	   (type-list (loop :for type :in (split-string type-str)
			 :collect (intern (string-upcase type) :keyword))))
      (current-frame-manage-window-type-generic type-list))))


(defun current-frame-manage-all-window-type ()
  "Manage all window type"
  (current-frame-manage-window-type-generic '(:all)))

(defun current-frame-manage-only-normal-window-type ()
  "Manage only normal window type"
  (current-frame-manage-window-type-generic '(:normal)))

(defun current-frame-manage-no-window-type ()
  "Do not manage any window type"
  (current-frame-manage-window-type-generic nil))







;;; Current window utilities
(defun get-current-window ()
  (typecase *current-child*
    (xlib:window  *current-child*)
    (frame (frame-selected-child *current-child*))))

(defmacro with-current-window (&body body)
  "Bind 'window' to the current window"
  `(let ((window (get-current-window)))
      (when (xlib:window-p window)
	,@body)))





;;; Force window functions
(defun force-window-in-frame ()
  "Force the current window to move in the frame (Useful only for unmanaged windows)"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (with-xlib-protect
	(setf (xlib:drawable-x window) (frame-rx parent)
	      (xlib:drawable-y window) (frame-ry parent)))))
  (leave-second-mode))


(defun force-window-center-in-frame ()
  "Force the current window to move in the center of the frame (Useful only for unmanaged windows)"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (with-xlib-protect
	(setf (xlib:drawable-x window) (truncate (+ (frame-rx parent)
						    (/ (- (frame-rw parent)
							  (xlib:drawable-width window)) 2)))
	      (xlib:drawable-y window) (truncate (+ (frame-ry parent)
						    (/ (- (frame-rh parent)
							  (xlib:drawable-height window)) 2)))))))
  (leave-second-mode))



(defun display-current-window-info ()
  "Display information on the current window"
  (with-current-window
    (info-mode (list (format nil "Window:       ~A" window)
		     (format nil "Window name:  ~A" (xlib:wm-name window))
		     (format nil "Window class: ~A" (xlib:get-wm-class window))
		     (format nil "Window type:  ~:(~A~)" (window-type window)))))
  (leave-second-mode))


(defun manage-current-window ()
  "Force to manage the current window by its parent frame"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (with-slots ((managed forced-managed-window)
		   (unmanaged forced-unmanaged-window)) parent
	(setf unmanaged (remove window unmanaged)
	      unmanaged (remove (xlib:wm-name window) unmanaged :test #'string-equal-p))
	(pushnew window managed))))
  (leave-second-mode))

(defun unmanage-current-window ()
  "Force to not manage the current window by its parent frame"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (with-slots ((managed forced-managed-window)
		   (unmanaged forced-unmanaged-window)) parent
	(setf managed (remove window managed)
	      managed (remove (xlib:wm-name window) managed :test #'string-equal-p))
	(pushnew window unmanaged))))
  (leave-second-mode))



;;; Moving window with the mouse function
(defun mouse-move-window-over-frame (window root-x root-y)
  "Move the window under the mouse cursor to another frame"
  (declare (ignore window))
  (let ((child (find-child-under-mouse root-x root-y)))
    (unless (equal child *current-root*)
      (hide-child child)
      (remove-child-in-frame child (find-parent-frame child))
      (wait-mouse-button-release 50 51)
      (multiple-value-bind (x y)
	  (xlib:query-pointer *root*)
	(let ((dest (find-child-under-mouse x y)))
	  (when (xlib:window-p dest)
	    (setf dest (find-parent-frame dest)))
	  (unless (equal child dest)
	    (move-child-to child dest))))))
  (stop-button-event))




;;; Hide/Show frame window functions
(defun hide/show-frame-window (frame value)
  "Hide/show the frame window"
  (when (frame-p frame)
    (setf (frame-show-window-p *current-child*) value)
    (show-all-children *current-root*))
  (leave-second-mode))


(defun hide-current-frame-window ()
  "Hide the current frame window"
  (hide/show-frame-window *current-child* nil))

(defun show-current-frame-window ()
  "Show the current frame window"
  (hide/show-frame-window *current-child* t))



;;; Hide/Unhide current child
(defun hide-current-child ()
  "Hide the current child"
  (let ((parent (find-parent-frame *current-child*)))
    (when (frame-p parent)
      (with-slots (child hidden-children) parent
	(hide-all *current-child*)
	(setf child (remove *current-child* child))
	(pushnew *current-child* hidden-children)
	(setf *current-child* parent))
      (show-all-children)))
  (leave-second-mode))


(defun frame-unhide-child (hidden frame-src frame-dest)
  "Unhide a hidden child from frame-src in frame-dest"
  (with-slots (hidden-children) frame-src
    (setf hidden-children (remove hidden hidden-children)))
  (with-slots (child) frame-dest
    (pushnew hidden child)))
  


(defun unhide-a-child ()
  "Unhide a child in the current frame"
  (when (frame-p *current-child*)
    (with-slots (child hidden-children) *current-child*
      (info-mode-menu (loop :for i :from 0
			 :for hidden :in hidden-children
			 :collect (list (code-char (+ (char-code #\a) i))
					(let ((lhd hidden))
					  (lambda ()
					    (frame-unhide-child lhd *current-child* *current-child*)))
					(format nil "Unhide ~A" (child-fullname hidden))))))
    (show-all-children))
  (leave-second-mode))


(defun unhide-all-children ()
  "Unhide all current frame hidden children"
  (when (frame-p *current-child*)
    (with-slots (child hidden-children) *current-child*
      (dolist (c hidden-children)
	(pushnew c child))
      (setf hidden-children nil))
    (show-all-children))
  (leave-second-mode))


(defun unhide-a-child-from-all-frames ()
  "Unhide a child from all frames in the current frame"
  (when (frame-p *current-child*)
    (let ((acc nil)
	  (keynum -1))
      (with-all-frames (*root-frame* frame)
	(when (frame-hidden-children frame)
	  (push (format nil "~A" (child-fullname frame)) acc)
	  (dolist (hidden (frame-hidden-children frame))
	    (push (list (code-char (+ (char-code #\a) (incf keynum)))
			(let ((lhd hidden))
			  (lambda ()
			    (frame-unhide-child lhd frame *current-child*)))
			(format nil "Unhide ~A" (child-fullname hidden)))
		  acc))))
      (info-mode-menu (nreverse acc)))
    (show-all-children))
  (leave-second-mode))




    
