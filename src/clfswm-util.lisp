;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Utility
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


;;; Configuration file
(defun xdg-config-home ()
  (aif (getenv "XDG_CONFIG_HOME")
       (pathname-directory (concatenate 'string it "/"))
       (append (pathname-directory (user-homedir-pathname)) '(".config"))))


(let ((saved-conf-name nil))
  (defun conf-file-name (&optional alternate-name)
    (unless (and saved-conf-name (not alternate-name))
      (let* ((user-conf (probe-file (merge-pathnames (user-homedir-pathname) #p".clfswmrc")))
	     (etc-conf (probe-file #p"/etc/clfswmrc"))
	     (config-user-conf (probe-file (make-pathname :directory (append (xdg-config-home) '("clfswm"))
							  :name "clfswmrc")))
	     (alternate-conf (and alternate-name (probe-file alternate-name))))
	(setf saved-conf-name (or alternate-conf config-user-conf user-conf etc-conf))))
    (print saved-conf-name)
    saved-conf-name))




(defun load-contrib (file)
  "Load a file in the contrib directory"
  (let ((truename (merge-pathnames file *contrib-dir*)))
    (format t "Loading contribution file: ~A~%" truename)
    (when (probe-file truename)
      (load truename :verbose nil))))


(defun reload-clfswm ()
  "Reload clfswm"
  (format t "~&-*- Reloading CLFSWM -*-~%")
  (asdf:oos 'asdf:load-op :clfswm)
  (reset-clfswm))



(defun query-yes-or-no (formatter &rest args)
  (let ((rep (query-string (apply #'format nil formatter args) "" '("yes" "no"))))
    (or (string= rep "")
	(char= (char rep 0) #\y)
	(char= (char rep 0) #\Y))))




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
	  (w (/ (query-number "Frame width in percent (%)" 100) 100))
	  (h (/ (query-number "Frame height in percent (%)" 100) 100)))
      (push (create-frame :name name :x x :y y :w w :h h)
	    (frame-child *current-child*))))
  (leave-second-mode))



(defun delete-focus-window-generic (close-fun)
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (when (child-equal-p window *current-child*)
	(setf *current-child* *current-root*))
      (hide-child window)
      (delete-child-and-children-in-all-frames window close-fun)
      (show-all-children))))

(defun delete-focus-window ()
  "Close focus window: Delete the focus window in all frames and workspaces"
  (delete-focus-window-generic 'delete-window))

(defun destroy-focus-window ()
  "Kill focus window: Destroy the focus window in all frames and workspaces"
  (delete-focus-window-generic 'destroy-window))

(defun remove-focus-window ()
  "Remove the focus window from the current frame"
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (setf *current-child* *current-root*)
      (hide-child window)
      (remove-child-in-frame window (find-parent-frame window))
      (show-all-children))))


(defun unhide-all-windows-in-current-child ()
  "Unhide all hidden windows into the current child"
  (dolist (window (get-hidden-windows))
    (unhide-window window)
    (process-new-window window)
    (map-window window))
  (show-all-children))




(defun find-window-under-mouse (x y)
  "Return the child window under the mouse"
  (let ((win *root*))
    (with-all-windows-frames-and-parent (*current-root* child parent)
      (when (and (or (managed-window-p child parent) (child-equal-p parent *current-child*))
		 (in-window child x y))
	(setf win child))
      (when (in-frame child x y)
	(setf win (frame-window child))))
    win))




(defun find-child-under-mouse-in-never-managed-windows (x y)
  "Return the child under mouse from never managed windows"
  (let ((ret nil))
    (dolist (win (xlib:query-tree *root*))
      (unless (window-hidden-p win)
	(multiple-value-bind (never-managed raise)
	    (never-managed-window-p win)
	  (when (and never-managed raise (in-window win x y))
	    (setf ret win)))))
    ret))


(defun find-child-under-mouse-in-child-tree (x y &optional first-foundp)
  "Return the child under the mouse"
  (let ((ret nil))
    (with-all-windows-frames-and-parent (*current-root* child parent)
      (when (and (not (window-hidden-p child))
		 (or (managed-window-p child parent) (child-equal-p parent *current-child*))
		 (in-window child x y))
	(if first-foundp
	    (return-from find-child-under-mouse-in-child-tree child)
	    (setf ret child)))
      (when (in-frame child x y)
	(if first-foundp
	    (return-from find-child-under-mouse-in-child-tree child)
	    (setf ret child))))
    ret))


(defun find-child-under-mouse (x y &optional first-foundp also-never-managed)
  "Return the child under the mouse"
  (or (and also-never-managed
	   (find-child-under-mouse-in-never-managed-windows x y))
      (find-child-under-mouse-in-child-tree x y first-foundp)))





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
  (hide-all *current-child*)
  (copy-current-child)
  (remove-child-in-frame *current-child* (find-parent-frame *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (show-all-children t))

(defun remove-current-child ()
  "Remove the current child from its parent frame"
  (hide-all *current-child*)
  (remove-child-in-frame *current-child* (find-parent-frame *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (show-all-children t)
  (leave-second-mode))

(defun delete-current-child ()
  "Delete the current child and its children in all frames"
  (hide-all *current-child*)
  (delete-child-and-children-in-all-frames *current-child*)
  (show-all-children t)
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




;;; Maximize function
(defun frame-toggle-maximize ()
  "Maximize/Unmaximize the current frame in its parent frame"
  (when (frame-p *current-child*)
    (let ((unmaximized-coords (frame-data-slot *current-child* :unmaximized-coords)))
      (if unmaximized-coords
	  (with-slots (x y w h) *current-child*
	    (destructuring-bind (nx ny nw nh) unmaximized-coords
	      (setf (frame-data-slot *current-child* :unmaximized-coords) nil
		    x nx y ny w nw h nh)))
	  (with-slots (x y w h) *current-child*
	    (setf (frame-data-slot *current-child* :unmaximized-coords)
		  (list x y w h)
		  x 0 y 0 w 1 h 1))))
    (show-all-children)
    (leave-second-mode)))









;;; CONFIG - Identify mode
(defun identify-key ()
  "Identify a key"
  (let* ((done nil)
	 (font (xlib:open-font *display* *identify-font-string*))
	 (window (xlib:create-window :parent *root*
				     :x 0 :y 0
				     :width (- (xlib:screen-width *screen*) (* *border-size* 2))
				     :height (* 5 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font)))
				     :background (get-color *identify-background*)
				     :border-width *border-size*
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
		      (keysym (keysym->keysym-name (keycode->keysym code modifiers))))
		 (setf done (and (equal key #\q) (equal modifiers *default-modifiers*)))
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
      (map-window window)
      (format t "~&Press 'q' to stop the identify loop~%")
      (print-key nil nil nil nil nil)
      (force-output)
      (unwind-protect
	   (loop until done do
		(when (xlib:event-listen *display* *loop-timeout*)
		  (xlib:process-event *display* :handler #'handle-identify))
		(xlib:display-finish-output *display*))
	(xlib:destroy-window window)
	(xlib:close-font font)
	(xgrab-pointer *root* 66 67)))))






(defun eval-from-query-string ()
  "Eval a lisp form from the query input"
  (let ((form (query-string (format nil "Eval Lisp - ~A" (package-name *package*))))
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
  (multiple-value-bind (program return)
      (query-string "Run:")
    (when (and (equal return :return) program (not (equal program "")))
      (setf *second-mode-leave-function* (let ((cmd (concatenate 'string "cd $HOME && " program)))
					   (lambda ()
					     (do-shell cmd))))
      (leave-second-mode))))




;;; Frame name actions
(defun ask-frame-name (msg)
  "Ask a frame name"
  (let ((all-frame-name nil))
    (with-all-frames (*root-frame* frame)
      (awhen (frame-name frame) (push it all-frame-name)))
    (query-string msg "" all-frame-name)))


;;; Focus by functions
(defun focus-frame-by (frame)
  (when (frame-p frame)
    (focus-all-children frame (or (find-parent-frame frame *current-root*)
				  (find-parent-frame frame)
				  *root-frame*))
    (show-all-children t)))


(defun focus-frame-by-name ()
  "Focus a frame by name"
  (focus-frame-by (find-frame-by-name (ask-frame-name "Focus frame:")))
  (leave-second-mode))

(defun focus-frame-by-number ()
  "Focus a frame by number"
  (focus-frame-by (find-frame-by-number (query-number "Focus frame by number:")))
  (leave-second-mode))


;;; Open by functions
(defun open-frame-by (frame)
  (when (frame-p frame)
    (push (create-frame :name (query-string "Frame name")) (frame-child frame))
    (show-all-children)))



(defun open-frame-by-name ()
  "Open a new frame in a named frame"
  (open-frame-by (find-frame-by-name (ask-frame-name "Open a new frame in: ")))
  (leave-second-mode))

(defun open-frame-by-number ()
  "Open a new frame in a numbered frame"
  (open-frame-by (find-frame-by-number (query-number "Open a new frame in the group numbered:")))
  (leave-second-mode))


;;; Delete by functions
(defun delete-frame-by (frame)
  (unless (child-equal-p frame *root-frame*)
    (when (child-equal-p frame *current-root*)
      (setf *current-root* *root-frame*))
    (when (child-equal-p frame *current-child*)
      (setf *current-child* *current-root*))
    (remove-child-in-frame frame (find-parent-frame frame)))
  (show-all-children t))


(defun delete-frame-by-name ()
  "Delete a frame by name"
  (delete-frame-by (find-frame-by-name (ask-frame-name "Delete frame: ")))
  (leave-second-mode))

(defun delete-frame-by-number ()
  "Delete a frame by number"
  (delete-frame-by (find-frame-by-number (query-number "Delete frame by number:")))
  (leave-second-mode))


;;; Move by function
(defun move-child-to (child frame-dest)
  (when (and child (frame-p frame-dest))
    (remove-child-in-frame child (find-parent-frame child))
    (pushnew child (frame-child frame-dest))
    (focus-all-children child frame-dest)
    (show-all-children t)))

(defun move-current-child-by-name ()
  "Move current child in a named frame"
  (move-child-to *current-child*
		 (find-frame-by-name
		  (ask-frame-name (format nil "Move '~A' to frame: " (child-name *current-child*)))))
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
    (pushnew child (frame-child frame-dest))
    (focus-all-children child frame-dest)
    (show-all-children t)))

(defun copy-current-child-by-name ()
  "Copy current child in a named frame"
  (copy-child-to *current-child*
		 (find-frame-by-name
		  (ask-frame-name (format nil "Copy '~A' to frame: " (child-name *current-child*)))))
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


(defun move-frame (frame parent orig-x orig-y)
  (when (and frame parent (not (child-equal-p frame *current-root*)))
    (hide-all-children frame)
    (with-slots (window) frame
      (move-window window orig-x orig-y #'display-frame-info (list frame))
      (setf (frame-x frame) (x-px->fl (xlib:drawable-x window) parent)
	    (frame-y frame) (y-px->fl (xlib:drawable-y window) parent)))
    (show-all-children)))

(defun resize-frame (frame parent orig-x orig-y)
  (when (and frame parent (not (child-equal-p frame *current-root*)))
    (hide-all-children frame)
    (with-slots (window) frame
      (resize-window window orig-x orig-y #'display-frame-info (list frame))
      (setf (frame-w frame) (w-px->fl (xlib:drawable-width window) parent)
	    (frame-h frame) (h-px->fl (xlib:drawable-height window) parent)))
    (show-all-children)))



(defun mouse-click-to-focus-generic (window root-x root-y mouse-fn)
  "Focus the current frame or focus the current window parent
mouse-fun is #'move-frame or #'resize-frame"
  (let* ((to-replay t)
	 (child (find-child-under-mouse root-x root-y))
	 (parent (find-parent-frame child))
	 (root-p (or (child-equal-p window *root*)
		     (and (frame-p *current-root*)
			  (child-equal-p child (frame-window *current-root*))))))
    (labels ((add-new-frame ()
	       (setf child (create-frame)
		     parent *current-root*
		     mouse-fn #'resize-frame)
	       (place-frame child parent root-x root-y 10 10)
	       (map-window (frame-window child))
	       (pushnew child (frame-child *current-root*))))
      (when (or (not root-p) *create-frame-on-root*)
	(unless parent
	  (if root-p
	      (add-new-frame)
	      (progn
		(unless (equal (type-of child) 'frame)
		  (setf child (find-frame-window child *current-root*)))
		(setf parent (find-parent-frame child)))))
	(when (and (frame-p child) (not (child-equal-p child *current-root*)))
	  (funcall mouse-fn child parent root-x root-y))
	(when (and child parent
		   (focus-all-children child parent
				       (not (and (child-equal-p *current-child* *current-root*)
						 (xlib:window-p *current-root*)))))
	  (when (show-all-children)
	    (setf to-replay nil))))
      (if to-replay
	  (replay-button-event)
	  (stop-button-event)))))


(defun mouse-click-to-focus-and-move (window root-x root-y)
  "Move and focus the current frame or focus the current window parent.
Or do actions on corners"
  (or (do-corner-action root-x root-y *corner-main-mode-left-button*)
      (mouse-click-to-focus-generic window root-x root-y #'move-frame)))

(defun mouse-click-to-focus-and-resize (window root-x root-y)
  "Resize and focus the current frame or focus the current window parent.
Or do actions on corners"
  (or (do-corner-action root-x root-y *corner-main-mode-right-button*)
      (mouse-click-to-focus-generic window root-x root-y #'resize-frame)))

(defun mouse-middle-click (window root-x root-y)
  "Do actions on corners"
  (declare (ignore window))
  (or (do-corner-action root-x root-y *corner-main-mode-middle-button*)
      (replay-button-event)))




(defun mouse-focus-move/resize-generic (root-x root-y mouse-fn window-parent)
  "Focus the current frame or focus the current window parent
mouse-fun is #'move-frame or #'resize-frame.
Focus child and its parents -
For window: set current child to window or its parent according to window-parent"
  (labels ((move/resize-managed (child)
	     (let ((parent (find-parent-frame child)))
	       (when (and (child-equal-p child *current-root*)
			  (frame-p *current-root*))
		 (setf child (create-frame)
		       parent *current-root*
		       mouse-fn #'resize-frame)
		 (place-frame child parent root-x root-y 10 10)
		 (map-window (frame-window child))
		 (pushnew child (frame-child *current-root*)))
	       (focus-all-children child parent window-parent)
	       (show-all-children)
	       (typecase child
		 (xlib:window
		  (if (managed-window-p child parent)
		      (funcall mouse-fn parent (find-parent-frame parent) root-x root-y)
		      (funcall (cond ((or (eql mouse-fn #'move-frame)
                                          (eql mouse-fn #'move-frame-constrained))
                                      #'move-window)
				     ((or (eql mouse-fn #'resize-frame)
                                          (eql mouse-fn #'resize-frame-constrained))
                                      #'resize-window))
			       child root-x root-y)))
		 (frame (funcall mouse-fn child parent root-x root-y)))
	       (show-all-children)))
	   (move/resize-never-managed (child raise-fun)
	     (funcall raise-fun child)
	     (funcall (cond ((eql mouse-fn #'move-frame) #'move-window)
			    ((eql mouse-fn #'resize-frame) #'resize-window))
		      child root-x root-y)))
    (let ((child (find-child-under-mouse root-x root-y nil t)))
      (multiple-value-bind (never-managed raise-fun)
	  (never-managed-window-p child)
	(if (and (xlib:window-p child) never-managed raise-fun)
	    (move/resize-never-managed child raise-fun)
	    (move/resize-managed child))))))





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

(defun show-help (&optional (browser "dillo") (tempfile "/tmp/clfswm.html"))
  "Show current keys and buttons bindings"
  (ignore-errors
    (produce-doc-html-in-file tempfile))
  (sleep 1)
  (do-shell (format nil "~A ~A" browser tempfile)))



;;;  Bind or jump functions
(let ((key-slots (make-array 10 :initial-element nil))
      (current-slot 1))
  (defun bind-on-slot (&optional (slot current-slot))
    "Bind current child to slot"
    (setf (aref key-slots slot) *current-child*))

  (defun remove-binding-on-slot ()
    "Remove binding on slot"
    (setf (aref key-slots current-slot) nil))

  (defun jump-to-slot ()
    "Jump to slot"
    (let ((jump-child (aref key-slots current-slot)))
      (when (find-child jump-child *root-frame*)
	(setf *current-root* jump-child
	      *current-child* *current-root*)
	(focus-all-children *current-child* *current-child*)
	(show-all-children t))))

  (defun bind-or-jump (n)
    "Bind or jump to a slot (a frame or a window)"
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


;;; Children navigation
(defun with-movement-select-next-brother ()
  "Select the next brother frame"
  (with-movement (select-next-brother-simple)))

(defun with-movement-select-previous-brother ()
  "Select the previous brother frame"
  (with-movement (select-previous-brother-simple)))

(defun with-movement-select-next-level ()
  "Select the next level"
  (with-movement (select-next-level)))

(defun with-movement-select-previous-level ()
  "Select the previous levelframe"
  (with-movement (select-previous-level)))

(defun with-movement-select-next-child ()
  "Select the next child"
  (with-movement (select-next-child-simple)))



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








;;; Force window functions
(defun force-window-in-frame ()
  "Force the current window to move in the frame (Useful only for unmanaged windows)"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (setf (xlib:drawable-x window) (frame-rx parent)
	    (xlib:drawable-y window) (frame-ry parent))
      (xlib:display-finish-output *display*)))
  (leave-second-mode))


(defun force-window-center-in-frame ()
  "Force the current window to move in the center of the frame (Useful only for unmanaged windows)"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (setf (xlib:drawable-x window) (truncate (+ (frame-rx parent)
						  (/ (- (frame-rw parent)
							(xlib:drawable-width window)) 2)))
	    (xlib:drawable-y window) (truncate (+ (frame-ry parent)
						  (/ (- (frame-rh parent)
							(xlib:drawable-height window)) 2))))
      (xlib:display-finish-output *display*)))
  (leave-second-mode))



(defun display-current-window-info ()
  "Display information on the current window"
  (with-current-window
    (info-mode (list (format nil "Window:       ~A" window)
		     (format nil "Window name:  ~A" (xlib:wm-name window))
		     (format nil "Window class: ~A" (xlib:get-wm-class window))
		     (format nil "Window type:  ~:(~A~)" (window-type window))
		     (format nil "Window id:    0x~X" (xlib:window-id window)))))
  (leave-second-mode))


(defun manage-current-window ()
  "Force to manage the current window by its parent frame"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (with-slots ((managed forced-managed-window)
		   (unmanaged forced-unmanaged-window)) parent
	(setf unmanaged (child-remove window unmanaged)
	      unmanaged (remove (xlib:wm-name window) unmanaged :test #'string-equal-p))
	(pushnew window managed))))
  (leave-second-mode))

(defun unmanage-current-window ()
  "Force to not manage the current window by its parent frame"
  (with-current-window
    (let ((parent (find-parent-frame window)))
      (with-slots ((managed forced-managed-window)
		   (unmanaged forced-unmanaged-window)) parent
	(setf managed (child-remove window managed)
	      managed (remove (xlib:wm-name window) managed :test #'string-equal-p))
	(pushnew window unmanaged))))
  (leave-second-mode))



;;; Moving child with the mouse button
(defun mouse-move-child-over-frame (window root-x root-y)
  "Move the child under the mouse cursor to another frame"
  (declare (ignore window))
  (let ((child (find-child-under-mouse root-x root-y)))
    (unless (child-equal-p child *current-root*)
      (hide-all child)
      (remove-child-in-frame child (find-parent-frame child))
      (wait-mouse-button-release 50 51)
      (multiple-value-bind (x y)
	  (xlib:query-pointer *root*)
	(let ((dest (find-child-under-mouse x y)))
	  (when (xlib:window-p dest)
	    (setf dest (find-parent-frame dest)))
	  (unless (child-equal-p child dest)
	    (move-child-to child dest)
	    (show-all-children))))))
  (stop-button-event))




;;; Hide/Show frame window functions
(defun hide/show-frame-window (frame value)
  "Hide/show the frame window"
  (when (frame-p frame)
    (setf (frame-show-window-p *current-child*) value)
    (show-all-children))
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
  (unless (child-equal-p *current-child* *current-root*)
    (let ((parent (find-parent-frame *current-child*)))
      (when (frame-p parent)
	(with-slots (child hidden-children) parent
	  (hide-all *current-child*)
	  (setf child (child-remove *current-child* child))
	  (pushnew *current-child* hidden-children)
	  (setf *current-child* parent))
	(show-all-children)))
    (leave-second-mode)))


(defun frame-unhide-child (hidden frame-src frame-dest)
  "Unhide a hidden child from frame-src in frame-dest"
  (with-slots (hidden-children) frame-src
    (setf hidden-children (child-remove hidden hidden-children)))
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





(let ((last-child nil))
  (defun init-last-child ()
    (setf last-child nil))
  (defun switch-to-last-child ()
    "Store the current child and switch to the previous one"
    (let ((current-child *current-child*))
      (when last-child
	(setf *current-root* last-child
	      *current-child* *current-root*)
	(focus-all-children *current-child* *current-child*)
	(show-all-children t))
      (setf last-child current-child))
    (leave-second-mode)))







;;; Focus policy functions
(defun set-focus-policy-generic (focus-policy)
  (when (frame-p *current-child*)
    (setf (frame-focus-policy *current-child*) focus-policy))
  (leave-second-mode))


(defun current-frame-set-click-focus-policy ()
  "Set a click focus policy for the current frame."
  (set-focus-policy-generic :click))

(defun current-frame-set-sloppy-focus-policy ()
  "Set a sloppy focus policy for the current frame."
  (set-focus-policy-generic :sloppy))

(defun current-frame-set-sloppy-strict-focus-policy ()
  "Set a (strict) sloppy focus policy only for windows in the current frame."
  (set-focus-policy-generic :sloppy-strict))

(defun current-frame-set-sloppy-select-policy ()
  "Set a sloppy select policy for the current frame."
    (set-focus-policy-generic :sloppy-select))



(defun set-focus-policy-generic-for-all (focus-policy)
  (with-all-frames (*root-frame* frame)
    (setf (frame-focus-policy frame) focus-policy))
  (leave-second-mode))


(defun all-frames-set-click-focus-policy ()
  "Set a click focus policy for all frames."
  (set-focus-policy-generic-for-all :click))

(defun all-frames-set-sloppy-focus-policy ()
  "Set a sloppy focus policy for all frames."
  (set-focus-policy-generic-for-all :sloppy))

(defun all-frames-set-sloppy-strict-focus-policy ()
  "Set a (strict) sloppy focus policy for all frames."
  (set-focus-policy-generic-for-all :sloppy-strict))

(defun all-frames-set-sloppy-select-policy ()
  "Set a sloppy select policy for all frames."
    (set-focus-policy-generic-for-all :sloppy-select))



;;; Ensure unique name/number functions
(defun extract-number-from-name (name)
  (when (stringp name)
    (let* ((pos (1+ (or (position #\. name :from-end t) -1)))
	   (number (parse-integer name :junk-allowed t :start pos)))
      (values number
	      (if number (subseq name 0 (1- pos)) name)))))




(defun ensure-unique-name ()
  "Ensure that all children names are unique"
  (with-all-children (*root-frame* child)
    (multiple-value-bind (num1 name1)
	(extract-number-from-name (child-name child))
      (declare (ignore num1))
      (when name1
	(let ((acc nil))
	  (with-all-children (*root-frame* c)
	    (unless (child-equal-p child c))
	    (multiple-value-bind (num2 name2)
		(extract-number-from-name (child-name c))
	      (when (string-equal name1 name2)
		(push num2 acc))))
	  (dbg acc)
	  (when (> (length acc) 1)
	    (setf (child-name child)
		  (format nil "~A.~A" name1
			  (1+ (find-free-number (loop for i in acc when i collect (1- i)))))))))))
  (leave-second-mode))

(defun ensure-unique-number ()
  "Ensure that all children numbers are unique"
  (let ((num -1))
    (with-all-frames (*root-frame* frame)
      (setf (frame-number frame) (incf num))))
  (leave-second-mode))



;;; Standard menu functions - Based on the XDG specifications
(defconfig *xdg-section-list* (append '(TextEditor FileManager WebBrowser)
                                      '(AudioVideo Audio Video Development Education Game Graphics Network Office Settings System Utility)
                                      '(TerminalEmulator Archlinux Screensaver))
  'Menu "Standard menu sections")


(defun um-create-xdg-section-list (menu)
  (dolist (section *xdg-section-list*)
    (add-sub-menu menu :next section (format nil "~A" section) menu)))

(defun um-find-submenu (menu section-list)
  (let ((acc nil))
    (dolist (section section-list)
      (awhen (find-toplevel-menu (intern (string-upcase section) :clfswm) menu)
	(push it acc)))
    (if acc
	acc
	(list (find-toplevel-menu 'Utility menu)))))


(defun um-extract-value (line)
  (second (split-string line #\=)))


(defun um-add-desktop (desktop menu)
  (let (name exec categories comment)
    (when (probe-file desktop)
      (with-open-file (stream desktop :direction :input)
	(loop for line = (read-line stream nil nil)
	   while line
	   do
	   (cond ((first-position "Name=" line) (setf name (um-extract-value line)))
		 ((first-position "Exec=" line) (setf exec (um-extract-value line)))
		 ((first-position "Categories=" line) (setf categories (um-extract-value line)))
		 ((first-position "Comment=" line) (setf comment (um-extract-value line))))
	   (when (and name exec categories)
	     (let* ((sub-menu (um-find-submenu menu (split-string categories #\;)))
		    (fun-name (intern name :clfswm)))
	       (setf (symbol-function fun-name) (let ((do-exec exec))
						  (lambda ()
						    (do-shell do-exec)
						    (leave-second-mode)))
		     (documentation fun-name 'function) (format nil "~A~A" name (if comment
										    (format nil " - ~A" comment)
										    "")))
	       (dolist (m sub-menu)
		 (add-menu-key (menu-name m) :next fun-name m)))
	     (setf name nil exec nil categories nil comment nil)))))))


(defun update-menus (&optional (menu (make-menu :name 'main :doc "Main menu")))
  (um-create-xdg-section-list menu)
  (let ((count 0)
	(found (make-hash-table :test #'equal)))
    (dolist (dir (remove-duplicates
		  (split-string (getenv "XDG_DATA_DIRS") #\:) :test #'string-equal))
      (dolist (desktop (directory (concatenate 'string dir "/applications/**/*.desktop")))
	(unless (gethash (file-namestring desktop) found)
	  (setf (gethash (file-namestring desktop) found) t)
	  (um-add-desktop desktop menu)
	  (incf count))))
    menu))



;;; Close/Kill focused window

(defun ask-close/kill-current-window ()
  "Close or kill the current window (ask before doing anything)"
  (let ((window (xlib:input-focus *display*)))
    (info-mode-menu
     (if (and window (not (xlib:window-equal window *no-focus-window*)))
	 `(,(format nil "Focus window: ~A" (xlib:wm-name window))
	    (#\c delete-focus-window "Close the focus window")
	    (#\k destroy-focus-window "Kill the focus window")
	    (#\r remove-focus-window)
	    (#\u unhide-all-windows-in-current-child))
	 `(,(format nil "Focus window: None")
	    (#\u unhide-all-windows-in-current-child))))))



;;; Other window manager functions
(defun get-proc-list ()
  (let ((proc (do-shell "ps x -o pid=" nil t))
	(proc-list nil))
    (loop for line = (read-line proc nil nil)
       while line
       do (push line proc-list))
    (dbg proc-list)
    proc-list))

(defun run-other-window-manager ()
  (let ((proc-start (get-proc-list)))
    (do-shell *other-window-manager* nil t :terminal)
    (let* ((proc-end (get-proc-list))
	   (proc-diff (set-difference proc-end proc-start :test #'equal)))
      (dbg 'killing-sigterm proc-diff)
      (do-shell (format nil "kill ~{ ~A ~}  2> /dev/null" proc-diff) nil t :terminal)
      (dbg 'killing-sigkill proc-diff)
      (do-shell (format nil "kill -9 ~{ ~A ~} 2> /dev/null" proc-diff) nil t :terminal)
      (sleep 1))
    (setf *other-window-manager* nil)))


(defun do-run-other-window-manager (window-manager)
  (setf *other-window-manager* window-manager)
  (throw 'exit-main-loop nil))

(defmacro def-run-other-window-manager (name &optional definition)
  (let ((definition (or definition name)))
    `(defun ,(create-symbol "run-" name) ()
       ,(format nil "Run ~A" definition)
       (do-run-other-window-manager ,(format nil "~A" name)))))

(def-run-other-window-manager "xterm")
(def-run-other-window-manager "icewm")
(def-run-other-window-manager "twm")
(def-run-other-window-manager "gnome-session" "Gnome")
(def-run-other-window-manager "startkde" "KDE")
(def-run-other-window-manager "xfce4-session" "XFCE")

(defun run-lxde ()
  "Run LXDE"
  (do-run-other-window-manager "( lxsession & ); xterm -e \"echo '  /----------------------------------\\' ; echo '  |  CLFSWM Note:                    |' ; echo '  |    Close this window when done.  |' ; echo '  \\----------------------------------/'; echo; echo; $SHELL\""))

(defun run-xfce4 ()
  "Run LXDE (xterm)"
  (do-run-other-window-manager "( xfce4-session &) ; xterm -e \"echo '  /----------------------------------\\' ; echo '  |  CLFSWM Note:                    |' ; echo '  |    Close this window when done.  |' ; echo '  \\----------------------------------/'; echo; echo; $SHELL\""))


(defun run-prompt-wm ()
  "Prompt for an other window manager"
  (let ((wm (query-string "Run an other window manager:" "icewm")))
    (do-run-other-window-manager wm)))


;;; Hide or show unmanaged windows utility.
(defun set-hide-unmanaged-window ()
  "Hide unmanaged windows when frame is not selected"
  (when (frame-p *current-child*)
    (setf (frame-data-slot *current-child* :unmanaged-window-action) :hide)
    (leave-second-mode)))

(defun set-show-unmanaged-window ()
  "Show unmanaged windows when frame is not selected"
  (when (frame-p *current-child*)
    (setf (frame-data-slot *current-child* :unmanaged-window-action) :show)
    (leave-second-mode)))

(defun set-default-hide-unmanaged-window ()
  "Set default behaviour to hide or not unmanaged windows when frame is not selected"
  (when (frame-p *current-child*)
    (setf (frame-data-slot *current-child* :unmanaged-window-action) nil)
    (leave-second-mode)))

(defun set-globally-hide-unmanaged-window ()
  "Hide unmanaged windows by default. This is overriden by functions above"
  (setf *hide-unmanaged-window* t)
  (leave-second-mode))

(defun set-globally-show-unmanaged-window ()
  "Show unmanaged windows by default. This is overriden by functions above"
  (setf *hide-unmanaged-window* nil)
  (leave-second-mode))


;;; Speed mouse movement.
(let (minx miny maxx maxy history lx ly)
  (labels ((middle (x1 x2)
	     (round (/ (+ x1 x2) 2)))
	   (reset-if-moved (x y)
	     (when (or (/= x (or lx x)) (/= y (or ly y)))
	       (speed-mouse-reset)))
	   (add-in-history (x y)
	     (push (list x y) history)))
    (defun speed-mouse-reset ()
      "Reset speed mouse coordinates"
      (setf minx nil miny nil maxx nil maxy nil history nil lx nil ly nil))
    (defun speed-mouse-left ()
      "Speed move mouse to left"
      (with-x-pointer
	(reset-if-moved x y)
	(setf maxx x)
	(add-in-history x y)
	(setf lx (middle (or minx 0) maxx))
	(xlib:warp-pointer *root* lx y)))
    (defun speed-mouse-right ()
      "Speed move mouse to right"
      (with-x-pointer
	(reset-if-moved x y)
	(setf minx x)
	(add-in-history x y)
	(setf lx (middle minx (or maxx (xlib:screen-width *screen*))))
	(xlib:warp-pointer *root* lx y)))
    (defun speed-mouse-up ()
      "Speed move mouse to up"
      (with-x-pointer
	(reset-if-moved x y)
	(setf maxy y)
	(add-in-history x y)
	(setf ly (middle (or miny 0) maxy))
	(xlib:warp-pointer *root* x ly)))
    (defun speed-mouse-down ()
      "Speed move mouse to down"
      (with-x-pointer
	(reset-if-moved x y)
	(setf miny y)
	(add-in-history x y)
	(setf ly (middle miny (or maxy (xlib:screen-height *screen*))))
	(xlib:warp-pointer *root* x ly)))
    (defun speed-mouse-undo ()
      "Undo last speed mouse move"
      (when history
	(let ((h (pop history)))
	  (when h
	    (destructuring-bind (bx by) h
	      (setf lx bx ly by
		    minx nil  maxx nil
		    miny nil  maxy nil)
	      (xlib:warp-pointer *root* lx ly))))))
    (defun speed-mouse-first-history ()
      "Revert to the first speed move mouse"
      (when history
	(let ((h (first (last history))))
	  (when h
	    (setf lx (first h)
		  ly (second h))
	    (xlib:warp-pointer *root* lx ly)))))))



;;; Notify window functions
(let (font
      window
      gc
      width height
      text
      current-child)
  (labels ((text-string (tx)
	     (typecase tx
	       (cons (first tx))
	       (t tx)))
	   (text-color (tx)
	     (get-color (typecase tx
			  (cons (second tx))
			  (t *notify-window-foreground*)))))
    (defun is-notify-window-p (win)
      (when (and (xlib:window-p win) (xlib:window-p window))
	(xlib:window-equal win window)))

    (defun refresh-notify-window ()
      (add-timer 0.1 #'refresh-notify-window :refresh-notify-window)
      (raise-window window)
      (let ((text-height (- (xlib:font-ascent font) (xlib:font-descent font))))
	(loop for tx in text
	   for i from 1 do
	     (setf (xlib:gcontext-foreground gc) (text-color tx))
	     (xlib:draw-glyphs window gc
			       (truncate (/ (- width (* (xlib:max-char-width font) (length (text-string tx)))) 2))
			       (* text-height i 2)
			       (text-string tx)))))

    (defun close-notify-window ()
      (erase-timer :refresh-notify-window)
      (setf *never-managed-window-list*
	    (remove (list #'is-notify-window-p 'raise-window)
		    *never-managed-window-list* :test #'equal))
      (when gc
	(xlib:free-gcontext gc))
      (when window
	(xlib:destroy-window window))
      (when font
	(xlib:close-font font))
      (xlib:display-finish-output *display*)
      (setf window nil
	    gc nil
	    font nil))

    (defun open-notify-window (text-list)
      (close-notify-window)
      (setf font (xlib:open-font *display* *notify-window-font-string*))
      (let ((text-height (- (xlib:font-ascent font) (xlib:font-descent font))))
	(setf text text-list)
	(setf width (* (xlib:max-char-width font) (+ (loop for tx in text-list
							maximize (length (text-string tx))) 2))
	      height (+ (* text-height (length text-list) 2) text-height))
	(with-placement (*notify-window-placement* x y width height)
	  (setf window (xlib:create-window :parent *root*
					   :x x
					   :y y
					   :width width
					   :height height
					   :background (get-color *notify-window-background*)
					   :border-width *border-size*
					   :border (get-color *notify-window-border*)
					   :colormap (xlib:screen-default-colormap *screen*)
					   :event-mask '(:exposure :key-press))
		gc (xlib:create-gcontext :drawable window
					 :foreground (get-color *notify-window-foreground*)
					 :background (get-color *notify-window-background*)
					 :font font
					 :line-style :solid))
	  (when (frame-p *current-child*)
	    (setf current-child *current-child*)
	    (push (list #'is-notify-window-p 'raise-window) *never-managed-window-list*))
	  (map-window window)
	  (refresh-notify-window)
	  (xlib:display-finish-output *display*))))))


(defun display-hello-window ()
  (open-notify-window '(("Welcome to CLFSWM" "yellow")
			"Press Alt+F1 for help"))
  (add-timer *notify-window-delay* #'close-notify-window))


;;; Run or raise functions
(defun run-or-raise (raisep run-fn &key (maximized nil))
  (let ((window (with-all-windows (*root-frame* win)
		  (when (funcall raisep win)
		    (return win)))))
    (if window
        (let ((parent (find-parent-frame window)))
          (setf *current-child* parent)
	  (put-child-on-top window parent)
          (when maximized
            (setf *current-root* parent))
	  (focus-all-children window parent)
          (show-all-children t))
        (funcall run-fn))))


