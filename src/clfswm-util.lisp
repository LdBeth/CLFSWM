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
  "Add a default frame"
  (when (frame-p *current-child*)
    (let ((name (query-string "Frame name")))
      (push (create-frame :name name) (frame-child *current-child*))))
  (leave-second-mode))
    

(defun add-placed-frame ()
  "Add a placed frame"
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
      (remove-child-in-frame window (find-father-frame window))
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
      (with-all-windows-frames (*current-root* child)
	(when (and (<= (xlib:drawable-x child) x (+ (xlib:drawable-x child) (xlib:drawable-width child)))
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
      (with-all-windows-frames (*current-root* child)
	(when (and (<= (xlib:drawable-x child) x (+ (xlib:drawable-x child) (xlib:drawable-width child)))
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
  (remove-child-in-frame *current-child* (find-father-frame *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (show-all-children))

(defun remove-current-child ()
  "Remove the current child from its father frame"
  (hide-all *current-child*)
  (remove-child-in-frame *current-child* (find-father-frame *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (leave-second-mode))
      

(defun paste-selection-no-clear ()
  "Paste the selection in the current frame - Do not clear the selection after paste"
  (let ((frame-dest (typecase *current-child*
		      (xlib:window (find-father-frame *current-child* *current-root*))
		      (frame *current-child*))))
    (when frame-dest
      (dolist (child *child-selection*)
	(pushnew child (frame-child frame-dest)))
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
		 (when function
		   (xlib:draw-image-glyphs window gc 10 (+ (* pos (+ (xlib:max-char-ascent font) (xlib:max-char-descent font))) 5)
					   (format nil "~A ~A" msg (documentation function 'function))))))
	     (print-key (code state keysym key modifiers)
	       (xlib:clear-area window)
	       (setf (xlib:gcontext-foreground gc) (get-color *identify-foreground*))
	       (xlib:draw-image-glyphs window gc 5 (+ (xlib:max-char-ascent font) 5)
				       (format nil "Press a key to identify. Press 'q' to stop the identify loop."))
	       (when code
		 (xlib:draw-image-glyphs window gc 10 (+ (* 2 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font))) 5)
					 (format nil "Code=~A  KeySym=~S  Key=~S  Modifiers=~A"
						 code keysym key modifiers))
		 (print-doc "Main mode  : " *main-keys* 3 code state)
		 (print-doc "Second mode: " *second-keys* 4 code state)))
	     (handle-identify-key (&rest event-slots &key root code state &allow-other-keys)
	       (declare (ignore event-slots root))
	       (let* ((modifiers (xlib:make-state-keys state))
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
    (focus-all-children frame (or (find-father-frame frame *current-root*)
				(find-father-frame frame)
				*root-frame*))))


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
    (push (create-frame :name (query-string "Frame name")) (frame-child frame))))



(defun open-frame-by-name ()
  "Open a new frame in a named frame"
  (open-frame-by (find-frame-by-name (ask-frame-name "Open a new frame in")))
  (leave-second-mode))

(defun open-frame-by-number ()
  "Open a new frame in a numbered frame"
  (open-frame-by (find-frame-by-name (ask-frame-name "Open a new frame in the grou numbered:")))
  (leave-second-mode))


;;; Delete by functions
(defun delete-frame-by (frame)
  (hide-all *current-root*)
  (unless (equal frame *root-frame*)
    (when (equal frame *current-root*)
      (setf *current-root* *root-frame*))
    (when (equal frame *current-child*)
      (setf *current-child* *current-root*))
    (remove-child-in-frame frame (find-father-frame frame))))


(defun delete-frame-by-name ()
  "Delete a frame by name"
  (delete-frame-by (find-frame-by-name (ask-frame-name "Delete frame")))
  (leave-second-mode))

(defun delete-frame-by-number ()
  "Delete a frame by number"
  (delete-frame-by (find-frame-by-number (query-number "Delete frame by number:")))
  (leave-second-mode))


;;; Move by function
(defun move-current-child-by (child frame-dest)
  (when (and child (frame-p frame-dest))
    (hide-all *current-root*)
    (remove-child-in-frame child (find-father-frame child))
    (pushnew child (frame-child frame-dest))
    (focus-all-children child frame-dest)))

(defun move-current-child-by-name ()
  "Move current child in a named frame"
  (move-current-child-by *current-child*
			 (find-frame-by-name
			  (ask-frame-name (format nil "Move '~A' to frame" (child-name *current-child*)))))
  (leave-second-mode))

(defun move-current-child-by-number ()
  "Move current child in a numbered frame"
  (move-current-child-by *current-child*
			 (find-frame-by-number
			  (query-number (format nil "Move '~A' to frame numbered:" (child-name *current-child*)))))
  (leave-second-mode))


;;; Copy by function
(defun copy-current-child-by (child frame-dest)
  (when (and child (frame-p frame-dest))
    (hide-all *current-root*)
    (pushnew child (frame-child frame-dest))
    (focus-all-children child frame-dest)))

(defun copy-current-child-by-name ()
  "Copy current child in a named frame"
  (copy-current-child-by *current-child*
			 (find-frame-by-name
			  (ask-frame-name (format nil "Copy '~A' to frame" (child-name *current-child*)))))
  (leave-second-mode))

(defun copy-current-child-by-number ()
  "Copy current child in a numbered frame"
  (copy-current-child-by *current-child*
			 (find-frame-by-number
			  (query-number (format nil "Copy '~A' to frame numbered:" (child-name *current-child*)))))
  (leave-second-mode))





;;; Force window functions
(defun force-window-in-frame ()
  "Force the current window to move in the frame (Useful only for transient windows)"
  (when (xlib:window-p *current-child*)
    (let ((father (find-father-frame *current-child*)))
      (with-xlib-protect
	(setf (xlib:drawable-x *current-child*) (frame-rx father)
	      (xlib:drawable-y *current-child*) (frame-ry father)))))
  (leave-second-mode))

(defun force-window-center-in-frame ()
  "Force the current window to move in the center of the frame (Useful only for transient windows)"
  (when (xlib:window-p *current-child*)
    (let ((father (find-father-frame *current-child*)))
      (with-xlib-protect
	(setf (xlib:drawable-x *current-child*) (truncate (+ (frame-rx father)
							     (/ (- (frame-rw father)
								   (xlib:drawable-width *current-child*)) 2)))
	      (xlib:drawable-y *current-child*) (truncate (+ (frame-ry father)
							     (/ (- (frame-rh father)
								   (xlib:drawable-height *current-child*)) 2)))))))
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
(defun move-frame (frame father orig-x orig-y)
  (hide-all-children frame)
  (with-slots (window) frame
    (raise-window window)
    (let ((done nil)
	  (dx (- (xlib:drawable-x window) orig-x))
	  (dy (- (xlib:drawable-y window) orig-y)))
      (labels ((motion-notify (&rest event-slots &key root-x root-y &allow-other-keys)
		 (declare (ignore event-slots))
		 (setf (xlib:drawable-x window) (+ root-x dx)
		       (xlib:drawable-y window) (+ root-y dy))
		 (display-frame-info frame))
	       (handle-event (&rest event-slots &key event-key &allow-other-keys)
		 (case event-key
		   (:motion-notify (apply #'motion-notify event-slots))
		   (:button-release (setf done t))
		   (:configure-request (call-hook *configure-request-hook* event-slots))
		   (:configure-notify (call-hook *configure-notify-hook* event-slots))
		   (:map-request (call-hook *map-request-hook* event-slots))
		   (:unmap-notify (call-hook *unmap-notify-hook* event-slots))
		   (:destroy-notify (call-hook *destroy-notify-hook* event-slots))
		   (:mapping-notify (call-hook *mapping-notify-hook* event-slots))
		   (:property-notify (call-hook *property-notify-hook* event-slots))
		   (:create-notify (call-hook *create-notify-hook* event-slots))
		   (:enter-notify (call-hook *enter-notify-hook* event-slots))
		   (:exposure (call-hook *exposure-hook* event-slots)))
		 t))
	(when frame
	  (loop until done
	     do (with-xlib-protect
		  (xlib:display-finish-output *display*)
		  (xlib:process-event *display* :handler #'handle-event))))
	(setf (frame-x frame) (x-px->fl (xlib:drawable-x window) father)
	      (frame-y frame) (y-px->fl (xlib:drawable-y window) father))
	(show-all-children)))))


(defun resize-frame (frame father orig-x orig-y)
  (hide-all-children frame)
  (with-slots (window) frame
    (raise-window window)
    (let ((done nil)
	  (dx (- (xlib:drawable-x window) orig-x))
	  (dy (- (xlib:drawable-y window) orig-y))
	  (lx orig-x)
	  (ly orig-y))
      (labels ((motion-notify (&rest event-slots &key root-x root-y &allow-other-keys)
		 (declare (ignore event-slots))
		 (setf (xlib:drawable-width window) (max (+ (xlib:drawable-width window) (- root-x lx)) 10)
		       (xlib:drawable-height window) (max (+ (xlib:drawable-height window) (- root-y ly)) 10)
		       dx (- dx (- root-x lx))
		       dy (- dy (- root-y ly))
		       lx root-x ly root-y)
		 (display-frame-info frame))
	       (handle-event (&rest event-slots &key event-key &allow-other-keys)
		 (case event-key
		   (:motion-notify (apply #'motion-notify event-slots))
		   (:button-release (setf done t))
		   (:configure-request (call-hook *configure-request-hook* event-slots))
		   (:configure-notify (call-hook *configure-notify-hook* event-slots))
		   (:map-request (call-hook *map-request-hook* event-slots))
		   (:unmap-notify (call-hook *unmap-notify-hook* event-slots))
		   (:destroy-notify (call-hook *destroy-notify-hook* event-slots))
		   (:mapping-notify (call-hook *mapping-notify-hook* event-slots))
		   (:property-notify (call-hook *property-notify-hook* event-slots))
		   (:create-notify (call-hook *create-notify-hook* event-slots))
		   (:enter-notify (call-hook *enter-notify-hook* event-slots))
		   (:exposure (call-hook *exposure-hook* event-slots)))
		 t))
	(when frame
	  (loop until done
	     do (with-xlib-protect
		  (xlib:display-finish-output *display*)
		  (xlib:process-event *display* :handler #'handle-event))))
	(setf (frame-w frame) (w-px->fl (xlib:drawable-width window) father)
	      (frame-h frame) (h-px->fl (xlib:drawable-height window) father))
	(show-all-children)))))

	   

(defun mouse-click-to-focus-generic (window root-x root-y mouse-fn)
  "Focus the current frame or focus the current window father
mouse-fun is #'move-frame or #'resize-frame"
  (let ((to-replay t)
	(child window)
	(father (find-father-frame window *current-root*))
	(root-p (or (equal window *root*)
		    (equal window (frame-window *current-root*)))))
    (when (or (not root-p) *create-frame-on-root*)
      (unless father
	(if root-p
	    (progn
	      (setf child (create-frame)
		    father *current-root*
		    mouse-fn #'resize-frame)
	      (place-frame child father root-x root-y 10 10)
	      (xlib:map-window (frame-window child))
	      (pushnew child (frame-child *current-root*)))
	    (setf child (find-frame-window window *current-root*)
		  father (find-father-frame child *current-root*)))
	(when child
	  (funcall mouse-fn child father root-x root-y)))
      (when (and child father (focus-all-children child father))
	(when (show-all-children)
	  (setf to-replay nil))))
    (if to-replay
	(replay-button-event)
	(stop-button-event))))

(defun mouse-click-to-focus-and-move (window root-x root-y)
  "Move and focus the current frame or focus the current window father"
  (mouse-click-to-focus-generic window root-x root-y #'move-frame))

(defun mouse-click-to-focus-and-resize (window root-x root-y)
  "Resize and focus the current frame or focus the current window father"
  (mouse-click-to-focus-generic window root-x root-y #'resize-frame))




(defun mouse-focus-move/resize-generic (root-x root-y mouse-fn window-father)
  "Focus the current frame or focus the current window father
mouse-fun is #'move-frame or #'resize-frame.
Focus child and its fathers -
For window: set current child to window or its father according to window-father"
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
    (focus-all-children child father window-father)
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
  "Leave the selected frame - ie make its father the root frame"
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
      (current-slot 0))
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
    (show-all-children))
  
  (defun bind-or-jump (n)
    (let ((default-bind `("Return" bind-on-slot
				   ,(format nil "Bind slot ~A on child: ~A" n (child-fullname *current-child*)))))
      (setf current-slot (- n 1))
      (info-mode-menu (aif (aref key-slots current-slot)
			   `(,default-bind
				("BackSpace" remove-binding-on-slot
					     ,(format nil "Remove slot ~A binding on child: ~A" n (child-fullname *current-child*)))
				("   -  " nil " -")
				("Tab" jump-to-slot
					 ,(format nil "Jump to child: ~A" (aif (aref key-slots current-slot)
									       (child-fullname it)
									       "Not set - Please, bind it with Return"))))
			   (list default-bind))))))

(defmacro def-bind-or-jump ()
  `(progn
     ,@(loop for i from 1 to 10
	  collect `(defun ,(intern (format nil "BIND-OR-JUMP-~A" i)) ()
		     ,(format nil "Bind or jump to the child on slot ~A" i)
		     (bind-or-jump ,i)))))


(def-bind-or-jump)
