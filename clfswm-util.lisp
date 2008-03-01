;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; #Date#: Sat Mar  1 00:03:08 2008
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



(defun add-default-group ()
  "Add a default group"
  (when (group-p *current-child*)
    (let ((name (query-string "Group name")))
      (push (create-group :name name) (group-child *current-child*))))
  (leave-second-mode))
    

(defun add-placed-group ()
  "Add a placed group"
  (when (group-p *current-child*)
    (let ((name (query-string "Group name"))
	  (x (/ (query-number "Group x in percent (%)") 100))
	  (y (/ (query-number "Group y in percent (%)") 100))
	  (w (/ (query-number "Group width in percent (%)") 100))
	  (h (/ (query-number "Group height in percent (%)") 100)))
      (push (create-group :name name :x x :y y :w w :h h)
	    (group-child *current-child*))))
  (leave-second-mode))



(defun delete-focus-window ()
  "Delete the focus window in all groups and workspaces"
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (setf *current-child* *current-root*)
      (remove-child-in-all-groups window)
      (send-client-message window :WM_PROTOCOLS
			   (xlib:intern-atom *display* "WM_DELETE_WINDOW"))
      (show-all-childs))))

(defun destroy-focus-window ()
  "Destroy the focus window in all groups and workspaces"
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (setf *current-child* *current-root*)
      (remove-child-in-all-groups window)
      (xlib:kill-client *display* (xlib:window-id window))
      (show-all-childs))))

(defun remove-focus-window ()
  "Remove the focus window in the current group"
  (let ((window (xlib:input-focus *display*)))
    (when (and window (not (xlib:window-equal window *no-focus-window*)))
      (setf *current-child* *current-root*)
      (hide-child window)
      (remove-child-in-group window (find-father-group window))
      (show-all-childs))))


(defun unhide-all-windows-in-current-child ()
  "Unhide all hidden windows into the current child"
  (with-xlib-protect
      (dolist (window (get-hidden-windows))
	(unhide-window window)
	(process-new-window window)
	(xlib:map-window window)))
  (show-all-childs))




(defun find-child-under-mouse (x y)
  "Return the child window under the mouse"
  (with-xlib-protect
      (let ((win nil))
	(with-all-windows-groups (*current-root* child)
	  (when (and (<= (xlib:drawable-x child) x (+ (xlib:drawable-x child) (xlib:drawable-width child)))
		     (<= (xlib:drawable-y child) y (+ (xlib:drawable-y child) (xlib:drawable-height child))))
	    (setf win child))
	  (when (and (<= (group-rx child) x (+ (group-rx child) (group-rw child)))
		     (<= (group-ry child) y (+ (group-ry child) (group-rh child))))
	    (setf win (group-window child))))
	win)))





;;; Selection functions
(defun clear-selection ()
  "Clear the current selection"
  (setf *child-selection* nil)
  (display-group-info *current-root*))

(defun copy-current-child ()
  "Copy the current child to the selection"
  (pushnew *current-child* *child-selection*)
  (display-group-info *current-root*))


(defun cut-current-child ()
  "Cut the current child to the selection"
  (copy-current-child)
  (hide-all-childs *current-child*)
  (remove-child-in-group *current-child* (find-father-group *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (show-all-childs))

(defun remove-current-child ()
  "Remove the current child from its father group"
  (hide-all-childs *current-child*)
  (remove-child-in-group *current-child* (find-father-group *current-child* *current-root*))
  (setf *current-child* *current-root*)
  (leave-second-mode))
      

(defun paste-selection-no-clear ()
  "Paste the selection in the current group - Do not clear the selection after paste"
  (let ((group-dest (typecase *current-child*
		      (xlib:window (find-father-group *current-child* *current-root*))
		      (group *current-child*))))
    (when group-dest
      (dolist (child *child-selection*)
	(pushnew child (group-child group-dest)))
      (show-all-childs))))

(defun paste-selection ()
  "Paste the selection in the current group"
  (paste-selection-no-clear)
  (setf *child-selection* nil)
  (display-group-info *current-root*))



  



;;; CONFIG - Identify mode
(defun identify-key ()
  "Identify a key"
  (let* ((done nil)
	 (font (xlib:open-font *display* *identify-font-string*))
	 (window (xlib:create-window :parent *root*
				     :x 0 :y 0
				     :width (- (xlib:screen-width *screen*) 2)
				     :height (* 3 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font)))
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
    (labels ((print-key (code keysym key modifiers)
	       (xlib:clear-area window)
	       (setf (xlib:gcontext-foreground gc) (get-color *identify-foreground*))
	       (xlib:draw-image-glyphs window gc 5 (+ (xlib:max-char-ascent font) 5)
				       (format nil "Press a key to identify. Press 'q' to stop the identify loop."))
	       (when code
		 (xlib:draw-image-glyphs window gc 10 (+ (* 2 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font))) 5)
					 (format nil "Code=~A  KeySym=~A  Key=~S  Modifiers=~A"
						 code keysym key modifiers))))
	     (handle-identify-key (&rest event-slots &key root code state &allow-other-keys)
	       (declare (ignore event-slots root))
	       (let* ((modifiers (xlib:make-state-keys state))
		      (key (keycode->char code state))
		      (keysym (keysym->keysym-name (xlib:keycode->keysym *display* code 0))))
		 (setf done (and (equal key #\q) (null modifiers)))
		 (dbg code keysym key modifiers)
		 (print-key code keysym key modifiers)
		 (force-output)))
	     (handle-identify (&rest event-slots &key display event-key &allow-other-keys)
	       (declare (ignore display))
	       (case event-key
		 (:key-press (apply #'handle-identify-key event-slots) t)
		 (:exposure (print-key nil nil nil nil)))
	       t))
      (xgrab-pointer *root* 92 93)
      (xlib:map-window window)
      (format t "~&Press 'q' to stop the identify loop~%")
      (print-key nil nil nil nil)
      (force-output)
      (unwind-protect
	   (loop until done do
		(xlib:display-finish-output *display*)
		(xlib:process-event *display* :handler #'handle-identify))
	(xlib:destroy-window window)
	(xlib:close-font font)
	(xgrab-pointer *root* 66 67)))))



(defun query-show-paren (orig-string pos)
  "Replace matching parentheses with brackets"
  (let ((string (copy-seq orig-string))) 
    (labels ((have-to-find-right? ()
	       (and (< pos (length string)) (char= (aref string pos) #\()))
	     (have-to-find-left? ()
	       (and (> (1- pos) 0) (char= (aref string (1- pos)) #\))))
	     (pos-right ()
	       (loop :for p :from (1+ pos) :below (length string)
		  :with level = 1   :for c = (aref string p)
		  :do (when (char= c #\() (incf level))
		  (when (char= c #\)) (decf level))
		  (when (= level 0) (return p))))
	     (pos-left ()
	       (loop :for p :from (- pos 2) :downto 0
		  :with level = 1   :for c = (aref string p)
		  :do (when (char= c #\() (decf level))
		  (when (char= c #\)) (incf level))
		  (when (= level 0) (return p)))))
      (when (have-to-find-right?)
	(let ((p (pos-right)))
	  (when p (setf (aref string p) #\]))))
      (when (have-to-find-left?)
	(let ((p (pos-left)))
	  (when p (setf (aref string p) #\[))))
      string)))


;;; CONFIG - Query string mode
(let ((history nil))
  (defun clear-history ()
    "Clear the query-string history"
    (setf history nil))
  
  (defun query-string (msg &optional (default ""))
    "Query a string from the keyboard. Display msg as prompt"
    (let* ((done nil)
	   (font (xlib:open-font *display* *query-font-string*))
	   (window (xlib:create-window :parent *root*
				       :x 0 :y 0
				       :width (- (xlib:screen-width *screen*) 2)
				       :height (* 3 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font)))
				       :background (get-color *query-background*)
				       :border-width 1
				       :border (get-color *query-border*)
				       :colormap (xlib:screen-default-colormap *screen*)
				       :event-mask '(:exposure)))
	   (gc (xlib:create-gcontext :drawable window
				     :foreground (get-color *query-foreground*)
				     :background (get-color *query-background*)
				     :font font
				     :line-style :solid))
	   (result-string default)
	   (pos (length default))
	   (local-history history))
      (labels ((add-cursor (string)
		 (concatenate 'string (subseq string 0 pos) "|" (subseq string pos)))
	       (print-string ()
		 (xlib:clear-area window)
		 (setf (xlib:gcontext-foreground gc) (get-color *query-foreground*))
		 (xlib:draw-image-glyphs window gc 5 (+ (xlib:max-char-ascent font) 5) msg)
		 (when (< pos 0) (setf pos 0))
		 (when (> pos (length result-string)) (setf pos (length result-string)))
		 (xlib:draw-image-glyphs window gc 10 (+ (* 2 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font))) 5)
					 (add-cursor (query-show-paren result-string pos))))
	       (call-backspace (modifiers)
		 (let ((del-pos (if (member :control modifiers)
				    (or (position #\Space result-string :from-end t :end pos) 0)
				    (1- pos))))
		   (when (>= del-pos 0)
		     (setf result-string (concatenate 'string
						      (subseq result-string 0 del-pos)
						      (subseq result-string pos))
			   pos del-pos))))
	       (call-delete (modifiers)
		 (let ((del-pos (if (member :control modifiers)
				    (1+ (or (position #\Space result-string :start pos) (1- (length result-string))))
				    (1+ pos))))
		   (if (<= del-pos (length result-string))
		       (setf result-string (concatenate 'string
							(subseq result-string 0 pos)
							(subseq result-string del-pos))))))
	       (call-delete-eof ()
		 (setf result-string (subseq result-string 0 pos)))
	       (handle-query-key (&rest event-slots &key root code state &allow-other-keys)
		 (declare (ignore event-slots root))
		 (let* ((modifiers (xlib:make-state-keys state))
			(keysym (xlib:keycode->keysym *display* code (cond  ((member :shift modifiers) 1)
									    ((member :mod-5 modifiers) 2)
									    (t 0))))
			(char (xlib:keysym->character *display* keysym))
			(keysym-name (keysym->keysym-name keysym)))
		   (setf done (cond ((string-equal keysym-name "Return") :Return)
				    ((string-equal keysym-name "Tab") :Complet)
				    ((string-equal keysym-name "Escape") :Escape)
				    (t nil)))
		   (cond ((string-equal keysym-name "Left")
			  (when (> pos 0)
			    (setf pos (if (member :control modifiers)
					  (let ((p (position #\Space result-string
							     :end (min (1- pos) (length result-string))
							     :from-end t)))
					    (if p p 0))
					  (1- pos)))))
			 ((string-equal keysym-name "Right")
			  (when (< pos (length result-string))
			    (setf pos (if (member :control modifiers)
					  (let ((p (position #\Space result-string
							     :start (min (1+ pos) (length result-string)))))
					    (if p p (length result-string)))
					  (1+ pos)))))
			 ((string-equal keysym-name "Up")
			  (setf result-string (first local-history)
				pos (length result-string)
				local-history (rotate-list local-history)))
			 ((string-equal keysym-name "Down")
			  (setf result-string (first local-history)
				pos (length result-string)
				local-history (anti-rotate-list local-history)))
			 ((string-equal keysym-name "Home") (setf pos 0))
			 ((string-equal keysym-name "End") (setf pos (length result-string)))
			 ((string-equal keysym-name "Backspace") (call-backspace modifiers))
			 ((string-equal keysym-name "Delete") (call-delete modifiers))
			 ((and (string-equal keysym-name "k") (member :control modifiers))
			  (call-delete-eof))
			 ((and (characterp char) (standard-char-p char))
			  (setf result-string (concatenate 'string
							   (when (<= pos (length result-string))
							     (subseq result-string 0 pos))
							   (string char)
							   (when (< pos (length result-string))
							     (subseq result-string pos))))
			  (incf pos)))
		   (print-string)))
	       (handle-query (&rest event-slots &key display event-key &allow-other-keys)
		 (declare (ignore display))
		 (case event-key
		   (:key-press (apply #'handle-query-key event-slots) t)
		   (:exposure (print-string)))
		 t))
	(xgrab-pointer *root* 92 93)
	(xlib:map-window window)
	(print-string)
	(wait-no-key-or-button-press)
	(unwind-protect
	     (loop until (member done '(:Return :Escape :Complet)) do
		  (xlib:display-finish-output *display*)
		  (xlib:process-event *display* :handler #'handle-query))
	  (xlib:destroy-window window)
	  (xlib:close-font font)
	  (xgrab-pointer *root* 66 67)))
      (values (when (member done '(:Return :Complet))
		(push result-string history)
		result-string)
	      done))))



(defun query-number (msg)
  "Query a number from the query input"
  (parse-integer (or (query-string msg) "") :junk-allowed t))



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




;;; Group name actions
;;;(loop :for str :in '("The Gimp" "The klm" "klm" "abc")  ;; Test
;;;   :when (zerop (or (search "ThE" str :test #'string-equal) -1))
;;;   :collect str)
(defun ask-group-name (msg)
  "Ask a group name"
  (let ((all-group-name nil)
	(name ""))
    (with-all-groups (*root-group* group)
      (awhen (group-name group) (push it all-group-name)))
    (labels ((selected-names ()
	       (loop :for str :in all-group-name
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
(defun focus-group-by (group)
  (when (group-p group)
    (focus-all-childs group (or (find-father-group group *current-root*)
				(find-father-group group)
				*root-group*))))


(defun focus-group-by-name ()
  "Focus a group by name"
  (focus-group-by (find-group-by-name (ask-group-name "Focus group")))
  (leave-second-mode))

(defun focus-group-by-number ()
  "Focus a group by number"
  (focus-group-by (find-group-by-number (query-number "Focus group by number:")))
  (leave-second-mode))


;;; Open by functions
(defun open-group-by (group)
  (when (group-p group)
    (push (create-group :name (query-string "Group name")) (group-child group))))



(defun open-group-by-name ()
  "Open a new group in a named group"
  (open-group-by (find-group-by-name (ask-group-name "Open a new group in")))
  (leave-second-mode))

(defun open-group-by-number ()
  "Open a new group in a numbered group"
  (open-group-by (find-group-by-name (ask-group-name "Open a new group in the grou numbered:")))
  (leave-second-mode))


;;; Delete by functions
(defun delete-group-by (group)
  (unless (equal group *root-group*)
    (when (equal group *current-root*)
      (setf *current-root* *root-group*))
    (when (equal group *current-child*)
      (setf *current-child* *current-root*))
    (remove-child-in-group group (find-father-group group))))


(defun delete-group-by-name ()
  "Delete a group by name"
  (delete-group-by (find-group-by-name (ask-group-name "Delete group")))
  (leave-second-mode))

(defun delete-group-by-number ()
  "Delete a group by number"
  (delete-group-by (find-group-by-number (query-number "Delete group by number:")))
  (leave-second-mode))


;;; Move by function
(defun move-current-child-by (child group-dest)
  (when (and child (group-p group-dest))
    (remove-child-in-group child (find-father-group child))
    (pushnew child (group-child group-dest))
    (focus-all-childs child group-dest)))

(defun move-current-child-by-name ()
  "Move current child in a named group"
  (move-current-child-by *current-child*
			 (find-group-by-name
			  (ask-group-name (format nil "Move '~A' to group" (child-name *current-child*)))))
  (leave-second-mode))

(defun move-current-child-by-number ()
  "Move current child in a numbered group"
  (move-current-child-by *current-child*
			 (find-group-by-number
			  (query-number (format nil "Move '~A' to group numbered:" (child-name *current-child*)))))
  (leave-second-mode))


;;; Copy by function
(defun copy-current-child-by (child group-dest)
  (when (and child (group-p group-dest))
    (pushnew child (group-child group-dest))
    (focus-all-childs child group-dest)))

(defun copy-current-child-by-name ()
  "Copy current child in a named group"
  (copy-current-child-by *current-child*
			 (find-group-by-name
			  (ask-group-name (format nil "Copy '~A' to group" (child-name *current-child*)))))
  (leave-second-mode))

(defun copy-current-child-by-number ()
  "Copy current child in a numbered group"
  (copy-current-child-by *current-child*
			 (find-group-by-number
			  (query-number (format nil "Copy '~A' to group numbered:" (child-name *current-child*)))))
  (leave-second-mode))






;;;;;,-----
;;;;;| Various definitions
;;;;;`-----
;;(defun stop-all-pending-actions ()
;;  "Stop all pending actions (actions like open in new workspace/group)"
;;  (setf *open-next-window-in-new-workspace* nil
;;	*open-next-window-in-new-group* nil
;;	*arrow-action* nil
;;	*pager-arrow-action* nil))
;;
;;(defun rotate-window-up ()
;;  "Rotate up windows in the current group"
;;  (setf (group-window-list (current-group))
;;	(rotate-list (group-window-list (current-group))))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;(defun rotate-window-down ()
;;  "Rotate down windows in the current group"
;;  (setf (group-window-list (current-group))
;;	(anti-rotate-list (group-window-list (current-group))))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;
;;(defun maximize-group (group)
;;  "Maximize the group"
;;  (when group
;;    (unless (group-fullscreenp group)
;;      (setf (group-fullscreenp group) t)
;;      (show-all-windows-in-workspace (current-workspace)))))
;;
;;(defun minimize-group (group)
;;  "Minimize the group"
;;  (when group
;;    (when (group-fullscreenp group)
;;      (setf (group-fullscreenp group) nil)
;;      (show-all-windows-in-workspace (current-workspace)))))
;;
;;(defun toggle-maximize-group (group)
;;  "Maximize/minimize a group"
;;  (if (group-fullscreenp group)
;;      (minimize-group group)
;;      (maximize-group group)))
;;
;;
;;(defun toggle-maximize-current-group ()
;;  "Maximize/minimize the current group"
;;  (toggle-maximize-group (current-group)))
;;
;;
;;(defun renumber-workspaces ()
;;  "Reset workspaces numbers (1 for current workspace, 2 for the second...) "
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (setf *current-workspace-number* 0)
;;  (loop for workspace in *workspace-list* do
;;       (setf (workspace-number workspace) (incf *current-workspace-number*)))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;(defun sort-workspaces ()
;;  "Sort workspaces by numbers"
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (setf *workspace-list* (sort *workspace-list*
;;			       #'(lambda (x y)
;;				   (< (workspace-number x) (workspace-number y)))))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;
;;(defun circulate-group-up ()
;;  "Circulate up in group"
;;  (banish-pointer)
;;  (minimize-group (current-group))
;;  (no-focus)
;;  (setf (workspace-group-list (current-workspace))
;;	(rotate-list (workspace-group-list (current-workspace))))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;
;;(defun circulate-group-up-move-window ()
;;  "Circulate up in group moving the current window in the next group"
;;  (banish-pointer)
;;  (minimize-group (current-group))
;;  (no-focus)
;;  (let ((window (current-window)))
;;    (remove-window-in-group window (current-group))
;;    (focus-window (current-window))
;;    (setf (workspace-group-list (current-workspace))
;;	  (rotate-list (workspace-group-list (current-workspace))))
;;    (add-window-in-group window (current-group)))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;(defun circulate-group-up-copy-window ()
;;  "Circulate up in group copying the current window in the next group"
;;  (banish-pointer)
;;  (minimize-group (current-group))
;;  (no-focus)
;;  (let ((window (current-window)))
;;    (setf (workspace-group-list (current-workspace))
;;	  (rotate-list (workspace-group-list (current-workspace))))
;;    (unless (window-already-in-workspace window (current-workspace))
;;      (add-window-in-group window (current-group))))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;
;;
;;(defun circulate-group-down ()
;;  "Circulate down in group"
;;  (banish-pointer)
;;  (minimize-group (current-group))
;;  (no-focus)
;;  (setf (workspace-group-list (current-workspace))
;;	(anti-rotate-list (workspace-group-list (current-workspace))))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;(defun circulate-group-down-move-window ()
;;  "Circulate down in group moving the current window in the next group"
;;  (banish-pointer)
;;  (minimize-group (current-group))
;;  (no-focus)
;;  (let ((window (current-window)))
;;    (remove-window-in-group window (current-group))
;;    (focus-window (current-window))
;;    (setf (workspace-group-list (current-workspace))
;;	  (anti-rotate-list (workspace-group-list (current-workspace))))
;;    (add-window-in-group window (current-group)))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;(defun circulate-group-down-copy-window ()
;;  "Circulate down in group copying the current window in the next group"
;;  (banish-pointer)
;;  (minimize-group (current-group))
;;  (no-focus)
;;  (let ((window (current-window)))
;;    (setf (workspace-group-list (current-workspace))
;;	  (anti-rotate-list (workspace-group-list (current-workspace))))
;;    (unless (window-already-in-workspace window (current-workspace))
;;      (add-window-in-group window (current-group))))
;;  (adapt-window-to-group (current-window) (current-group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;
;;
;;
;;
;;(defun circulate-workspace-by-number (number)
;;  "Focus a workspace given its number"
;;  (no-focus)
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (dotimes (i (length *workspace-list*))
;;    (when (= (workspace-number (current-workspace)) number)
;;      (return))
;;    (setf *workspace-list* (rotate-list *workspace-list*)))
;;  (show-all-windows-in-workspace (current-workspace)))
;;  
;;
;;(defun circulate-workspace-up ()
;;  "Circulate up in workspace"
;;  (no-focus)
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (setf *workspace-list* (rotate-list *workspace-list*))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;(defun circulate-workspace-up-move-group ()
;;  "Circulate up in workspace moving current group in the next workspace"
;;  (no-focus)
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (let ((group (current-group)))
;;    (remove-group-in-workspace group (current-workspace))
;;    (setf *workspace-list* (rotate-list *workspace-list*))
;;    (add-group-in-workspace (copy-group group) (current-workspace)))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;(defun circulate-workspace-up-copy-group ()
;;  "Circulate up in workspace copying current group in the next workspace"
;;  (no-focus)
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (let ((group (current-group)))
;;    (setf *workspace-list* (rotate-list *workspace-list*))
;;    (unless (group-windows-already-in-workspace group (current-workspace))
;;      (add-group-in-workspace (copy-group group) (current-workspace))))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;(defun circulate-workspace-down ()
;;  "Circulate down in workspace"
;;  (no-focus)
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (setf *workspace-list* (anti-rotate-list *workspace-list*))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;(defun circulate-workspace-down-move-group ()
;;  "Circulate down in workspace moving current group in the next workspace"
;;  (no-focus)
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (let ((group (current-group)))
;;    (remove-group-in-workspace group (current-workspace))
;;    (setf *workspace-list* (anti-rotate-list *workspace-list*))
;;    (add-group-in-workspace (copy-group group) (current-workspace)))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;(defun circulate-workspace-down-copy-group ()
;;  "Circulate down in workspace copying current group in the next workspace"
;;  (no-focus)
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (let ((group (current-group)))
;;    (setf *workspace-list* (anti-rotate-list *workspace-list*))
;;    (unless (group-windows-already-in-workspace group (current-workspace))
;;      (add-group-in-workspace (copy-group group) (current-workspace))))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;(defun delete-current-window ()
;;  "Delete the current window in all groups and workspaces"
;;  (let ((window (current-window)))
;;    (when window
;;      (no-focus)
;;      (remove-window-in-all-workspace window)
;;      (send-client-message window :WM_PROTOCOLS
;;			   (intern-atom *display* "WM_DELETE_WINDOW"))))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;
;;(defun destroy-current-window ()
;;  "Destroy the current window in all groups and workspaces"
;;  (let ((window (current-window)))
;;    (when window
;;      (no-focus)
;;      (remove-window-in-all-workspace window)
;;      (kill-client *display* (xlib:window-id window))))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;(defun remove-current-window ()
;;  "Remove the current window in the current group"
;;  (let ((window (current-window)))
;;    (when window
;;      (no-focus)
;;      (hide-window window)
;;      (remove-window-in-group (current-window) (current-group))))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;(defun remove-current-group ()
;;  "Remove the current group in the current workspace"
;;  (minimize-group (current-group))
;;  (let ((group (current-group)))
;;    (when group
;;      (no-focus)
;;      (dolist (window (group-window-list group))
;;	(when window
;;	  (hide-window window)))
;;      (remove-group-in-workspace group (current-workspace))))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;(defun remove-current-workspace ()
;;  "Remove the current workspace"
;;  (let ((workspace (current-workspace)))
;;    (when workspace
;;      (hide-all-windows-in-workspace workspace)
;;      (remove-workspace workspace)
;;      (show-all-windows-in-workspace (current-workspace)))))
;;
;;
;;(defun unhide-all-windows-in-current-group ()
;;  "Unhide all hidden windows into the current group"
;;  (let ((all-windows (get-all-windows))
;;	(hidden-windows (remove-if-not #'window-hidden-p
;;				       (copy-list (xlib:query-tree *root*))))
;;	(current-group (current-group)))
;;    (dolist (window (set-difference hidden-windows all-windows))
;;      (unhide-window window)
;;      (process-new-window window)
;;      (xlib:map-window window)
;;      (adapt-window-to-group window current-group)))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;
;;
;;
;;(defun create-new-default-group ()
;;  "Create a new default group"
;;  (minimize-group (current-group))
;;  (add-group-in-workspace (copy-group *default-group*)
;;			  (current-workspace))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;(defun create-new-default-workspace ()
;;  "Create a new default workspace"
;;  (hide-all-windows-in-workspace (current-workspace))
;;  (add-workspace (create-default-workspace))
;;  (show-all-windows-in-workspace (current-workspace)))
;;
;;
;;
;;
;;;;;,-----
;;;;;| Group moving
;;;;;`-----
;;(defun move-group (group dx dy)
;;  "Move group"
;;  (setf (group-x group) (+ (group-x group) dx)
;;	(group-y group) (+ (group-y group) dy))
;;  (dolist (window (group-window-list group))
;;    (adapt-window-to-group window group))
;;  (show-all-group (current-workspace)))
;;
;;(defun move-group-to (group x y)
;;  "Move group to"
;;  (setf (group-x group) x
;;	(group-y group) y)
;;  (dolist (window (group-window-list group))
;;    (adapt-window-to-group window group))
;;  (focus-window (current-window))
;;  (show-all-group (current-workspace)))
;;
;;
;;(defun resize-group (group dx dy)
;;  "Resize group"
;;  (setf (group-width group) (max (+ (group-width group) dx) 100)
;;	(group-height group) (max (+ (group-height group) dy) 100))
;;  (dolist (window (group-window-list group))
;;    (adapt-window-to-group window group))
;;  (show-all-group (current-workspace)))
;;
;;(defun force-window-in-group ()
;;  "Force the current window to move in the group (Useful only for transient windows)"
;;  (let ((group (current-group))
;;	(window (current-window)))
;;    (when window
;;      (setf (xlib:drawable-x window) (group-x group)
;;	    (xlib:drawable-y window) (group-y group))
;;      (show-all-windows-in-workspace (current-workspace)))))
;;
;;(defun force-window-center-in-group ()
;;  "Force the current window to move in the center of the group (Useful only for transient windows)"
;;  (let ((group (current-group))
;;	(window (current-window)))
;;    (when window
;;      (setf (xlib:drawable-x window) (truncate (+ (group-x group)
;;						  (/ (- (group-width group) (xlib:drawable-width window)) 2)))
;;	    (xlib:drawable-y window) (truncate (+ (group-y group)
;;						  (/ (- (group-height group) (xlib:drawable-height window)) 2))))
;;      (show-all-windows-in-workspace (current-workspace)))))
;;
;;
;;
;;  
;;
;;(defun show-help (&optional (browser "dillo") (tempfile "/tmp/clfswm.html"))
;;  "Show current keys and buttons bindings"
;;  (ignore-errors
;;    (produce-doc-html-in-file tempfile))
;;  (sleep 1)
;;  (do-shell (format nil "~A ~A" browser tempfile)))
