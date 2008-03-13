;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Main functions
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


;;; Minimal hook
(defun call-hook (hook &optional args)
  "Call a hook (a function, a symbol or a list of functions)
Return the result of the last hook"
  (let ((result nil))
    (labels ((rec (hook)
	       (when hook
		 (typecase hook
		   (cons (dolist (h hook)
			   (rec h)))
		   (t (setf result (apply hook args)))))))
      (rec hook)
      result)))




(defgeneric group-p (group))
(defmethod group-p ((group group))
  (declare (ignore group))
  t)
(defmethod group-p (group)
  (declare (ignore group))
  nil)



;;; Group data manipulation functions
(defun group-data-slot (group slot)
  "Return the value associated to data slot"
  (when (group-p group)
    (second (assoc slot (group-data group)))))

(defun set-group-data-slot (group slot value)
  "Set the value associated to data slot"
  (when (group-p group)
    (with-slots (data) group
      (setf data (remove (assoc slot data) data))
      (push (list slot value) data))
    value))

(defsetf group-data-slot set-group-data-slot)






(defgeneric child-name (child))

(defmethod child-name ((child xlib:window))
  (xlib:wm-name child))

(defmethod child-name ((child group))
  (group-name child))

(defmethod child-name (child)
  (declare (ignore child))
  "???")



(defgeneric rename-child (child name))

(defmethod rename-child ((child group) name)
  (setf (group-name child) name))

(defmethod rename-child ((child xlib:window) name)
  (setf (xlib:wm-name child) name))

(defmethod rename-child (child name)
  (declare (ignore child name)))



;; (with-all-childs (*root-group* child) (typecase child (xlib:window (print child)) (group (print (group-number child)))))
(defmacro with-all-childs ((root child) &body body)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(labels ((,rec (,child)
		,@body
		(when (group-p ,child)
		  (dolist (,sub-child (group-child ,child))
		    (,rec ,sub-child)))))
       (,rec ,root))))


;; (with-all-group (*root-group* group) (print (group-number group)))
(defmacro with-all-groups ((root group) &body body)
  (let ((rec (gensym))
	(child (gensym)))
    `(labels ((,rec (,group)
		(when (group-p ,group)
		  ,@body
		  (dolist (,child (group-child ,group))
		    (,rec ,child)))))
       (,rec ,root))))


;; (with-all-windows (*root-group* window) (print window))
(defmacro with-all-windows ((root window) &body body)
  (let ((rec (gensym))
	(child (gensym)))
    `(labels ((,rec (,window)
		(when (xlib:window-p ,window)
		  ,@body)
		(when (group-p ,window)
		  (dolist (,child (group-child ,window))
		    (,rec ,child)))))
       (,rec ,root))))



;; (with-all-groups-windows (*root-group* child) (print child) (print (group-number child)))
(defmacro with-all-windows-groups ((root child) body-window body-group)
  (let ((rec (gensym))
	(sub-child (gensym)))
    `(labels ((,rec (,child)
		(typecase ,child
		  (xlib:window ,body-window)
		  (group ,body-group
			 (dolist (,sub-child (group-child ,child))
			   (,rec ,sub-child))))))
       (,rec ,root))))



(defun group-find-free-number ()
  (let ((all-numbers nil))
    (with-all-groups (*root-group* group)
      (push (group-number group) all-numbers))
    (find-free-number all-numbers)))


(defun create-group (&rest args &key (number (group-find-free-number)) &allow-other-keys)
  (let* ((window (xlib:create-window :parent *root*
				     :x 0
				     :y 0
				     :width 200
				     :height 200
				     :background (get-color "Black")
				     :colormap (xlib:screen-default-colormap *screen*)
				     :border-width 1
				     :border (get-color "Red")
				     :event-mask '(:exposure :button-press :button-release :pointer-motion)))
	 (gc (xlib:create-gcontext :drawable window
				   :foreground (get-color "Green")
				   :background (get-color "Black")
				   :font *default-font*
				   :line-style :solid)))
    (apply #'make-instance 'group :number number :window window :gc gc args)))



(defun add-group (group father)
  (push group (group-child father)))






(defun get-current-child ()
  "Return the current focused child"
  (unless (equal *current-child* *root-group*)
    (typecase *current-child*
      (xlib:window *current-child*)
      (group (if (xlib:window-p (first (group-child *current-child*)))
		 (first (group-child *current-child*))
		 *current-child*)))))


(defun find-child (to-find root)
  "Find to-find in root or in its childs"
  (with-all-childs (root child)
    (when (equal child to-find)
      (return-from find-child t))))



(defun find-father-group (to-find &optional (root *root-group*))
  "Return the father group of to-find"
  (with-all-groups (root group)
    (when (member to-find (group-child group))
      (return-from find-father-group group))))

  

(defun find-group-window (window &optional (root *root-group*))
  "Return the group with the window window"
  (with-all-groups (root group)
    (when (xlib:window-equal window (group-window group))
      (return-from find-group-window group))))


(defun find-group-by-name (name)
  "Find a group from its name"
  (when name
    (with-all-groups (*root-group* group)
      (when (string-equal name (group-name group))
	(return-from find-group-by-name group)))))

(defun find-group-by-number (number)
  "Find a group from its number"
  (when (numberp number)
    (with-all-groups (*root-group* group)
      (when (= number (group-number group))
	(return-from find-group-by-number group)))))




(defun get-all-windows (&optional (root *root-group*))
  "Return all windows in root and in its childs"
  (let ((acc nil))
    (with-all-windows (root window)
      (push window acc))
    acc))


(defun get-hidden-windows ()
  "Return all hiddens windows"
  (let ((all-windows (get-all-windows))
	(hidden-windows (remove-if-not #'window-hidden-p
				       (copy-list (xlib:query-tree *root*)))))
    (set-difference hidden-windows all-windows)))




(defun display-group-info (group)
  (let ((dy (+ (xlib:max-char-ascent *default-font*) (xlib:max-char-descent *default-font*))))
    (with-slots (name number gc window child) group
      (when (equal group *current-root*)
	(xlib:clear-area window))
      (setf (xlib:gcontext-foreground gc) (get-color (if (and (equal group *current-root*)
							      (equal group *current-child*))
							 "Red" "Green")))
      (xlib:draw-image-glyphs window gc 5 dy		 
			      (format nil "Group: ~A~A                                                  "
				      number
				      (if name  (format nil " - ~A" name) "")))
      (let ((pos dy))
	(when (equal group *current-root*)
	  (xlib:draw-image-glyphs window gc 5 (incf pos dy)
				  (format nil "~A hidden windows             " (length (get-hidden-windows))))
	  (when *child-selection*
	    (xlib:draw-image-glyphs window gc 5 (incf pos dy)
				    (with-output-to-string (str)
				      (format str "Selection: ")
				      (dolist (child *child-selection*)
					(typecase child
					  (xlib:window (format str "~A " (xlib:wm-name child)))
					  (group (format str "group:~A[~A] " (group-number child)
							 (aif (group-name child) it "")))))
				      (format str "                                                   ")))))
	(dolist (ch child)
	  (when (xlib:window-p ch)
	    (xlib:draw-glyphs window gc 5 (incf pos dy) (ensure-printable (xlib:wm-name ch)))))))))










(defun get-father-layout (child father)
  (if (group-p father)
      (aif (group-layout father)
	   (funcall it child father)
	   (no-layout child father))
      (get-fullscreen-size)))


(defgeneric adapt-child-to-father (child father))

(defmethod adapt-child-to-father ((window xlib:window) father)
  (with-xlib-protect
      (multiple-value-bind (nx ny nw nh raise-p)
	  (get-father-layout window father)
	(setf (xlib:drawable-x window) nx
	      (xlib:drawable-y window) ny
	      (xlib:drawable-width window) nw
	      (xlib:drawable-height window) nh)
	raise-p)))

(defmethod adapt-child-to-father ((group group) father)
  (with-xlib-protect
      (multiple-value-bind (nx ny nw nh raise-p)
	  (get-father-layout group father)
	(with-slots (rx ry rw rh window) group
	  (setf rx nx  ry ny  rw nw  rh nh)
	  (setf (xlib:drawable-x window) rx
		(xlib:drawable-y window) ry
		(xlib:drawable-width window) rw
		(xlib:drawable-height window) rh)
	  raise-p))))
   
  

(defgeneric show-child (child father))
(defgeneric hide-child (child))

(defmethod show-child ((group group) father)
  (with-xlib-protect
      (with-slots (window) group
	(let ((raise-p (adapt-child-to-father group father)))
	  (when (or *show-root-group-p* (not (equal group *current-root*)))
	    (setf (xlib:window-background window) (get-color "Black"))
	    (xlib:map-window window)
	    (when raise-p
	      (raise-window window))
	    (display-group-info group))))))


(defmethod hide-child ((group group))
  (with-xlib-protect
      (with-slots (window) group
	(xlib:unmap-window window))))


(defmethod show-child ((window xlib:window) father)
  (with-xlib-protect
      (let ((raise-p nil))
	(when (eql (window-type window) :normal)
	  (setf raise-p (adapt-child-to-father window father)))
	(xlib:map-window window)
	(when raise-p
	  (raise-window window)))))

(defmethod hide-child ((window xlib:window))
  (hide-window window))






(defgeneric select-child (child selected))

(defmethod select-child ((group group) selected)
  (with-xlib-protect
      (when (and (group-p group) (group-window group))
	(setf (xlib:window-border (group-window group))
	      (get-color (cond ((equal selected :maybe) *color-maybe-selected*)
			       ((equal selected nil) *color-unselected*)
			       (selected *color-selected*)))))))

(defmethod select-child ((window xlib:window) selected)
  (with-xlib-protect
      (setf (xlib:window-border window)
	    (get-color (cond ((equal selected :maybe) *color-maybe-selected*)
			     ((equal selected nil) *color-unselected*)
			     (selected *color-selected*))))))

(defun select-current-group (selected)
  (select-child *current-child* selected))



(defun set-focus-to-current-child ()
  (labels ((rec (child)
	     (typecase child
	       (xlib:window (focus-window child))
	       (group (rec (first (group-child child)))))))
    (no-focus)
    (rec *current-child*)))





(defun show-all-childs ()
  "Show all childs from *current-root*"
  (labels ((rec (root father first-p)
	     (show-child root father)
	     (select-child root (if (equal root *current-child*) t
				    (if first-p :maybe nil)))
	     (when (group-p root)
	       (let ((first-child (first (group-child root))))
		 (dolist (child (reverse (group-child root)))
		   (rec child root (and first-p (equal child first-child))))))))
    (rec *current-root* nil t)
    (set-focus-to-current-child)))




(defun hide-all-childs (root)
  (hide-child root)
  (when (group-p root)
    (dolist (child (group-child root))
      (hide-all-childs child))))




(defun select-next/previous-brother (fun-rotate)
  "Select the next/previous brother group"
  (let ((group-is-root? (and (equal *current-root* *current-child*)
			     (not (equal *current-root* *root-group*)))))
    (if group-is-root?
	(hide-all-childs *current-root*)
	(select-current-group nil))
    (let ((father (find-father-group *current-child*)))
      (when (group-p father)
	(with-slots (child) father
	  (setf child (funcall fun-rotate child))
	  (setf *current-child* (first child)))))
    (when group-is-root?
      (setf *current-root* *current-child*))
    (show-all-childs)))


(defun select-next-brother ()
  "Select the next brother group"
  (select-next/previous-brother #'anti-rotate-list))

(defun select-previous-brother ()
  "Select the previous brother group"
  (select-next/previous-brother #'rotate-list))


(defun select-next-level ()
  "Select the next level in group"
  (select-current-group nil)
  (when (group-p *current-child*)
    (awhen (first (group-child *current-child*))
	   (setf *current-child* it)))
  (show-all-childs))

(defun select-previous-level ()
  "Select the previous level in group"
  (unless (equal *current-child* *current-root*)
    (select-current-group nil)
    (awhen (find-father-group *current-child*)
	   (setf *current-child* it))
    (show-all-childs)))



(defun select-next/previous-child (fun-rotate)
  "Select the next/previous child"
  (when (group-p *current-child*)
    (with-slots (child) *current-child*
      (setf child (funcall fun-rotate child)))
    (show-all-childs)))


(defun select-next-child ()
  "Select the next child"
  (select-next/previous-child #'anti-rotate-list))

(defun select-previous-child ()
  "Select the previous child"
  (select-next/previous-child #'rotate-list))



(defun enter-group ()
  "Enter in the selected group - ie make it the root group"
  (hide-all-childs *current-root*)
  (setf *current-root* *current-child*)
  (show-all-childs))

(defun leave-group ()
  "Leave the selected group - ie make its father the root group"
  (hide-all-childs *current-root*)
  (awhen (find-father-group *current-root*)
	 (when (group-p it)
	   (setf *current-root* it)))
  (show-all-childs))


(defun switch-to-root-group ()
  "Switch to the root group"
  (hide-all-childs *current-root*)
  (setf *current-root* *root-group*)
  (show-all-childs))

(defun switch-and-select-root-group ()
  "Switch and select the root group"
  (hide-all-childs *current-root*)
  (setf *current-root* *root-group*)
  (setf *current-child* *current-root*)
  (show-all-childs))


(defun toggle-show-root-group ()
  "Show/Hide the root group"
  (hide-all-childs *current-root*)
  (setf *show-root-group-p* (not *show-root-group-p*))
  (show-all-childs))


(defun focus-child (child father)
  "Focus child - Return true if something has change"
  (when (and (group-p father)
	     (member child (group-child father)))
    (when (not (equal child (first (group-child father))))
      (loop until (equal child (first (group-child father)))
	 do (setf (group-child father) (rotate-list (group-child father))))
      t)))

(defun focus-child-rec (child father)
  "Focus child and its fathers - Return true if something has change"
  (let ((change nil))
    (labels ((rec (child father)
	       (when (focus-child child father)
		 (setf change t))
	       (when father
		 (rec father (find-father-group father)))))
      (rec child father))
    change))

(defun set-current-child (child father)
  "Set *current-child* to child - Return t if something has change"
  (cond ((and (group-p child) (not (equal *current-child* child)))
	 (setf *current-child* child)
	 t)
	((and (group-p father) (not (equal *current-child* father)))
	 (setf *current-child* father)
	 t)))

(defun set-current-root (father)
  "Set current root if father is not in current root"
  (unless (find-child father *current-root*)
    (setf *current-root* father)))


(defun focus-all-childs (child father)
  "Focus child and its fathers - Set current group to father"
  (let ((new-focus (focus-child-rec child father))
	(new-current-child (set-current-child child father))
	(new-root (set-current-root father)))
    (or new-focus new-current-child new-root)))



(defun remove-child-in-group (child group)
  "Remove the child in group"
  (when (group-p group)
    (setf (group-child group) (remove child (group-child group) :test #'equal))))

(defun remove-child-in-groups (child root)
  "Remove child in the group root and in all its childs"
  (with-all-groups (root group)
    (remove-child-in-group child group))
  (when (xlib:window-p child)
    (netwm-remove-in-client-list child)))



(defun remove-child-in-all-groups (child)
  "Remove child in all groups from *root-group*"
  (when (equal child *current-root*)
    (setf *current-root* (find-father-group child)))
  (when (equal child *current-child*)
    (setf *current-child* *current-root*))
  (remove-child-in-groups child *root-group*))





(defun place-window-from-hints (window)
  "Place a window from its hints"
  (let* ((hints (xlib:wm-normal-hints window))
	 (min-width (or (and hints (xlib:wm-size-hints-min-width hints)) 0))
	 (min-height (or (and hints (xlib:wm-size-hints-min-height hints)) 0))
	 (max-width (or (and hints (xlib:wm-size-hints-max-width hints)) (xlib:drawable-width *root*)))
	 (max-height (or (and hints (xlib:wm-size-hints-max-height hints)) (xlib:drawable-height *root*)))
	 (rwidth (or (and hints (or (xlib:wm-size-hints-width hints) (xlib:wm-size-hints-base-width hints)))
		     (xlib:drawable-width window)))
	 (rheight (or (and hints (or (xlib:wm-size-hints-height hints) (xlib:wm-size-hints-base-height hints)))
		      (xlib:drawable-height window))))
    (setf (xlib:drawable-width window) (min (max min-width rwidth) max-width)
	  (xlib:drawable-height window) (min (max min-height rheight) max-height))
    (setf (xlib:drawable-x window) (truncate (+ (group-rx *current-child*) (/ (- (group-rw *current-child*) (xlib:drawable-width window)) 2)))
	  (xlib:drawable-y window) (truncate (+ (group-ry *current-child*) (/ (- (group-rh *current-child*) (xlib:drawable-height window)) 2))))))



(defun default-group-nw-hook (window)
  (when (xlib:window-p *current-child*)
    (leave-group)
    (select-previous-level))
  ;;(unless (eql (window-type window) :maxsize) ;; PHIL: this is sufficient for the ROX panel
  (when (group-p *current-child*)
    (pushnew window (group-child *current-child*))) ;)
  ;;(dbg (xlib:wm-name window) (xlib:get-wm-class window) (window-type window)) ;;; PHIL
  (case (window-type window)
    (:normal (adapt-child-to-father window *current-child*))
    (t (place-window-from-hints window))))


(defun open-in-new-group-nw-hook (group window)
  (declare (ignore group))
  (pushnew window (group-child *current-root*))
  ;;(dbg (xlib:wm-name window) (xlib:get-wm-class window) (window-type window)) ;;; PHIL
  (case (window-type window)
    (:normal (adapt-child-to-father window *current-root*))
    (t (place-window-from-hints window)))
  (list t nil))
  


(defun do-all-groups-nw-hook (window)
  "Call nw-hook of each group. A hook must return one value or a list of two values.
If the value or the first value is true then the default nw-hook is not executed.
If the second value is true then no more group can do an action with the window (ie leave the loop)."
  (let ((result nil))
    (with-all-groups (*root-group* group)
      (let ((ret (call-hook (group-nw-hook group) (list group window))))
	(typecase ret
	  (cons (when (first ret)
		  (setf result t))
		(when (second ret)
		  (return-from do-all-groups-nw-hook result)))
	  (t (when ret
	       (setf result t))))))
    result))

(defun process-new-window (window)
  "When a new window is created (or when we are scanning initial
windows), this function dresses the window up and gets it ready to be
managed."
  (with-xlib-protect
    (setf (xlib:window-event-mask window) *window-events*)
    (set-window-state window +normal-state+)
    (setf (xlib:drawable-border-width window) (case (window-type window)
						(:normal 1)
						(:maxsize 1)
						(:transient 1)
						(t 0)))
    (grab-all-buttons window)
;;    (when (group-p *current-child*) ;; PHIL: Remove this!!!
;;      (setf (group-nw-hook *current-child*) #'open-in-new-group-nw-hook))
    (unless (do-all-groups-nw-hook window)
      (default-group-nw-hook window))
    (unhide-window window)
    (netwm-add-in-client-list window)))




;;(defun hide-existing-windows (screen)
;;  "Hide all existing windows in screen"
;;  (dolist (win (xlib:query-tree (xlib:screen-root screen)))
;;    (hide-window win)))

(defun process-existing-windows (screen)
  "Windows present when clfswm starts up must be absorbed by clfswm."
  (let ((id-list nil)
	(all-windows (get-all-windows)))
    (dolist (win (xlib:query-tree (xlib:screen-root screen)))
      (unless (member win all-windows)
	(let ((map-state (xlib:window-map-state win))
	      (wm-state (window-state win)))
	  (unless (or (eql (xlib:window-override-redirect win) :on)
		      (eql win *no-focus-window*))
	    (when (or (eql map-state :viewable)
		      (eql wm-state +iconic-state+))
	      (format t "Processing ~S: type=~A ~S~%" (xlib:wm-name win) (window-type win)win)
	      ;;	    (unhide-window win)
	      (process-new-window win)
	      (xlib:map-window win)
	      (pushnew (xlib:window-id win) id-list))))))
    (netwm-set-client-list id-list)))
