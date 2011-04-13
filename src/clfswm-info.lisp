;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Info function (see the end of this file for user definition
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

(defstruct info window gc font list ilw ilh x y max-x max-y)


(defparameter *info-selected-item* nil)


(defun leave-info-mode (info)
  "Leave the info mode"
  (declare (ignore info))
  (setf *info-selected-item* nil)
  (throw 'exit-info-loop nil))

(defun leave-info-mode-and-valid (info)
  "Leave the info mode and valid the selected item"
  (declare (ignore info))
  (throw 'exit-info-loop nil))

(defun mouse-leave-info-mode (window root-x root-y info)
  "Leave the info mode"
  (declare (ignore window root-x root-y info))
  (setf *info-selected-item* nil)
  (throw 'exit-info-loop nil))



(defun find-info-item-from-mouse (root-x root-y info)
  (if (< (xlib:drawable-x (info-window info)) root-x
	 (+ (xlib:drawable-x (info-window info))
	    (xlib:drawable-width (info-window info))))
      (truncate (/ (- (+ (- root-y (xlib:drawable-y (info-window info)))
			 (xlib:max-char-ascent (info-font info))
			 (info-y info)) (info-ilh info)) (info-ilh info)))
      nil))


(defun set-info-item-form-mouse (root-x root-y info)
  (setf *info-selected-item* (find-info-item-from-mouse root-x root-y info)))


(defun info-y-display-coords (info posy)
  (- (+ (* (info-ilh info) posy) (info-ilh info)) (info-y info)))

(defun incf-info-selected-item (info n)
  (setf *info-selected-item*
	(min (if *info-selected-item*
		 (+ *info-selected-item* n)
		 0)
	     (1- (or (length (info-list info)) 1)))))

(defun decf-info-selected-item (info n)
  (declare (ignore info))
  (setf *info-selected-item*
	(max (if *info-selected-item*
		 (- *info-selected-item* n)
		 0)
	     0)))



(defun draw-info-window (info)
  (labels ((print-line (line posx posy &optional (color *info-foreground*))
	     (xlib:with-gcontext ((info-gc info) :foreground (get-color color)
				  :background (if (equal posy *info-selected-item*)
						  (get-color *info-selected-background*)
						  (get-color *info-background*)))
	       (xlib:draw-image-glyphs *pixmap-buffer* (info-gc info)
				 (- (+ (info-ilw info) (* posx (info-ilw info))) (info-x info))
				 (info-y-display-coords info posy)
				 (format nil "~A" line)))
	     (+ posx (length line))))
    (clear-pixmap-buffer (info-window info) (info-gc info))
    (loop for line in (info-list info)
       for y from 0
       do (typecase line
	    (cons (typecase (first line)
		    (cons (let ((posx 0))
			    (dolist (l line)
			      (typecase l
				(cons (setf posx (print-line (first l) posx y (second l))))
				(t (setf posx (print-line l posx y)))))))
		    (t (print-line (first line) 0 y (second line)))))
	    (t (print-line line 0 y))))
    (copy-pixmap-buffer (info-window info) (info-gc info))))





;;;,-----
;;;| Key binding
;;;`-----

(add-hook *binding-hook* 'init-*info-keys* 'init-*info-mouse*)

(defun set-default-info-keys ()
  (define-info-key (#\q) 'leave-info-mode)
  (define-info-key ("Return") 'leave-info-mode-and-valid)
  (define-info-key ("space") 'leave-info-mode-and-valid)
  (define-info-key ("Escape") 'leave-info-mode)
  (define-info-key ("g" :control) 'leave-info-mode)
  (define-info-key ("twosuperior")
      (defun info-banish-pointer (info)
	"Move the pointer to the lower right corner of the screen"
	(declare (ignore info))
	(banish-pointer)))
  (define-info-key ("Down")
      (defun info-next-line (info)
	"Move one line down"
	(incf-info-selected-item info 1)
	(when (> (info-y-display-coords info *info-selected-item*)
		 (+ (xlib:drawable-y (info-window info))
		    (xlib:drawable-height (info-window info))))
	  (setf (info-y info) (min (+ (info-y info) (info-ilh info)) (info-max-y info))))
	(draw-info-window info)))
  (define-info-key ("Up")
      (defun info-previous-line (info)
	"Move one line up"
	(decf-info-selected-item info 1)
	(when (< (info-y-display-coords info *info-selected-item*)
		 (+ (xlib:drawable-y (info-window info))
		    (info-ilh info)))
	  (setf (info-y info) (max (- (info-y info) (info-ilh info)) 0)))
	(draw-info-window info)))
  (define-info-key ("Left")
      (defun info-previous-char (info)
	"Move one char left"
	(setf (info-x info) (max (- (info-x info) (info-ilw info)) 0))
	(draw-info-window info)))
  (define-info-key ("Right")
      (defun info-next-char (info)
	"Move one char right"
	(setf (info-x info) (min (+ (info-x info) (info-ilw info)) (info-max-x info)))
	(draw-info-window info)))
  (define-info-key ("Home")
      (defun info-first-line (info)
	"Move to first line"
	(setf (info-x info) 0
	      (info-y info) 0)
	(setf *info-selected-item* 0)
	(draw-info-window info)))
  (define-info-key ("End")
      (defun info-end-line (info)
	"Move to last line"
	(setf (info-x info) 0
	      (info-y info) (- (* (length (info-list info)) (info-ilh info)) (xlib:drawable-height (info-window info))))
	(setf *info-selected-item* (1- (or (length (info-list info)) 1)))
	(draw-info-window info)))
  (define-info-key ("Page_Down")
      (defun info-next-ten-lines (info)
	"Move ten lines down"
	(incf-info-selected-item info 10)
	(when (> (info-y-display-coords info *info-selected-item*)
		 (+ (xlib:drawable-y (info-window info))
		    (xlib:drawable-height (info-window info))))
	  (setf (info-y info) (min (+ (info-y info) (* (info-ilh info) 10)) (info-max-y info))))
	(draw-info-window info)))
  (define-info-key ("Page_Up")
      (defun info-previous-ten-lines (info)
	"Move ten lines up"
	(decf-info-selected-item info 10)
	(when (< (info-y-display-coords info *info-selected-item*)
		 (+ (xlib:drawable-y (info-window info))
		    (info-ilh info)))
	  (setf (info-y info) (max (- (info-y info) (* (info-ilh info) 10)) 0)))
	(draw-info-window info))))

(add-hook *binding-hook* 'set-default-info-keys)




(defparameter *info-start-grab-x* nil)
(defparameter *info-start-grab-y* nil)


(defun info-begin-grab (window root-x root-y info)
  "Begin grab text"
  (declare (ignore window))
  (setf *info-start-grab-x* (min (max (+ root-x (info-x info)) 0) (info-max-x info))
	*info-start-grab-y* (min (max (+ root-y (info-y info)) 0) (info-max-y info)))
  (draw-info-window info))

(defun info-end-grab (window root-x root-y info)
  "End grab"
  (declare (ignore window))
  (setf (info-x info) (min (max (- *info-start-grab-x* root-x) 0) (info-max-x info))
	(info-y info) (min (max (- *info-start-grab-y* root-y) 0) (info-max-y info))
	*info-start-grab-x* nil
	*info-start-grab-y* nil)
  (draw-info-window info))

(defun info-mouse-next-line (window root-x root-y info)
  "Move one line down"
  (declare (ignore window))
  (setf (info-y info) (min (+ (info-y info) (info-ilh info)) (info-max-y info)))
  (set-info-item-form-mouse root-x root-y info)
  (draw-info-window info))

(defun info-mouse-previous-line (window root-x root-y info)
  "Move one line up"
  (declare (ignore window))
  (setf (info-y info) (max (- (info-y info) (info-ilh info)) 0))
  (set-info-item-form-mouse root-x root-y info)
  (draw-info-window info))


(defun info-mouse-motion-drag (window root-x root-y info)
  "Grab text"
  (declare (ignore window))
  (when (and *info-start-grab-x* *info-start-grab-y*)
    (setf (info-x info) (min (max (- *info-start-grab-x* root-x) 0) (info-max-x info))
	  (info-y info) (min (max (- *info-start-grab-y* root-y) 0) (info-max-y info)))
    (draw-info-window info)))







(defun info-mouse-select-item (window root-x root-y info)
  (declare (ignore window))
  (set-info-item-form-mouse root-x root-y info)
  (leave-info-mode-and-valid info))

(defun info-mouse-motion-click (window root-x root-y info)
  (declare (ignore window))
  (let ((last *info-selected-item*))
    (set-info-item-form-mouse root-x root-y info)
    (unless (equal last *info-selected-item*)
      (draw-info-window info))))



(defun set-default-info-mouse ()
  (if *info-click-to-select*
      (define-info-mouse (1) nil 'info-mouse-select-item)
      (define-info-mouse (1) 'info-begin-grab 'info-end-grab))
  (define-info-mouse (2) 'mouse-leave-info-mode)
  (define-info-mouse (3) 'mouse-leave-info-mode)
  (define-info-mouse (4) 'info-mouse-previous-line)
  (define-info-mouse (5) 'info-mouse-next-line)
  (if *info-click-to-select*
      (define-info-mouse ('motion) 'info-mouse-motion-click nil)
      (define-info-mouse ('motion) 'info-mouse-motion-drag nil)))

(add-hook *binding-hook* 'set-default-info-mouse)



(let (info)
  (define-handler info-mode :key-press (code state)
    (funcall-key-from-code *info-keys* code state info))

  (define-handler info-mode :motion-notify (window root-x root-y)
    (unless (compress-motion-notify)
      (funcall-button-from-code *info-mouse* 'motion (modifiers->state *default-modifiers*)
				window root-x root-y *fun-press* (list info))))

  (define-handler info-mode :button-press (window root-x root-y code state)
    (funcall-button-from-code *info-mouse* code state window root-x root-y *fun-press* (list info)))

  (define-handler info-mode :button-release (window root-x root-y code state)
    (funcall-button-from-code *info-mouse* code state window root-x root-y *fun-release* (list info)))



  (defun info-mode (info-list &key (width nil) (height nil))
    "Open the info mode. Info-list is a list of info: One string per line
Or for colored output: a list (line_string color)
Or ((1_word color) (2_word color) 3_word (4_word color)...)"
    (when info-list
      (setf *info-selected-item* 0)
      (labels ((compute-size (line)
		 (typecase line
		   (cons (typecase (first line)
			   (cons (let ((val 0))
				   (dolist (l line val)
				     (incf val (typecase l
						 (cons (length (first l)))
						 (t (length l)))))))
			   (t (length (first line)))))
		   (t (length line)))))
	(let* ((font (xlib:open-font *display* *info-font-string*))
	       (ilw (xlib:max-char-width font))
	       (ilh (+ (xlib:max-char-ascent font) (xlib:max-char-descent font) 1))
	       (width (or width
			  (min (* (+ (loop for l in info-list maximize (compute-size l)) 2) ilw)
			       (xlib:screen-width *screen*))))
	       (height (or height
			   (min (round (+ (* (length info-list) ilh) (/ ilh 2)))
				(xlib:screen-height *screen*)))))
	  (with-placement (*info-mode-placement* x y width height)
	    (let* ((pointer-grabbed-p (xgrab-pointer-p))
		   (keyboard-grabbed-p (xgrab-keyboard-p))
		   (window (xlib:create-window :parent *root*
					       :x x :y y
					       :width width
					       :height height
					       :background (get-color *info-background*)
					       :colormap (xlib:screen-default-colormap *screen*)
					       :border-width *border-size*
					       :border (get-color *info-border*)
					       :event-mask '(:exposure)))
		   (gc (xlib:create-gcontext :drawable window
					     :foreground (get-color *info-foreground*)
					     :background (get-color *info-background*)
					     :font font
					     :line-style :solid)))
	      (setf info (make-info :window window :gc gc :x 0 :y 0 :list info-list
				    :font font :ilw ilw :ilh ilh
				    :max-x (* (loop for l in info-list maximize (compute-size l)) ilw)
				    :max-y (* (length info-list) ilh)))
	      (map-window window)
	      (draw-info-window info)
	      (xgrab-pointer *root* 68 69)
	      (unless keyboard-grabbed-p
		(xgrab-keyboard *root*))
	      (wait-no-key-or-button-press)
	      (generic-mode 'info-mode 'exit-info-loop
			    :loop-function (lambda ()
					     (raise-window (info-window info)))
			    :original-mode '(main-mode))
	      (if pointer-grabbed-p
		  (xgrab-pointer *root* 66 67)
		  (xungrab-pointer))
	      (unless keyboard-grabbed-p
		(xungrab-keyboard))
	      (xlib:free-gcontext gc)
	      (xlib:destroy-window window)
	      (xlib:close-font font)
	      (xlib:display-finish-output *display*)
	      (display-all-frame-info)
	      (wait-no-key-or-button-press)
	      *info-selected-item*)))))))



(defun info-mode-menu (item-list &key (width nil) (height nil))
  "Open an info help menu.
Item-list is: '((key function) separator (key function))
or with explicit docstring: '((key function \"documentation 1\") (key function \"bla bla\") (key function))
key is a character, a keycode or a keysym
Separator is a string or a symbol (all but a list)
Function can be a function or a list (function color) for colored output"
  (let ((info-list nil)
	(action nil)
        (old-info-keys (copy-hash-table *info-keys*)))
    (labels ((define-key (key function)
	       (define-info-key-fun (list key)
		   (lambda (&optional args)
		     (declare (ignore args))
		     (setf action function)
		     (leave-info-mode nil)))))
      (dolist (item item-list)
	(typecase item
	  (cons (destructuring-bind (key function explicit-doc) (ensure-n-elems item 3)
		  (typecase function
		    (cons (push (list (list (format nil "~A" key) *menu-color-menu-key*)
				      (list (format nil ": ~A" (or explicit-doc (documentation (first function) 'function)))
					    (second function)))
				info-list)
			  (define-key key (first function)))
		    (t (push (list (list (format nil "~A" key) *menu-color-key*)
				   (format nil ": ~A" (or explicit-doc (documentation function 'function))))
			     info-list)
		       (define-key key function)))))
	  (t (push (list (format nil "-=- ~A -=-" item) *menu-color-comment*) info-list))))
      (let ((selected-item (info-mode (nreverse info-list) :width width :height height)))
        (setf *info-keys* old-info-keys)
	(when selected-item
	  (awhen (nth selected-item item-list)
	    (when (consp it)
	      (destructuring-bind (key function explicit-doc) (ensure-n-elems it 3)
		(declare (ignore key explicit-doc))
		(typecase function
		  (cons (setf action (first function)))
		  (t (setf action function)))))))
	(typecase action
	  (function (funcall action))
	  (symbol (when (fboundp action)
		    (funcall action))))))))





(defun keys-from-list (list)
  "Produce a key menu based on list item"
  (loop for l in list
     for i from 0
     collect (list (number->char i) l)))


;;;,-----
;;;| CONFIG - Info mode functions
;;;`-----
(defun key-binding-colorize-line (list)
  (loop :for line :in list
     :collect (cond ((search "* CLFSWM Keys *" line) (list line *info-color-title*))
		    ((search "---" line) (list line *info-color-underline*))
		    ((begin-with-2-spaces line)
		     (list (list (subseq line 0 22) *info-color-second*)
			   (list (subseq line 22 35) *info-color-first*)
			   (subseq line 35)))
		    (t line))))


(defun show-key-binding (&rest hash-table-key)
  "Show the binding of each hash-table-key.
Pass the :no-producing-doc symbol to remove the producing doc"
  (info-mode (key-binding-colorize-line
	      (split-string (append-newline-space
			     (with-output-to-string (stream)
			       (produce-doc (remove :no-producing-doc hash-table-key)
					    stream
					    (not (member :no-producing-doc hash-table-key)))))
			    #\Newline))))


(defun show-global-key-binding ()
  "Show all key binding"
  (show-key-binding *main-keys* *main-mouse* *second-keys* *second-mouse*
		    *info-keys* *info-mouse*))

(defun show-main-mode-key-binding ()
  "Show the main mode binding"
  (show-key-binding *main-keys* *main-mouse*))

(defun show-second-mode-key-binding ()
  "Show the second mode key binding"
  (show-key-binding *second-keys* *second-mouse*))

(defun show-circulate-mode-key-binding ()
  "Show the circulate mode key binding"
  (show-key-binding *circulate-keys*))

(defun show-expose-window-mode-key-binding ()
  "Show the expose window mode key binding"
  (show-key-binding *expose-keys* *expose-mouse*))


(defun show-first-aid-kit ()
  "Show the first aid kit key binding"
  (labels ((add-key (hash symbol &optional (hashkey *main-keys*))
	     (multiple-value-bind (k v)
		 (find-in-hash symbol hashkey)
	       (setf (gethash k hash) v))))
    (let ((hash (make-hash-table :test #'equal))
	  (hash-second (make-hash-table :test #'equal)))
      (setf (gethash 'name hash) "First aid kit - Main mode key binding"
	    (gethash 'name hash-second) "First aid kit - Second mode key binding")
      (add-key hash 'select-next-child)
      (add-key hash 'select-previous-child)
      (add-key hash 'select-next-brother)
      (add-key hash 'select-previous-brother)
      (add-key hash 'select-previous-level)
      (add-key hash 'select-next-level)
      (add-key hash 'enter-frame)
      (add-key hash 'leave-frame)
      (add-key hash 'second-key-mode)
      (add-key hash 'expose-windows-mode)
      (add-key hash 'expose-all-windows-mode)
      (add-key hash 'present-clfswm-terminal)
      (add-key hash-second 'leave-second-mode *second-keys*)
      (add-key hash-second 'open-menu *second-keys*)
      (add-key hash-second 'run-program-from-query-string *second-keys*)
      (add-key hash-second 'eval-from-query-string *second-keys*)
      (add-key hash-second 'set-open-in-new-frame-in-parent-frame-nw-hook *second-keys*)
      (add-key hash-second 'b-start-xterm *second-keys*)
      (add-key hash-second 'b-start-emacs *second-keys*)
      (show-key-binding hash hash-second :no-producing-doc))))


(defun corner-help-colorize-line (list)
  (loop :for line :in list
     :collect (cond ((search "CLFSWM:" line) (list line *info-color-title*))
		    ((search "*:" line) (list line *info-color-underline*))
		    ((begin-with-2-spaces line)
		     (let ((pos (position #\: line)))
		       (if pos
			   (list (list (subseq line 0 (1+ pos)) *info-color-first*)
				 (subseq line (1+ pos)))
			   line)))
		    (t line))))

(defun show-corner-help ()
  "Help on clfswm corner"
  (info-mode (corner-help-colorize-line
	      (split-string (append-newline-space
			     (with-output-to-string (stream)
			       (produce-corner-doc stream)))
			    #\Newline))))


(defun configuration-variable-colorize-line (list)
  (loop :for line :in list
     :collect (cond ((search "CLFSWM " line) (list line *info-color-title*))
		    ((search "* =" line)
		     (let ((pos (position #\= line)))
		       (list (list (subseq line 0 (1+ pos)) *info-color-first*)
			     (list (subseq line (1+ pos)) *info-color-second*))))
		    ((search "<=" line) (list line *info-color-underline*))
		    (t line))))


(defun show-config-variable ()
  "Show all configurable variables"
  (let ((result nil))
    (labels ((rec ()
	       (setf result nil)
	       (info-mode-menu (loop :for group :in (config-all-groups)
				  :for i :from 0
				  :collect (list (number->char i)
						 (let ((group group))
						   (lambda ()
						     (setf result group)))
						 (config-group->string group))))
	       (when result
		 (info-mode (configuration-variable-colorize-line
			     (split-string (append-newline-space
					    (with-output-to-string (stream)
					      (produce-conf-var-doc stream result t nil)))
					   #\Newline)))
		 (rec))))
      (rec))))




(defun show-date ()
  "Show the current time and date"
  (info-mode (list (list `("Current date:" ,*menu-color-comment*) (date-string)))))






(defun info-on-shell (msg program)
  (let ((lines (do-shell program nil t)))
    (info-mode (append (list (list msg *menu-color-comment*))
		       (loop for line = (read-line lines nil nil)
			  while line
			  collect line)))))



(defun show-cpu-proc ()
  "Show current processes sorted by CPU usage"
  (info-on-shell "Current processes sorted by CPU usage:"
		 "ps --cols=1000 --sort='-%cpu,uid,pgid,ppid,pid' -e -o user,pid,stime,pcpu,pmem,args"))

(defun show-mem-proc ()
  "Show current processes sorted by memory usage"
  (info-on-shell "Current processes sorted by MEMORY usage:"
		 "ps --cols=1000 --sort='-vsz,uid,pgid,ppid,pid' -e -o user,pid,stime,pcpu,pmem,args"))


(defun show-cd-info ()
  "Show the current CD track"
  (info-on-shell "Current CD track:" "pcd i"))

(defun show-cd-playlist ()
  "Show the current CD playlist"
  (info-on-shell "Current CD playlist:" "pcd mi"))


(defun show-version ()
  "Show the current CLFSWM version"
  (info-mode (list *version*)))






