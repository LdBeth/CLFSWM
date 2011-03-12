;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Query utility
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


(defparameter *query-window* nil)
(defparameter *query-font* nil)
(defparameter *query-gc* nil)

(defparameter *query-history* nil)
(defparameter *query-complet-list* nil)

(defparameter *query-message* nil)
(defparameter *query-string* nil)
(defparameter *query-pos* nil)
(defparameter *query-return* nil)



(defun query-show-paren (orig-string pos dec)
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
		  (when (= level 0) (return p))))
	     (draw-bloc (p &optional (color *query-parent-color*))
	       (setf (xlib:gcontext-foreground *query-gc*) (get-color color))
	       (xlib:draw-rectangle *pixmap-buffer* *query-gc*
				    (+ 10 (* p (xlib:max-char-width *query-font*)) dec)
				    (+ (xlib:max-char-ascent *query-font*) (xlib:max-char-descent *query-font*) 7)
				    (xlib:max-char-width *query-font*)
				    (+ (xlib:max-char-ascent *query-font*) (xlib:max-char-descent *query-font*))
				    t)))
      (cond ((have-to-find-left?) (let ((p (pos-left)))
				    (if p
					(progn (draw-bloc p) (draw-bloc (1- pos)))
					(draw-bloc (1- pos) *query-parent-error-color*))))
	    ((have-to-find-right?) (let ((p (pos-right)))
				     (if p
					 (progn (draw-bloc p) (draw-bloc pos))
					 (draw-bloc pos *query-parent-error-color*))))))))


(defun clear-query-history ()
  "Clear the query-string history"
  (setf *query-history* nil))



(defun leave-query-mode (&optional (return :Escape))
  "Leave the query mode"
  (setf *query-return* return)
  (throw 'exit-query-loop nil))


(defun leave-query-mode-valid ()
  (leave-query-mode :Return))

(add-hook *binding-hook* 'init-*query-keys*)


(defun query-find-complet-list ()
  (remove-if-not (lambda (x)
		   (zerop (or (search *query-string* x :test #'string-equal) -1)))
		 *query-complet-list*))


(defun query-print-string ()
  (let ((dec (min 0 (- (- (xlib:drawable-width *query-window*) 10)
		       (+ 10 (* *query-pos* (xlib:max-char-width *query-font*)))))))
    (clear-pixmap-buffer *query-window* *query-gc*)
    (setf (xlib:gcontext-foreground *query-gc*) (get-color *query-message-color*))
    (xlib:draw-glyphs *pixmap-buffer* *query-gc* 5 (+ (xlib:max-char-ascent *query-font*) 5)
		      (format nil "~A ~{~A~^, ~}" *query-message*
			      (query-find-complet-list)))
    (when (< *query-pos* 0)
      (setf *query-pos* 0))
    (when (> *query-pos* (length *query-string*))
      (setf *query-pos* (length *query-string*)))
    (query-show-paren *query-string* *query-pos* dec)
    (setf (xlib:gcontext-foreground *query-gc*) (get-color *query-foreground*))
    (xlib:draw-glyphs *pixmap-buffer* *query-gc*
		      (+ 10 dec)
		      (+ (* 2 (+ (xlib:max-char-ascent *query-font*) (xlib:max-char-descent *query-font*))) 5)
		      *query-string*)
    (setf (xlib:gcontext-foreground *query-gc*) (get-color *query-cursor-color*))
    (xlib:draw-line *pixmap-buffer* *query-gc*
		    (+ 10 (* *query-pos* (xlib:max-char-width *query-font*)) dec)
		    (+ (* 2 (+ (xlib:max-char-ascent *query-font*) (xlib:max-char-descent *query-font*))) 6)
		    (+ 10 (* *query-pos* (xlib:max-char-width *query-font*)) dec)
		    (+ (* 1 (+ (xlib:max-char-ascent *query-font*) (xlib:max-char-descent *query-font*))) 7))
    (copy-pixmap-buffer *query-window* *query-gc*)))



(defun query-enter-function ()
  (setf *query-font* (xlib:open-font *display* *query-font-string*))
  (let ((width (- (xlib:screen-width *screen*) 2))
	(height (* 3 (+ (xlib:max-char-ascent *query-font*) (xlib:max-char-descent *query-font*)))))
    (with-placement (*query-mode-placement* x y width height)
      (setf *query-window* (xlib:create-window :parent *root*
					       :x x :y y
					       :width width
					       :height height
					       :background (get-color *query-background*)
					       :border-width *border-size*
					       :border (get-color *query-border*)
					       :colormap (xlib:screen-default-colormap *screen*)
					       :event-mask '(:exposure :key-press))
	    *query-gc* (xlib:create-gcontext :drawable *query-window*
					     :foreground (get-color *query-foreground*)
					     :background (get-color *query-background*)
					     :font *query-font*
					     :line-style :solid))
      (map-window *query-window*)
      (query-print-string)
      (wait-no-key-or-button-press))))



(defun query-leave-function ()
  (xlib:destroy-window *query-window*)
  (xlib:close-font *query-font*)
  (wait-no-key-or-button-press))

(defun query-loop-function ()
  (raise-window *query-window*))



(labels ((generic-backspace (del-pos)
	   (when (>= del-pos 0)
	     (setf *query-string* (concatenate 'string
					       (subseq *query-string* 0 del-pos)
					       (subseq *query-string* *query-pos*))
		   *query-pos* del-pos))))
  (defun query-backspace ()
    "Delete a character backward"
    (generic-backspace (1- *query-pos*)))

  (defun query-backspace-word ()
    "Delete a word backward"
    (generic-backspace (or (position #\Space *query-string* :from-end t :end *query-pos*) 0))))


(labels ((generic-delete (del-pos)
	   (when (<= del-pos (length *query-string*))
	     (setf *query-string* (concatenate 'string
					      (subseq *query-string* 0 *query-pos*)
					      (subseq *query-string* del-pos))))))
  (defun query-delete ()
    "Delete a character forward"
    (generic-delete (1+ *query-pos*)))

  (defun query-delete-word ()
    "Delete a word forward"
    (generic-delete (1+ (or (position #\Space *query-string* :start *query-pos*)
			    (1- (length *query-string*)))))))



(defun query-home ()
  "Move cursor to line begining"
  (setf *query-pos* 0))

(defun query-end ()
  "Move cursor to line end"
  (setf *query-pos* (length *query-string*)))


(defun query-left ()
  "Move cursor to left"
  (when (> *query-pos* 0)
    (setf *query-pos* (1- *query-pos*))))

(defun query-left-word ()
  "Move cursor to left word"
  (when (> *query-pos* 0)
    (setf *query-pos* (let ((p (position #\Space *query-string*
					 :end (min (1- *query-pos*) (length *query-string*))
					 :from-end t)))
			(if p p 0)))))

(defun query-right ()
  "Move cursor to right"
  (when (< *query-pos* (length *query-string*))
    (setf *query-pos* (1+ *query-pos*))))

(defun query-right-word ()
  "Move cursor to right word"
  (when (< *query-pos* (length *query-string*))
    (setf *query-pos* (let ((p (position #\Space *query-string*
					 :start (min (1+ *query-pos*) (length *query-string*)))))
			(if p p (length *query-string*))))))

(defun query-previous-history ()
  "Circulate backward in history"
  (setf	*query-string* (first *query-history*)
	*query-pos* (length *query-string*)
	*query-history* (rotate-list *query-history*)))


(defun query-next-history ()
  "Circulate forward in history"
  (setf	*query-string* (first *query-history*)
	*query-pos* (length *query-string*)
	*query-history* (anti-rotate-list *query-history*)))



(defun query-delete-eof ()
  "Delete the end of the line"
  (setf *query-string* (subseq *query-string* 0 *query-pos*)))


(defun query-mode-complet ()
  (setf *query-string* (find-common-string *query-string* (query-find-complet-list)))
  (let ((complet (query-find-complet-list)))
    (when (= (length complet) 1)
      (setf *query-string* (first complet))))
  (query-end))



(add-hook *binding-hook* 'set-default-query-keys)

(defun set-default-query-keys ()
  (define-query-key ("Return") 'leave-query-mode-valid)
  (define-query-key ("Escape") 'leave-query-mode)
  (define-query-key ("g" :control) 'leave-query-mode)
  (define-query-key ("Tab") 'query-mode-complet)
  (define-query-key ("BackSpace") 'query-backspace)
  (define-query-key ("BackSpace" :control) 'query-backspace-word)
  (define-query-key ("Delete") 'query-delete)
  (define-query-key ("Delete" :control) 'query-delete-word)
  (define-query-key ("Home") 'query-home)
  (define-query-key ("End") 'query-end)
  (define-query-key ("Left") 'query-left)
  (define-query-key ("Left" :control) 'query-left-word)
  (define-query-key ("Right") 'query-right)
  (define-query-key ("Right" :control) 'query-right-word)
  (define-query-key ("Up") 'query-previous-history)
  (define-query-key ("Down") 'query-next-history)
  (define-query-key ("k" :control) 'query-delete-eof))



(defun add-in-query-string (code state)
  (let* ((modifiers (state->modifiers state))
	 (keysym (keycode->keysym code modifiers))
	 (char (xlib:keysym->character *display* keysym state)))
    (when (and char (characterp char))
      (setf *query-string* (concatenate 'string
					(when (<= *query-pos* (length *query-string*))
					  (subseq *query-string* 0 *query-pos*))
					(string char)
					(when (< *query-pos* (length *query-string*))
					  (subseq *query-string* *query-pos*))))
      (incf *query-pos*))))



(define-handler query-mode :key-press (code state)
  (unless (funcall-key-from-code *query-keys* code state)
    (add-in-query-string code state))
  (query-print-string))



(defun  query-string (message &optional (default "") complet-list)
  "Query a string from the keyboard. Display msg as prompt"
  (let ((grab-keyboard-p (xgrab-keyboard-p))
	(grab-pointer-p (xgrab-pointer-p)))
    (setf *query-message* message
	  *query-string* default
	  *query-pos* (length default)
	  *query-complet-list* complet-list)
    (xgrab-pointer *root* 92 93)
    (unless grab-keyboard-p
      (ungrab-main-keys)
      (xgrab-keyboard *root*))
    (generic-mode 'query-mode 'exit-query-loop
		  :enter-function #'query-enter-function
		  :loop-function #'query-loop-function
		  :leave-function #'query-leave-function
		  :original-mode '(main-mode))
    (unless grab-keyboard-p
      (xungrab-keyboard)
      (grab-main-keys))
    (if grab-pointer-p
	(xgrab-pointer *root* 66 67)
	(xungrab-pointer)))
  (when (equal *query-return* :Return)
    (pushnew default *query-history* :test #'equal)
    (push *query-string* *query-history*))
  (values *query-string*
	  *query-return*))



(defun query-number (msg &optional (default 0))
  "Query a number from the query input"
  (parse-integer (or (query-string msg (format nil "~A" default)) "") :junk-allowed t))
