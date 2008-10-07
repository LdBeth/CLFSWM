;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Query utility
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
	   (local-history history)
	   (grab-keyboard-p (xgrab-keyboard-p))
	   (grab-pointer-p (xgrab-pointer-p)))
      (labels ((add-cursor (string)
		 (concatenate 'string (subseq string 0 pos) "|" (subseq string pos)))
	       (print-string ()
		 (clear-pixmap-buffer window gc)
		 (setf (xlib:gcontext-foreground gc) (get-color *query-foreground*))
		 (xlib:draw-glyphs *pixmap-buffer* gc 5 (+ (xlib:max-char-ascent font) 5) msg)
		 (when (< pos 0) (setf pos 0))
		 (when (> pos (length result-string)) (setf pos (length result-string)))
		 (xlib:draw-glyphs *pixmap-buffer* gc 10 (+ (* 2 (+ (xlib:max-char-ascent font) (xlib:max-char-descent font))) 5)
				   (add-cursor (query-show-paren result-string pos)))
		 (copy-pixmap-buffer window gc))
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
		 (let* ((modifiers (state->modifiers state))
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
	(unless grab-keyboard-p
	  (ungrab-main-keys)
	  (xgrab-keyboard *root*))
	(xlib:map-window window)
	(print-string)
	(wait-no-key-or-button-press)
	(unwind-protect
	     (loop until (member done '(:Return :Escape :Complet)) do
		  (xlib:display-finish-output *display*)
		  (xlib:process-event *display* :handler #'handle-query))
	  (xlib:destroy-window window)
	  (xlib:close-font font)
	  (unless grab-keyboard-p
	    (xungrab-keyboard)
	    (grab-main-keys))
	  (if grab-pointer-p
	      (xgrab-pointer *root* 66 67)
	      (xungrab-pointer))))
      (values (when (member done '(:Return :Complet))
		(push result-string history)
		result-string)
	      done))))



(defun query-number (msg &optional (default 0))
  "Query a number from the query input"
  (parse-integer (or (query-string msg (format nil "~A" default)) "") :junk-allowed t))
