;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Keys functions definition
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


(defparameter *fun-press* #'first)
(defparameter *fun-release* #'second)



(defun with-capslock ()
  (pushnew :lock *default-modifiers*))

(defun without-capslock ()
  (setf *default-modifiers* (remove :lock *default-modifiers*)))

(defun with-numlock ()
  (pushnew :mod-2 *default-modifiers*))

(defun without-numlock ()
  (setf *default-modifiers* (remove :mod-2 *default-modifiers*)))



;;; CONFIG - Key mode names
(defmacro define-init-hash-table-key (hash-table name)
  (let ((init-name (create-symbol "init-" (format nil "~A" hash-table))))
    `(progn
      (defun ,init-name ()
	(setf ,hash-table (make-hash-table :test 'equal))
	(setf (gethash 'name ,hash-table) ,name))
      (,init-name))))

(define-init-hash-table-key *main-keys* "Main mode keys")
(define-init-hash-table-key *main-mouse* "Mouse buttons actions in main mode")
(define-init-hash-table-key *second-keys* "Second mode keys")
(define-init-hash-table-key *second-mouse* "Mouse buttons actions in second mode")
(define-init-hash-table-key *info-keys* "Info mode keys")
(define-init-hash-table-key *info-mouse* "Mouse buttons actions in info mode")
(define-init-hash-table-key *query-keys* "Query mode keys")

(define-init-hash-table-key *circulate-keys* "Circulate mode keys")
(define-init-hash-table-key *circulate-keys-release* "Circulate mode release keys")

(define-init-hash-table-key *expose-keys* "Expose windows mode keys")
(define-init-hash-table-key *expose-mouse* "Mouse buttons actions in expose windows mode")

(defun unalias-modifiers (list)
  (dolist (mod *modifier-alias*)
    (setf list (substitute (second mod) (first mod) list)))
  list)

(defun key->list (key)
  (list (first key) (modifiers->state (append (unalias-modifiers (rest key))
					      (unalias-modifiers *default-modifiers*)))))

(defmacro define-define-key (name hashtable)
  (let ((name-key-fun (create-symbol "define-" name "-key-fun"))
	(name-key (create-symbol "define-" name "-key"))
	(undefine-name-fun (create-symbol "undefine-" name "-key-fun"))
	(undefine-name (create-symbol "undefine-" name "-key"))
	(undefine-multi-name (create-symbol "undefine-" name "-multi-keys")))
    `(progn
      (defun ,name-key-fun (key function &rest args)
	"Define a new key, a key is '(char modifier1 modifier2...))"
	(setf (gethash (key->list key) ,hashtable) (list function args)))
      (defmacro ,name-key ((key &rest modifiers) function &rest args)
	`(,',name-key-fun (list ,key ,@modifiers) ,function ,@args))
      (defun ,undefine-name-fun (key)
	"Undefine a new key, a key is '(char modifier1 modifier2...))"
	(remhash (key->list key) ,hashtable))
      (defmacro ,undefine-name ((key &rest modifiers))
	`(,',undefine-name-fun (list ,key ,@modifiers)))
      (defmacro ,undefine-multi-name (&rest keys)
	`(progn
	  ,@(loop for k in keys
		  collect `(,',undefine-name ,k)))))))


(defmacro define-define-mouse (name hashtable)
  (let ((name-mouse-fun (create-symbol "define-" name "-fun"))
	(name-mouse (create-symbol "define-" name))
	(undefine-name (create-symbol "undefine-" name)))
    `(progn
       (defun ,name-mouse-fun (button function-press &optional function-release &rest args)
	 "Define a new mouse button action, a button is '(button number '(modifier list))"
	 (setf (gethash (key->list button) ,hashtable) (list function-press function-release args)))
       (defmacro ,name-mouse ((button &rest modifiers) function-press &optional function-release &rest args)
	 `(,',name-mouse-fun (list ,button ,@modifiers) ,function-press ,function-release ,@args))
       (defmacro ,undefine-name ((key &rest modifiers))
	 `(remhash (list ,key ,@modifiers) ,',hashtable)))))



(define-define-key "main" *main-keys*)
(define-define-key "second" *second-keys*)
(define-define-key "info" *info-keys*)
(define-define-key "query" *query-keys*)

(define-define-key "circulate" *circulate-keys*)
(define-define-key "circulate-release" *circulate-keys-release*)

(define-define-key "expose" *expose-keys*)

(define-define-mouse "main-mouse" *main-mouse*)
(define-define-mouse "second-mouse" *second-mouse*)
(define-define-mouse "info-mouse" *info-mouse*)
(define-define-mouse "expose-mouse" *expose-mouse*)






(defun add-in-state (state modifier)
  "Add a modifier in a state"
  (modifiers->state (append (state->modifiers state) (list modifier))))

(defmacro define-ungrab/grab (name function hashtable)
  `(defun ,name ()
     (maphash #'(lambda (k v)
		  (declare (ignore v))
		  (when (consp k)
		    (handler-case
			(let* ((key (first k))
			       (modifiers (second k))
			       (keycode (typecase key
					  (character (multiple-value-list (char->keycode key)))
					  (number key)
					  (string (let* ((keysym (keysym-name->keysym key))
							 (ret-keycode (multiple-value-list (xlib:keysym->keycodes *display* keysym))))
						    (let ((found nil))
						      (dolist (kc ret-keycode)
							(when (= keysym (xlib:keycode->keysym *display* kc 0))
							  (setf found t)))
						      (unless found
							(setf modifiers (add-in-state modifiers :shift))))
						    ret-keycode)))))
			  (if keycode
			      (if (consp keycode)
				  (dolist (kc (remove-duplicates keycode))
				    (,function *root* kc :modifiers modifiers))
				  (,function *root* keycode :modifiers modifiers))
			      (format t "~&Grabbing error: Can't find key '~A'~%" key)))
		      (error (c)
			;;(declare (ignore c))
			(format t "~&Grabbing error: Can't grab key '~A' (~A)~%" k c)))
		    (force-output)))
	      ,hashtable)))

(define-ungrab/grab grab-main-keys xlib:grab-key *main-keys*)
(define-ungrab/grab ungrab-main-keys xlib:ungrab-key *main-keys*)










(defun find-key-from-code (hash-table-key code state)
  "Return the function associated to code/state"
  (labels ((function-from (key &optional (new-state state))
	     (multiple-value-bind (function foundp)
		 (gethash (list key new-state) hash-table-key)
	       (when (and foundp (first function))
		 function)))
	   (from-code ()
	     (function-from code))
	   (from-char ()
	     (let ((char (keycode->char code state)))
	       (function-from char)))
	   (from-string ()
	     (let ((string (keysym->keysym-name (xlib:keycode->keysym *display* code 0))))
	       (function-from string)))
	   (from-string-shift ()
	     (let* ((modifiers (state->modifiers state))
		    (string (keysym->keysym-name (keycode->keysym code modifiers))))
	       (function-from string)))
	   (from-string-no-shift ()
	     (let* ((modifiers (state->modifiers state))
		    (string (keysym->keysym-name (keycode->keysym code modifiers))))
	       (function-from string (modifiers->state (remove :shift modifiers))))))
    (or (from-code) (from-char) (from-string) (from-string-shift) (from-string-no-shift))))



(defun funcall-key-from-code (hash-table-key code state &rest args)
  (let ((function (find-key-from-code hash-table-key code state)))
    (when function
      (apply (first function) (append args (second function)))
      t)))


(defun funcall-button-from-code (hash-table-key code state window root-x root-y
				 &optional (action *fun-press*) args)
  (let ((state (modifiers->state (set-difference (state->modifiers state)
						 '(:button-1 :button-2 :button-3 :button-4 :button-5)))))
    (multiple-value-bind (function foundp)
	(gethash (list code state) hash-table-key)
      (if (and foundp (funcall action function))
	  (progn
	    (apply (funcall action function) `(,window ,root-x ,root-y ,@(append args (third function))))
	    t)
	  nil))))




(defun binding-substitute-modifier (to from &optional (hashtables (list *main-keys* *main-mouse*
									*second-keys* *second-mouse*
									*info-keys* *info-mouse*
									*query-keys*
									*circulate-keys* *circulate-keys-release*
									*expose-keys* *expose-mouse*)))
  "Utility to change modifiers after binding definition"
  (labels ((change (&optional (hashtable *main-keys*) to from)
	     (maphash (lambda (k v)
			(when (consp k)
			  (let ((state (modifiers->state (substitute to from (state->modifiers (second k))))))
			    (remhash k hashtable)
			    (setf (gethash (list (first k) state) hashtable) v))))
		      hashtable)))
    (dolist (h hashtables)
      (change h to from))))
