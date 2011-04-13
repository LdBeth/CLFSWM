;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Menu functions
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


(defmacro with-all-menu ((menu item) &body body)
  (let ((rec (gensym))
	(subm (gensym)))
    `(labels ((,rec (,item)
		,@body
		(when (menu-p ,item)
		  (dolist (,subm (menu-item ,item))
		    (,rec ,subm)))
		(when (and (menu-item-p ,item) (menu-p (menu-item-value ,item)))
		  (,rec (menu-item-value ,item)))))
       (,rec ,menu))))

(defun add-item (item &optional (menu *menu*))
  (setf (menu-item menu) (nconc (menu-item menu) (list item))))

(defun del-item (item &optional (menu *menu*))
  (setf (menu-item menu) (remove item (menu-item menu))))



;;; Finding functions
(defun find-menu (name  &optional (root *menu*))
  (with-all-menu (root item)
    (when (and (menu-p item)
	       (equal name (menu-name item)))
      (return-from find-menu item))))

(defun find-toplevel-menu (name &optional (root *menu*))
  (when (menu-p root)
    (dolist (item (menu-item root))
      (when (and (menu-item-p item)
		 (menu-p (menu-item-value item)))
	(when (equal name (menu-name (menu-item-value item)))
	  (return (menu-item-value item)))))))


(defun find-item-by-key (key &optional (root *menu*))
  (with-all-menu (root item)
    (when (and (menu-item-p item)
	       (equal (menu-item-key item) key))
      (return-from find-item-by-key item))))

(defun find-item-by-value (value &optional (root *menu*))
  (with-all-menu (root item)
    (when (and (menu-item-p item)
	       (equal (menu-item-value item) value))
      (return-from find-item-by-value item))))


(defun del-item-by-key (key &optional (menu *menu*))
  (del-item (find-item-by-key key menu) menu))

(defun del-item-by-value (value &optional (menu *menu*))
  (del-item (find-item-by-value value menu) menu))



;;; Convenient functions
(defun find-next-menu-key (key menu)
  "key is :next for the next free key in menu or a string"
  (if (eql key :next)
      (string (number->char (length (menu-item menu))))
      key))


(defun add-menu-key (menu-name key value &optional (root *menu*))
  (let ((menu (find-menu menu-name root)))
    (add-item (make-menu-item :key (find-next-menu-key key menu) :value value) (find-menu menu-name root))))

(defun add-sub-menu (menu-or-name key sub-menu-name &optional (doc "Sub menu") (root *menu*))
  (let ((menu (if (or (stringp menu-or-name) (symbolp menu-or-name))
		  (find-menu menu-or-name root)
		  menu-or-name))
	(submenu (make-menu :name sub-menu-name :doc doc)))
    (add-item (make-menu-item :key (find-next-menu-key key menu) :value submenu) menu)
    submenu))



(defun del-menu-key (menu-name key &optional (root *menu*))
  (del-item-by-key key (find-menu menu-name root)))

(defun del-menu-value (menu-name value &optional (root *menu*))
  (del-item-by-value value (find-menu menu-name root)))

(defun del-sub-menu (menu-name sub-menu-name &optional (root *menu*))
  (del-item-by-value (find-menu sub-menu-name) (find-menu menu-name root)))

(defun clear-sub-menu (menu-name sub-menu-name &optional (root *menu*))
  (setf (menu-item (find-menu sub-menu-name (find-menu menu-name root))) nil))


(defun add-menu-comment (menu-name &optional (comment "---") (root *menu*))
  (add-item (make-menu-item :key nil :value comment) (find-menu menu-name root)))



(defun init-menu ()
  (setf *menu* (make-menu :name 'main :doc "Main menu")))


;;; Display menu functions
(defun open-menu-do-action (action menu parent)
  (typecase action
    (menu (open-menu action (cons menu parent)))
    (null (awhen (first parent)
	    (open-menu it (rest parent))))
    (t (when (fboundp action)
	 (funcall action)))))


(defun open-menu (&optional (menu *menu*) (parent nil))
  "Open the main menu"
  (let ((action nil)
        (old-info-keys (copy-hash-table *info-keys*)))
    (labels ((populate-menu ()
	       (let ((info-list nil))
		 (dolist (item (menu-item menu))
		   (let ((value (menu-item-value item)))
		     (push (typecase value
			     (menu (list (list (format nil "~A" (menu-item-key item)) *menu-color-menu-key*)
					 (list (format nil ": < ~A >" (menu-doc value)) *menu-color-submenu*)))
			     (string (list (list (format nil "~A" (menu-item-value item)) *menu-color-comment*)))
			     (t (list (list (format nil "~A" (menu-item-key item)) *menu-color-key*)
				      (format nil ": ~A" (documentation value 'function)))))
			   info-list)
		     (when (menu-item-key item)
		       (define-info-key-fun (list (menu-item-key item))
			   (lambda (&optional args)
			     (declare (ignore args))
			     (setf action value)
			     (leave-info-mode nil))))))
		 (nreverse info-list))))
      (let ((selected-item (info-mode (populate-menu))))
        (setf *info-keys* old-info-keys)
	(when selected-item
	  (awhen (nth selected-item (menu-item menu))
	    (setf action (menu-item-value it)))))
      (open-menu-do-action action menu parent))))


