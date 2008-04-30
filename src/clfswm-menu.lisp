;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Menu functions
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
(defun add-menu-key (menu-name key value)
  (add-item (make-menu-item :key key :value value) (find-menu menu-name)))

(defun add-sub-menu (menu-name key sub-menu-name &optional (doc "Sub menu"))
  (add-item (make-menu-item :key key :value (make-menu :name sub-menu-name :doc doc)) (find-menu menu-name)))


(defun del-menu-key (menu-name key)
  (del-item-by-key key (find-menu menu-name)))

(defun del-menu-value (menu-name value)
  (del-item-by-value value (find-menu menu-name)))

(defun del-sub-menu (menu-name sub-menu-name)
  (del-item-by-value (find-menu sub-menu-name) (find-menu menu-name)))




;;; Display menu functions
(defun open-menu (&optional (menu *menu*))
  "Open the main menu"
  (let ((info-list nil)
	(action nil))
    (dolist (item (menu-item menu))
      (let ((value (menu-item-value item)))
	(push (format nil "~A: ~A" (menu-item-key item) (typecase value
							  (menu (format nil "< ~A >" (menu-doc value)))
							  (t (documentation value 'function))))
	      info-list)
	(define-info-key-fun (list (menu-item-key item) 0)
	    (lambda (&optional args)
	      (declare (ignore args))
	      (setf action value)
	      (throw 'exit-info-loop nil)))))
    (info-mode (nreverse info-list))
    (dolist (item (menu-item menu))
      (undefine-info-key-fun (list (menu-item-key item) 0)))
    (typecase action
      (menu (open-menu action))
      (t (when (fboundp action)
	   (funcall action))))))

      