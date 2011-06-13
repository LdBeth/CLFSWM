;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Configuration definitions and Menu generation
;;;
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

(defun find-configuration-variables ()
  (let ((all-groups nil)
	(all-variables nil))
    (maphash (lambda (key val)
               (pushnew  (configvar-group val) all-groups :test #'string-equal)
               (push (list key (configvar-group val)) all-variables))
             *config-var-table*)
    (values all-groups all-variables)))


(defun find-symbol-function (function)
  (with-all-internal-symbols (symbol :clfswm)
    (when (and (fboundp symbol) (equal (symbol-function symbol) function))
      (return-from find-symbol-function symbol))))

(defun escape-conf-value (value)
  (cond ((or (equal value t) (equal value nil))
         (format nil "~S" value))
        ((consp value)
         (format nil "(quote ~S)" value))
        ((symbolp value)
         (format nil "'~S" value))
        ((functionp value)
         (format nil "'~S" (find-symbol-function value)))
        ((xlib:color-p value)
         (format nil "(->color #x~X)" (color->rgb value)))
        (t (format nil "~S" value))))

(defun escape-conf-symbol-value (symbol)
  (let ((value (symbol-value symbol)))
    (escape-conf-value value)))

(defun get-config-value (value)
  (ignore-errors (eval (read-from-string value))))

(defun reset-config-to-default-value (symbol)
  (setf (symbol-value symbol) (config-default-value symbol)))


;;; Save configuration variables part
(defun temp-conf-file-name ()
  (let ((name (conf-file-name)))
    (make-pathname :directory (pathname-directory name)
		   :name (concatenate 'string (pathname-name name) "-tmp"))))


(defun copy-previous-conf-file-begin (stream-in stream-out)
  (loop for line = (read-line stream-in nil nil)
     while line
     until (zerop (or (search ";;; ### Internal variables definitions" line) -1))
     do (format stream-out "~A~%" line)))

(defun copy-previous-conf-file-end (stream-in stream-out)
  (loop for line = (read-line stream-in nil nil)
     while line
     until (zerop (or (search ";;; ### End of internal variables definitions" line) -1)))
  (loop for line = (read-line stream-in nil nil)
     while line
     do (format stream-out "~A~%" line)))



(defun save-variables-in-conf-file (stream)
  (multiple-value-bind (all-groups all-variables)
      (find-configuration-variables)
    (format stream "~&;;; ### Internal variables definitions                    ### ;;;~%")
    (format stream ";;; ### You can edit this part when clfswm is not running ### ;;;~%")
    (format stream ";;; ### And you can remove this part to revert to the     ### ;;;~%")
    (format stream ";;; ### original configuration variables values.          ### ;;;~%")
    (format stream "(in-package :clfswm)~2%")
    (format stream "(setf~%")
    (dolist (group all-groups)
      (format stream "  ;; ~A:~%" (config-group->string group))
      (dolist (var all-variables)
        (unless (equal (escape-conf-symbol-value (first var))
                       (escape-conf-value (config-default-value (first var))))
          (when (string-equal (second var) group)
            (format stream "  ~A ~A~%" (first var)
                    (escape-conf-symbol-value (first var))))))
      (format stream "~%"))
    (format stream ")~%")
    (format stream ";;; ### End of internal variables definitions ### ;;;~%")))




(defun save-configuration-variables ()
  "Save all configuration variables in clfswmrc"
  (let ((conffile (conf-file-name))
	(tempfile (temp-conf-file-name)))
    (with-open-file (stream-in conffile :direction :input :if-does-not-exist :create)
      (with-open-file (stream-out tempfile :direction :output :if-exists :supersede)
	(copy-previous-conf-file-begin stream-in stream-out)
	(save-variables-in-conf-file stream-out)
	(copy-previous-conf-file-end stream-in stream-out)))
    (delete-file conffile)
    (rename-file tempfile conffile)
    nil))


;;; Configuration menu definition

(defun group->menu (group)
  (intern (string-upcase (format nil "conf-~A" group)) :clfswm))

(defun query-conf-value (var string original)
  (labels ((warn-wrong-type (result original)
	     (if (equal (simple-type-of result) (simple-type-of original))
		 result
		 (if (query-yes-or-no "~A and ~A are not of the same type (~A and ~A). Do you really want to use this value?"
				      (escape-conf-value result) (escape-conf-value original)
                                      (type-of result) (type-of original))
		     result
		     original)))
	   (ask-set-default-value (original-val)
	     (let ((default (config-default-value var)))
	       (if (query-yes-or-no "Reset ~A from ~A to ~A?" var original (escape-conf-value default))
		   default
		   original-val))))
    (multiple-value-bind (result return)
	(query-string (format nil "Configure ~A - ~A (blank=Default: ~A)" string
			      (documentation var 'variable)
                              (escape-conf-value (config-default-value var)))
		      original)
      (let ((original-val (get-config-value original)))
	(if (equal return :Return)
	    (if (string= result "")
		(ask-set-default-value original-val)
		(let ((result-val (get-config-value result)))
		  (warn-wrong-type result-val original-val)))
	    original-val)))))


(defun create-conf-function (var)
  (let* ((string (remove #\* (format nil "~A" var)))
	 (symbol (intern (format nil "CONFIGURE-~A" string) :clfswm)))
    (setf (symbol-function symbol) (lambda ()
				     (setf (symbol-value var) (query-conf-value var string (escape-conf-symbol-value var)))
				     (open-menu (find-menu 'configuration-menu)))
	  (documentation symbol 'function) (format nil "Configure ~A" string))
    symbol))


(defun create-configuration-menu (&key clear)
  "Configuration menu"
  (when clear
    (clear-sub-menu 'main 'configuration-menu))
  (multiple-value-bind (all-groups all-variables)
      (find-configuration-variables)
    (loop for group in all-groups
       for i from 0
       do (let ((menu (group->menu group)))
	    (add-sub-menu 'configuration-menu (number->char i) menu (config-group->string group))
	    (loop for var in all-variables
	       with j = -1
	       do (when (equal (second var) group)
		    (add-menu-key menu (number->char (incf j))
				  (create-conf-function (first var))))))))
  (add-menu-key 'configuration-menu "F2" 'save-configuration-variables)
  (add-menu-key 'configuration-menu "F3" 'reset-all-config-variables))



(defun reset-all-config-variables ()
  "Reset all configuration variables to their default values"
  (when (query-yes-or-no "Do you really want to reset all values to their default?")
    (maphash (lambda (key val)
               (declare (ignore val))
               (reset-config-to-default-value key))
             *config-var-table*))
  (open-menu (find-menu 'configuration-menu)))

