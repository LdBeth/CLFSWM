;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Configuration definitions and Menu generation
;;;
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


(defun find-configuration-variables ()
  (let ((all-groups nil)
	(all-variables nil))
    (with-all-internal-symbols (symbol :clfswm)
      (when (is-config-p symbol)
	(pushnew (config-group symbol) all-groups :test #'string-equal)
	(push (list symbol (config-group symbol)) all-variables)))
    (values all-groups all-variables)))


(defun escape-conf-value (value)
  (let ((value (symbol-value value)))
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
	  (t (format nil "~S" value)))))



;;; Configuration variables save

(defun find-symbol-function (function)
  (with-all-internal-symbols (symbol :clfswm)
    (when (and (fboundp symbol) (equal (symbol-function symbol) function))
      (return-from find-symbol-function symbol))))

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
    (format stream "~2&;;; ### Internal variables definitions                    ### ;;;~%")
    (format stream ";;; ### You can edit this part when clfswm is not running ### ;;;~%")
    (format stream ";;; ### And you can remove this part to revert to the     ### ;;;~%")
    (format stream ";;; ### original configuration variables values.          ### ;;;~%")
    (format stream "(in-package :clfswm)~2%")
    (format stream "(setf~%")
    (dolist (group all-groups)
      (format stream "  ;; ~A:~%" group)
      (dolist (var all-variables)
	(when (string-equal (second var) group)
	  (format stream "  ~A ~A~%" (first var)
		  (escape-conf-value (first var)))))
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
  (intern (string-upcase
	   (format nil "conf-~A" (substitute #\- #\Space group)))
	  :clfswm))

(defun query-conf-value (string original)
  (labels ((warn-wrong-type (result original)
	     (if (equal (simple-type-of result) (simple-type-of original))
		 result
		 (if (string-equal
		      (query-string
		       (format nil "~S and ~S are not of the same type (~A and ~A). Do you really want to use this value? (yes/no)"
			       result original (type-of result) (type-of original))
		       "no")
		      "yes")
		     result
		     original))))
     (multiple-value-bind (result return)
	 (query-string (format nil "Configure ~A" string) original)
       (let ((result-val (eval (read-from-string result)))
	     (original-val (eval (read-from-string original))))
	 (if (member return '(:Return :Complet))
	     (warn-wrong-type result-val original-val)
	     original-val)))))


(defun create-conf-function (var)
  (let* ((string (remove #\* (format nil "~A" var)))
	 (symbol (intern (format nil "CONFIGURE-~A" string) :clfswm)))
    (setf (symbol-function symbol) (lambda ()
				     (setf (symbol-value var) (query-conf-value string (escape-conf-value var)))
				     (open-menu (find-menu 'configuration-menu)))
	  (documentation symbol 'function) (format nil "Configure ~A" string))
    symbol))


(defun create-configuration-menu ()
  "Configuration menu"
  (multiple-value-bind (all-groups all-variables)
      (find-configuration-variables)
    (add-menu-key 'configuration-menu "a" 'save-configuration-variables)
    (loop for group in all-groups
       for i from 1
       do (let ((menu (group->menu group)))
	    (add-sub-menu 'configuration-menu (number->char i) menu group)
	    (loop for var in all-variables
	       with j = -1
	       do (when (equal (second var) group)
		    (add-menu-key menu (number->char (incf j))
				  (create-conf-function (first var)))))))))






