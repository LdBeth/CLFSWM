;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Auto documentation tools
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


(defun produce-doc-html (hash-table-key-list &optional (stream t))
  "Produce an html doc from a hash-table key"
  (labels ((clean-string (str)
	     (cond ((string-equal str "#\\:") ":")
		   ((string-equal str "#\\#") "#")
		   ((string-equal str "#\\\\") "\\")
		   (t (substitute #\Space #\#
				  (substitute #\Space #\\
					      (substitute #\Space #\: str))))))
	   (produce-keys (hk)
	     `("table class=\"ex\" cellspacing=\"5\" border=\"0\" width=\"100%\""
	       (tr ("th align=\"right\" width=\"10%\"" "Modifiers")
		   ("th align=\"center\" width=\"10%\"" "Key/Button")
		   ("th align=\"left\"" "Function"))
	       ,@(let ((acc nil))
		      (maphash #'(lambda (k v)
				   (when (consp k)
				     (push `(tr
					     ("td align=\"right\" style=\"color:#FF0000\" nowrap"
					      ,(clean-string (format nil "~{~@(~S ~)~}" (state->modifiers (second k)))))
					     ("td align=\"center\" nowrap"
					      ,(clean-string (format nil "~@(~S~)"
								     (or (and (stringp (first k))
									      (intern (string-upcase (first k))))
									 (first k)))))
					     ("td style=\"color:#0000FF\" nowrap" ,(documentation (or (first v) (third v)) 'function)))
					   acc)))
			       hk)
		      (nreverse acc)))))
    (produce-html
     `(html
       (head
	(title "CLFSWM Keys"))
       (body
	(h1 "CLFSWM Keys")
	(p (small "Note: Mod-1 is the Meta or Alt key"))
	,@(let ((acc nil))
	       (dolist (hk hash-table-key-list)
		 (push `(h3 (u ,(gethash 'name hk))) acc)
		 (push (produce-keys hk) acc))
	       (nreverse acc))))
     0 stream)))


(defun produce-doc-html-in-file (filename)
  (format t "Producing html keys documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-doc-html (list *main-keys* *main-mouse* *second-keys* *second-mouse*
			    *info-keys* *info-mouse*)
		      stream))
  (format t " done~%"))



(defun produce-doc (hash-table-key-list &optional (stream t))
  "Produce a text doc from a hash-table key"
  (format stream "    * CLFSWM Keys *~%")
  (format stream "      -----------~%")
  (format stream "~%Note: Mod-1 is the Meta or Alt key~%")
  (dolist (hk hash-table-key-list)
    (format stream "~2&~A:~%" (gethash 'name hk))
    (dotimes (i (length (gethash 'name hk)))
      (format stream "-"))
    (format stream "~2%")
    (maphash #'(lambda (k v)
		 (when (consp k)
		   (format stream "~&~20@<~{~@(~A~) ~}~> ~13@<~@(~A~)~>   ~A~%"
			   (state->modifiers (second k))
			   (remove #\# (remove #\\ (format nil "~S" (or (and (stringp (first k))
									     (intern (string-upcase (first k))))
									(first k)))))
			   (documentation (or (first v) (third v)) 'function))))
	     hk)
    (format stream "~2&")))

			   

(defun produce-doc-in-file (filename)
  (format t "Producing text keys documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-doc (list *main-keys* *main-mouse* *second-keys* *second-mouse*
		       *info-keys* *info-mouse*)
		 stream))
  (format t " done~%"))





;;; Menu autodoc functions
(defun produce-menu-doc (&optional (stream t))
  (labels ((rec (base)
	     (format stream "~2&~:(~A~)~%" (menu-name base))
	     (dolist (item (menu-item base))
	       (typecase item
		 (menu (format stream "~A: ~A~%" (menu-name item) (menu-doc item)))
		 (menu-item (format stream "~A: ~A~%" (menu-item-key item)
				    (typecase (menu-item-value item)
				      (menu (format nil "< ~A >" (menu-doc (menu-item-value item))))
				      (t (documentation (menu-item-value item) 'function)))))))
	     (dolist (item (menu-item base))
	       (typecase item
		 (menu (rec item))
		 (menu-item (when (menu-p (menu-item-value item))
			      (rec (menu-item-value item))))))))
    (format stream "Here is the map of the CLFSWM menu:~%")
    (format stream "(By default it is bound on second-mode + m)~%")
    (rec *menu*)))


  
(defun produce-menu-doc-in-file (filename)
  (format t "Producing text menus documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-menu-doc stream))
  (format t " done~%"))




(defun produce-menu-doc-html (&optional (stream t))
  (let ((menu-list nil))
    (labels ((rec (base parent)
	       (push `(h3 ,(format nil "<a name=\"~A\"></a><a href=\"#~A\">~:(~A~)</a>"
				   (menu-name base)
				   (if parent (menu-name parent) "Top")
				   (menu-name base))) menu-list)
	       (dolist (item (menu-item base))
		 (typecase item
		   (menu (push `(p ,(format nil "~A: ~A" (menu-name item) (menu-doc item))) menu-list))
		   (menu-item (push `(p ,(format nil "~A: ~A" (menu-item-key item)
						 (typecase (menu-item-value item)
						   (menu (format nil "<a href=\"#~A\">< ~A ></a>"
								 (menu-name (menu-item-value item))
								 (menu-doc (menu-item-value item))))
						   (t (documentation (menu-item-value item) 'function)))))
				    menu-list))))
	       (push '<hr> menu-list)
	       (dolist (item (menu-item base))
		 (typecase item
		   (menu (rec item base))
		   (menu-item (when (menu-p (menu-item-value item))
				(rec (menu-item-value item) base)))))))
      (rec *menu* nil)
      (produce-html `(html
		      (head
		       (title "CLFSWM Menu"))
		      (body
		       (h1 ("a name=\"Top\"" "CLFSWM Menu"))
		       (p "Here is the map of the CLFSWM menu:"
			  "(By default it is bound on second-mode + m)")
		       ,@(nreverse menu-list)))
		    0 stream))))

  
(defun produce-menu-doc-html-in-file (filename)
  (format t "Producing html menus documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-menu-doc-html stream))
  (format t " done~%"))







(defun produce-all-docs ()
  "Produce all docs in keys.html and keys.txt"
  (produce-doc-in-file "doc/keys.txt")
  (produce-doc-html-in-file "doc/keys.html")
  (produce-menu-doc-in-file "doc/menu.txt")
  (produce-menu-doc-html-in-file "doc/menu.html"))

