;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Auto documentation tools
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


(defun is-string-keysym (k)
  (when (stringp k)
    (or (parse-integer k :junk-allowed t)
	(intern (string-upcase k)))))


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
								     (or (is-string-keysym (first k)) (first k)))))
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
	       (nreverse acc))
	(p (small "This documentation was produced with the CLFSWM auto-doc functions. To reproduce it, use the produce-doc-html-in-file or
the produce-all-docs function from the Lisp REPL."))
	(p (small "Something like this:<br>
LISP> (in-package :clfswm)<br>
CLFSWM> (produce-doc-html-in-file \"my-keys.html\")<br>
or<br> CLFSWM> (produce-all-docs)"))))
     0 stream)))


(defun produce-doc-html-in-file (filename)
  (format t "Producing html keys documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-doc-html (list *main-keys* *main-mouse* *second-keys* *second-mouse*
			    *info-keys* *info-mouse* *circulate-keys* *expose-keys* *expose-mouse*)
		      stream))
  (format t " done~%"))



(defun produce-doc (hash-table-key-list &optional (stream t) (display-producing-doc t))
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
		   (format stream "~&  ~20@<~{~@(~A~) ~}~> ~13@<~@(~A~)~>   ~A~%"
			   (state->modifiers (second k))
			   (remove #\# (remove #\\ (format nil "~S" (or (is-string-keysym (first k)) (first k)))))
			   (documentation (or (first v) (third v)) 'function))))
	     hk)
    (format stream "~2&"))
  (when display-producing-doc
    (format stream "~2%This documentation was produced with the CLFSWM auto-doc functions.
To reproduce it, use the produce-doc-in-file or the produce-all-docs
function from the Lisp REPL.

Something like this:
LISP> (in-package :clfswm)
CLFSWM> (produce-doc-in-file \"my-keys.txt\")
or
CLFSWM> (produce-all-docs)~2%")))



(defun produce-doc-in-file (filename)
  (format t "Producing text keys documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-doc (list *main-keys* *main-mouse* *second-keys* *second-mouse*
		       *info-keys* *info-mouse* *circulate-keys* *expose-keys* *expose-mouse*)
		 stream))
  (format t " done~%"))





;;; Menu autodoc functions
(defun produce-menu-doc (&optional (stream t))
  (labels ((rec (base)
	     (format stream "~2&~:(~A~)~%" (menu-name base))
	     (dolist (item (menu-item base))
	       (typecase item
		 (menu (format stream "~A: ~A~%" (menu-name item) (menu-doc item)))
		 (menu-item (aif (menu-item-key item)
				 (format stream "~A: ~A~%" it
					 (typecase (menu-item-value item)
					   (menu (format nil "< ~A >" (menu-doc (menu-item-value item))))
					   (t (documentation (menu-item-value item) 'function))))
				 (format stream "~A~%" (menu-item-value item))))))
	     (dolist (item (menu-item base))
	       (typecase item
		 (menu (rec item))
		 (menu-item (when (menu-p (menu-item-value item))
			      (rec (menu-item-value item))))))))
    (format stream "Here is the map of the CLFSWM menu:~%")
    (format stream "(By default it is bound on second-mode + m)~%")
    (rec *menu*)
    (format stream "~2%This documentation was produced with the CLFSWM auto-doc functions. To reproduce it, use the produce-menu-doc-in-file or
the produce-all-docs function from the Lisp REPL.

Something like this:
LISP> (in-package :clfswm)
CLFSWM> (produce-menu-doc-in-file \"my-menu.txt\")
or
CLFSWM> (produce-all-docs)~2%")))



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
		   (menu-item (push `(p ,(aif (menu-item-key item)
					      (format nil "~A: ~A" it
						      (typecase (menu-item-value item)
							(menu (format nil "<a href=\"#~A\">< ~A ></a>"
								      (menu-name (menu-item-value item))
								      (menu-doc (menu-item-value item))))
							(t (documentation (menu-item-value item) 'function))))
					      (format nil "~A" (menu-item-value item))))
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
		       ,@(nreverse menu-list)
		       (p (small "This documentation was produced with the CLFSWM auto-doc functions. To reproduce it, use the produce-menu-doc-html-in-file or
the produce-all-docs function from the Lisp REPL."))
		       (p (small "Something like this:<br>
LISP> (in-package :clfswm)<br>
CLFSWM> (produce-menu-doc-html-in-file \"my-menu.html\")<br>
or<br> CLFSWM> (produce-all-docs)"))))
		    0 stream))))


(defun produce-menu-doc-html-in-file (filename)
  (format t "Producing html menus documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-menu-doc-html stream))
  (format t " done~%"))



;;; Corner autodoc functions
(defun produce-corner-doc (&optional (stream t))
  (labels ((print-doc (corner-list)
	     (format stream "~2&~:(~A~):~%" corner-list)
	     (dolist (corner (symbol-value corner-list))
	       (format stream "  ~:(~A:~) ~A~%" (first corner)
		       (if (fboundp (second corner))
			   (documentation (second corner) 'function)
			   "---")))))
    (format stream "Here are the actions associated to screen corners in CLFSWM:")
    (dolist (corner '(*corner-main-mode-left-button* *corner-main-mode-middle-button* *corner-main-mode-right-button*
		      *corner-second-mode-left-button* *corner-second-mode-middle-button* *corner-second-mode-right-button*))
      (print-doc corner))
    (format stream "~2%This documentation was produced with the CLFSWM auto-doc functions.
To reproduce it, use the produce-corner-doc-in-file or
the produce-all-docs function from the Lisp REPL.

Something like this:
LISP> (in-package :clfswm)
CLFSWM> (produce-corner-doc-in-file \"my-corner.txt\")
or
CLFSWM> (produce-all-docs)~2%")))


(defun produce-corner-doc-in-file (filename)
  (format t "Producing text corner documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-corner-doc stream))
  (format t " done~%"))



(defun produce-corner-doc-html (&optional (stream t))
  (let ((corner-html nil))
    (labels ((one-corner (corner-list)
	       (push `(h3 ,corner-list) corner-html)
	       (push `("table class=\"ex\" cellspacing=\"5\" border=\"0\" width=\"100%\""
		       ,@(loop :for corner :in (symbol-value corner-list)
			    :collect `(tr ("td align=\"left\" width=\"1%\" style=\"color:#FF0000\" nowrap"
					   ,(format nil "~:(~A~):" (first corner)))
					  ("td style=\"color:#0000FF\" nowrap"
					   ,(if (fboundp (second corner))
						(documentation (second corner) 'function)
						"---")))))
		     corner-html))
	     (fill-corner-list ()
	       (dolist (corner '(*corner-main-mode-left-button* *corner-main-mode-middle-button* *corner-main-mode-right-button*
				 *corner-second-mode-left-button* *corner-second-mode-middle-button* *corner-second-mode-right-button*))
		 (one-corner corner))))
      (fill-corner-list)
      (produce-html `(html
		      (head
		       (title "CLFSWM Corners"))
		      (body
		       (h1 ("a name=\"Top\"" "CLFSWM Corners"))
		       (p "Here are the actions associated to screen corners in CLFSWM:")
		       ,@(nreverse corner-html)
		       (p (small "This documentation was produced with the CLFSWM auto-doc functions. To reproduce it, use the produce-corner-doc-html-in-file or
the produce-all-docs function from the Lisp REPL."))
		       (p (small "Something like this:<br>
LISP> (in-package :clfswm)<br>
CLFSWM> (produce-corner-doc-html-in-file \"my-corner.html\")<br>
or<br> CLFSWM> (produce-all-docs)"))))
		    0 stream))))


(defun produce-corner-doc-html-in-file (filename)
  (format t "Producing html corner documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-corner-doc-html stream))
  (format t " done~%"))



;;; Configuration variables autodoc functions
(defun produce-conf-var-doc (stream &optional (group t) (title t) (footnote t))
  (when title
    (format stream "    * CLFSWM Configuration variables *~%")
    (format stream "      ------------------------------~2%"))
  (format stream "<= ~A =>~2%" (if (equal group t) ""
                                     (config-group->string group)))
  (maphash (lambda (key val)
             (when (or (equal group t)
                       (equal group (configvar-group val)))
               (format stream "  ~A = ~S~%    ~A~%" key (symbol-value key)
                       (documentation key 'variable))))
           *config-var-table*)
  (when footnote
    (format stream "~2& Those variables can be changed in clfswm.
Maybe you'll need to restart clfswm to take care of new values")
    (format stream "~2%This documentation was produced with the CLFSWM auto-doc functions.
To reproduce it, use the produce-conf-var-doc-in-file or
the produce-all-docs function from the Lisp REPL.

Something like this:
LISP> (in-package :clfswm)
CLFSWM> (produce-conf-var-doc-in-file \"my-variables.txt\")
or
CLFSWM> (produce-all-docs)~2%"))
  (format stream "~2%"))

(defun produce-conf-var-doc-in-file (filename)
  (format t "Producing text config variables documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (let* ((title t)
           (all-groups (config-all-groups))
           (last-group (first (last all-groups))))
      (dolist (group all-groups)
        (produce-conf-var-doc stream group title
                                             (equal group last-group))
        (setf title nil))))
  (format t " done~%"))


(defun produce-conf-var-doc-html (&optional (stream t))
  (let ((all-groups (config-all-groups)))
    (labels ((conf-var-group ()
               `((h3 ("a name='TOP'" "Configuration variables groups:"))
                 (ul ,@(loop for group in all-groups
                          collect `(li (,(format nil "a href='#~A'" group) ,(config-group->string group)))))))
             (colorize-line (group list)
               (let ((acc nil))
                 (dolist (line list)
                   (cond ((search "* =" line)
                          (let ((pos (position #\= line)))
                            (push `("font color='#FF0000'" ,(format nil "&nbsp;&nbsp;~(~A~)" (subseq line 0 (1- pos)))) acc)
                            (push `("font color='#0000FF'" ,(format nil "~A<br>" (subseq line (1- pos)))) acc)))
                         ((search "<=" line)
                          (push `(p (,(format nil "a name='~A' href='#TOP'" group) ,(escape-html line))) acc))
                         ((not (string= line " "))
                          (push (format nil "&nbsp; &nbsp; &nbsp; &nbsp; ~A<br>~%" line) acc))))
                 (nreverse acc)))
             (conf-var (group)
               (colorize-line group
                              (split-string (append-newline-space
                                             (with-output-to-string (stream)
                                               (produce-conf-var-doc stream group nil nil)))
                                            #\Newline)))
             (all-conf-var ()
               (let ((acc nil))
                 (dolist (group all-groups)
                   (setf acc (nconc acc (conf-var group))))
                 acc)))
      (produce-html `(html
                      (head
                       (title "CLFSWM - Configuration variables"))
                      (body
                       (h1 ("a name='Top'" "CLFSWM - Configuration variables"))
                       (p "Here are the variables you can configure in CLFSWM with the configuration file or the configuration menu:")
                       ,@(conf-var-group)
                       ,@(all-conf-var)
                       (p (small "This documentation was produced with the CLFSWM auto-doc functions. To reproduce it, use the produce-conf-var-doc-html-in-file or
the produce-all-docs function from the Lisp REPL."))
                       (p (small "Something like this:<br>
LISP> (in-package :clfswm)<br>
CLFSWM> (produce-conf-var-doc-html-in-file \"my-variables.html\")<br>
or<br> CLFSWM> (produce-all-docs)"))))
                    0 stream))))


(defun produce-conf-var-doc-html-in-file (filename)
  (format t "Producing html configuration variables documentation in ~S " filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede :if-does-not-exist :create)
    (produce-conf-var-doc-html stream))
  (format t " done~%"))








(defun produce-all-docs ()
  "Produce all docs in keys.html and keys.txt"
  (produce-doc-in-file "doc/keys.txt")
  (produce-doc-html-in-file "doc/keys.html")
  (produce-menu-doc-in-file "doc/menu.txt")
  (produce-menu-doc-html-in-file "doc/menu.html")
  (produce-corner-doc-in-file "doc/corner.txt")
  (produce-corner-doc-html-in-file "doc/corner.html")
  (produce-conf-var-doc-in-file "doc/variables.txt")
  (produce-conf-var-doc-html-in-file "doc/variables.html"))





