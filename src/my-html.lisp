;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Html generator helper
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



(in-package :common-lisp-user)

(defpackage :my-html
  (:use :common-lisp :tools)
  (:export :insert-html-doctype
           :escape-html
	   :produce-html
	   :with-html
	   :produce-html-string))

(in-package :my-html)


(defun insert-html-doctype ()
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/html4/transitional.dtd\">")


(defun escape-html (string &optional (replace '((">" "&gt;") ("<" "&lt;"))))
  (if replace
      (aif (search (caar replace) string)
           (escape-html (concatenate 'string (subseq string 0 it)
                                     (cadar replace)
                                     (subseq string (+ it (length (caar replace)))))
                replace)
           (escape-html string (cdr replace)))
      string))




(defun produce-html (tree &optional (level 0) (stream *standard-output*))
  (cond ((listp tree)
	 (print-space level stream)
	 (format stream "~(<~A>~)~%" (first tree))
	 (dolist (subtree (rest tree))
	   (produce-html subtree (+ 2 level) stream))
	 (print-space level stream)
	 (format stream "~(</~A>~)~%"
		 (if (stringp (first tree))
		     (subseq (first tree) 0 (position #\Space (first tree)))
		     (first tree))))
	(t
	 (print-space level stream)
	 (format stream (if (stringp tree) "~A~%" "~(~A~)~%") tree))))


(defmacro with-html ((&optional (stream t)) &rest rest)
  `(produce-html ',@rest 0 ,stream))


(defun produce-html-string (tree &optional (level 0))
  (with-output-to-string (str)
    (produce-html tree level str)))




(defun test1 ()
  (produce-html `(html
		  (head
		   (title "Plop"))
		  (body
		   (h1 "A title")
		   (h2 "plop")
		   Plop ,(+ 2 2)
		   ,(format nil "Plip=~A" (+ 3 5))
		   ("a href=\"index.html\"" index)
		   (ul
		    (li "toto")
		    (li "klm"))))))


(defun test2 ()
  (with-html ()
    (html
     (head
      (title "Plop"))
     "<img src=\"toto.png\">"
     (body
      (h1 "Un titre")
      (h2 "plop")
      (ul
       (li "toto")
       (li "klm"))))))


(defun test3 ()
  (produce-html-string `(html
			 (head
			  (title "Plop"))
			 (body
			  (h1 "A title")
			  (h2 plop)
			  Plop ,(+ 2 2)
			  ,(format nil "Plip=~A" (+ 3 5))
			  |Foo Bar Baz|
			  ("a href=\"index.html\"" Index)
			  (ul
			   (li "toto")
			   (li "klm"))))
		       10))





