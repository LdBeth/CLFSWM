;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Client/server connection.
;;; The connection is crypted and you can only connect to the server with the
;;; same clfswm binary.
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
;;; Server protocole:
;;;  Server ->  Client:  orig_key=a generated key crypted with *key*
;;;  Client           :  build its new_key with orig_key+*key*
;;;  Client ->  Server:  new_key+(md5 new_key) crypted with new_key
;;;  Server ->  Client:  check if the keys match and then authenticate the client.
;;;  Server <-> Client:  All connections are crypted with new_key
;;; --------------------------------------------------------------------------

(in-package :common-lisp-user)

(defpackage :crypt
  (:use :common-lisp)
  (:export :crypt
	   :decrypt
	   :generate-key))

(in-package :crypt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
	(princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))



(defmacro circ-loop (binding &body body)
  "Loop circularly over some sequences.
binding is a list of (variable sequence).
The loop is the same size of the first sequence.
Each variable binding element is bound to each character in the
sequence in the second element.
See 'test-circ-loop for some usage examples."
  (labels ((let-body (prefix list)
	     (loop for i from 0
		for l in list
		collect `(,(symb prefix "-" i) (coerce ,(second l) 'list))))
	   (loop-var-name (l)
	      (symb "LOOP-VAR-" (first l)))
	   (do-body (prefix list)
	     (cons (list (loop-var-name (first list))
			 (symb prefix "-" 0)
			 `(cdr ,(loop-var-name (first list))))
		   (loop for i from 1
		      for l in (cdr list)
		      collect (list (loop-var-name l)
				    (symb prefix "-" i)
				    `(or (cdr ,(loop-var-name l))
					 ,(symb prefix "-" i))))))
	   (stop-body (list)
	     (list `(null ,(loop-var-name (first list)))))
	   (symbol-body (list)
	     (loop for l in list
		collect `(,(first l) (car ,(loop-var-name l))))))
    (let ((prefix (gensym)))
      `(let (,@(let-body prefix binding))
	 (do ,(do-body prefix binding)
	     ,(stop-body binding)
	   (symbol-macrolet ,(symbol-body binding)
	     ,@body))))))

(defun test-circ-loop ()
  (print 'first-test)
  (circ-loop ((m "Ceci est un test. יאח^# 1234567890")
	      (k "azerty")
	      (p "test")
	      (o "123"))
    (print (list m k p o)))
  (print 'second-test) (terpri)
  (circ-loop ((a #(1 2 3 4 5 6 7 8 9 10))
	      (b '(1 2 3))
	      (c "abcd"))
    (format t "(~A ~A ~A) " a b c)))



(defun crypt-to-list (msg &optional (size 4))
  (let ((len (length msg)))
    (when (zerop (mod len size))
      (loop for i from 0 below (/ len size)
	 collect (parse-integer (subseq msg (* i size) (* (1+ i) size)) :radix 16 :junk-allowed t)))))



(defun crypt (msg key)
  (with-output-to-string (str)
    (circ-loop ((m msg) (k key))
      (format str "~4,'0X" (logxor (char-code m) (char-code k))))))


(defun decrypt (msg key)
  (with-output-to-string (str)
    (circ-loop ((m (crypt-to-list msg 4)) (k key))
      (princ (code-char (logxor m (char-code k))) str))))

(defun test ()
  (let* ((key "11a3e229084349bc25d97e29393ced1d")
	 (msg (format nil "~C Ceci est un test. יאח^# 1234567890" (code-char 100)))
	 (crypt (crypt msg key))
	 (decrypt (decrypt crypt key)))
    (format t "msg:     ~A~%Crypt:   ~A~%Decrypt: ~A~%" msg crypt decrypt)))




(let* ((dic (with-output-to-string (str)
	      (dotimes (i 26)
		(princ (code-char (+ i (char-code #\a))) str)
		(princ (code-char (+ i (char-code #\A))) str))
	      (dotimes (i 10)
		(princ (code-char (+ i (char-code #\0))) str))))
       (dic-size (length dic)))
  (defun generate-key (&optional (min-size 10) (max-size 30))
    (let ((length (+ (random (- max-size min-size)) min-size)))
      (with-output-to-string (str)
	(dotimes (i length)
	  (princ (aref dic (random dic-size)) str))))))
