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

(in-package :crypt)

(export '(load-new-key
	  save-new-key
	  *key*))

(defparameter *key-filename* "/tmp/.clfswm-server.key")

(defparameter *key* "Automatically changed")

(defparameter *initial-key-perms* "0600")
(defparameter *final-key-perms* "0400")




(defun ushell-sh (formatter &rest args)
  (labels ((remove-plist (plist &rest keys)
	     "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
	     (do (copy rest)
		 ((null (setq rest (nth-value 2 (get-properties plist keys))))
		  (nreconc copy plist))
	       (do () ((eq plist rest))
		 (push (pop plist) copy)
		 (push (pop plist) copy))
	       (setq plist (cddr plist))))
	   (urun-prog (prog &rest opts &key args (wait t) &allow-other-keys)
	     "Common interface to shell. Does not return anything useful."
	     #+gcl (declare (ignore wait))
	     (setq opts (remove-plist opts :args :wait))
	     #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
			      :wait wait opts)
	     #+(and clisp      lisp=cl)
	     (apply #'ext:run-program prog :arguments args :wait wait opts)
	     #+(and clisp (not lisp=cl))
	     (if wait
		 (apply #'lisp:run-program prog :arguments args opts)
		 (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
	     #+cmu (apply #'ext:run-program prog args :wait wait :output *standard-output* opts)
	     #+gcl (apply #'si:run-process prog args)
	     #+liquid (apply #'lcl:run-program prog args)
	     #+lispworks (apply #'sys::call-system-showing-output
				(format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
				opts)
	     #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
	     #+sbcl (apply #'sb-ext:run-program prog args :wait wait :output *standard-output* opts)
	     #+ecl (apply #'ext:run-program prog args opts)
	     #+ccl (apply #'ccl:run-program prog args opts)
	     #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl ccl ecl)
	     (error "Error: urun-prog not implemented")))
    (urun-prog "/bin/sh" :args (list "-c" (apply #'format nil formatter args)))))


(defun save-new-key ()
  (when (probe-file *key-filename*)
    (delete-file *key-filename*))
  (with-open-file (stream *key-filename* :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "Nothing useful~%"))
  (ushell-sh "chmod ~A ~A" *initial-key-perms* *key-filename*)
  (setf *key* (generate-key))
  (with-open-file (stream *key-filename* :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "~A~%" *key*))
  (ushell-sh "chmod ~A ~A" *final-key-perms* *key-filename*))

(defun load-new-key ()
  (if (probe-file *key-filename*)
      (with-open-file (stream *key-filename* :direction :input)
	(setf *key* (read-line stream nil nil)))
      (error "Key file ~S not found" *key-filename*)))


