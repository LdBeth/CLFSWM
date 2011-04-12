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

(defpackage :clfswm-client
  (:use :common-lisp :crypt)
  (:export :start-client))

(in-package :clfswm-client)

(defun uquit ()
  #+(or clisp cmu) (ext:quit)
  #+sbcl (sb-ext:quit)
  #+ecl (si:quit)
  #+gcl (lisp:quit)
  #+lispworks (lw:quit)
  #+(or allegro-cl allegro-cl-trial) (excl:exit)
  #+ccl (ccl:quit))


;;(defparameter *server-port* 33333)

(defun print-output (sock &optional wait)
  (when (or wait (ignore-errors (listen sock)))
    (let ((line (ignore-errors (string-trim '(#\newline) (read-line sock nil nil)))))
      (when line
	(format t "~&~A" (decrypt line *key*))
	(force-output)))))


(defun quit-on-command (line sock)
  (when (member line '("quit" "close" "bye") :test #'string-equal)
    (loop for line = (read-line sock nil nil)
       while line
       do (format t "~&~A" (decrypt line *key*))
	 (force-output))
    (terpri)
    (uquit)))


(defun parse-args (sock args)
  (unless (string= args "")
    (multiple-value-bind (form pos)
	(read-from-string args)
      (let ((str (format nil "~A" form)))
	(format t "~A~% " str)
	(format sock "~A~%" (crypt str *key*))
	(force-output sock)
	(print-output sock t)
	(quit-on-command str sock)
	(parse-args sock (subseq args pos))))))


(defun start-client (args &optional (url "127.0.0.1") (port clfswm::*server-port*))
  (load-new-key)
  (let* ((sock (port:open-socket url port))
	 (key (string-trim '(#\Newline #\Space) (decrypt (read-line sock nil nil) *key*))))
    (setf *key* (concatenate 'string key *key*))
    (write-line (crypt (format nil "~A~A" *key* (md5:md5 *key*)) *key*) sock)
    (force-output sock)
    (print-output sock t)
    (dolist (a args)
      (parse-args sock a))
    (loop
       (print-output sock)
       (when (listen)
	 (let ((line (read-line)))
	   (write-line (crypt line *key*) sock)
	   (force-output sock)
	   (quit-on-command line sock)))
       (sleep 0.01))))

