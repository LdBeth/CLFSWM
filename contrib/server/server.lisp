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

(in-package :clfswm)

(defparameter *server-port* 33333)

(format t "Loading the clfswm server code... ")

(pushnew (truename (merge-pathnames "server/" *contrib-dir*)) asdf:*central-registry*)

(dbg asdf:*central-registry*)

(asdf:oos 'asdf:load-op :clfswm-client)

(in-package :clfswm)

(use-package :crypt)

(defstruct server-socket stream auth form key)
(defparameter *server-socket* nil)

(defparameter *server-allowed-host* '("127.0.0.1"))
(defparameter *server-wait-timeout* 0.001d0)

(defparameter *server-connection* nil)

(defparameter *server-commands* '("bye" "close" "quit" "info" "clear" "ls[d][v|f] [pattern]"))




(defun server-show-prompt (sock)
  ;;(send-to-client sock nil (format nil "~A> " (package-name *package*))))
  (format (server-socket-stream sock) "~A~%"
          (crypt (format nil"~A> " (package-name *package*)) (server-socket-key sock)))
  (force-output (server-socket-stream sock)))


(defun send-to-client (sock show-prompt-p &rest msg)
  (dolist (m (if (consp (car msg)) (car msg) msg))
    (format (server-socket-stream sock) "~A~%" (crypt m (server-socket-key sock)))
    (force-output (server-socket-stream sock)))
  (when show-prompt-p
    (server-show-prompt sock)))

;;(defun server-show-prompt (sock)
;;  (send-to-client sock nil (format nil "~A> " (package-name *package*))))



(defun read-from-client (sock)
  (decrypt (read-line (server-socket-stream sock) nil nil) (server-socket-key sock)))



(defun server-remove-connection (sock)
  (send-to-client sock nil "Connection closed by server")
  (multiple-value-bind (local-host local-port remote-host remote-port)
      (port:socket-host/port (server-socket-stream sock))
    (declare (ignore local-host local-port))
    (format t "~&Connection from ~A:~A closed.~%" remote-host remote-port))
  (close (server-socket-stream sock))
  (setf *server-connection* (remove sock *server-connection*)))

(defun server-show-info (sock)
  (send-to-client sock t (format nil "~A" *server-connection*)))


(defun server-clear-connection ()
  (dolist (sock *server-connection*)
    (handler-case
	(send-to-client sock t "Server clear connection in progress.")
      (error ()
	(server-remove-connection sock)))))


(defun server-show-help (sock)
  (send-to-client sock t (format nil "Availables commandes: ~{~S~^, ~}" *server-commands*)))


(defun server-ls (sock line ls-word var-p fun-p &optional show-doc)
  (let* ((pattern (string-trim '(#\space #\tab) (subseq (string-trim '(#\space #\tab) line) (length ls-word))))
	 (all-search (string= pattern "")))
    (with-all-internal-symbols (symbol :clfswm)
      (when (or all-search (symbol-search pattern symbol))
	(cond ((and var-p (boundp symbol))
	       (send-to-client sock nil (format nil "~A (variable) ~A" symbol
						(if show-doc
						    (format nil "~&  ~A~&  => ~A"
							    (documentation symbol 'variable)
							    (symbol-value symbol))
						    ""))))
	      ((and fun-p (fboundp symbol))
	       (send-to-client sock nil (format nil "~A (function) ~A" symbol
						(if show-doc
						    (documentation symbol 'function)
						    "")))))))
    (send-to-client sock t "Done.")))



(defun server-is-allowed-host (stream)
  (multiple-value-bind (local-host local-port remote-host remote-port)
      (port:socket-host/port stream)
    (declare (ignore local-host local-port))
    (and (member remote-host *server-allowed-host* :test #'string-equal)
	 (equal remote-port *server-port*))))


(defun server-handle-new-connection ()
  (handler-case
      (let ((stream (and *server-socket* (port:socket-accept *server-socket* :wait *server-wait-timeout*))))
	(when stream
	  (if (server-is-allowed-host stream)
	      (multiple-value-bind (local-host local-port remote-host remote-port)
		  (port:socket-host/port stream)
		(declare (ignore local-host local-port))
		(format t "~&New connection from ~A:~A " remote-host remote-port)
		(let ((new-sock (make-server-socket :stream stream :auth nil :form "" :key *key*))
		      (key (generate-key)))
		  (push  new-sock *server-connection*)
		  (send-to-client new-sock nil key)
		  (setf (server-socket-key new-sock) (concatenate 'string key *key*))))
	      (close stream))))
    (error (c)
      (format t "Connection rejected: ~A~%" c)
      (force-output))))


(defun server-line-is (line &rest strings)
  (dolist (str strings)
    (when (string-equal line str)
      (return-from server-line-is t)))
  nil)


(defun server-complet-from (sock)
  (ignore-errors
    (when (listen (server-socket-stream sock))
      (let ((line (read-from-client sock)))
	(cond ((server-line-is line "help") (server-show-help sock))
	      ((server-line-is line "bye" "close" "quit") (server-remove-connection sock))
	      ((server-line-is line "info") (server-show-info sock))
	      ((server-line-is line "clear") (server-clear-connection))
	      ((first-position "lsdv" line) (server-ls sock line  "lsdv" t nil t))
	      ((first-position "lsdf" line) (server-ls sock line  "lsdf" nil t t))
	      ((first-position "lsd" line) (server-ls sock line  "lsd" t t t))
	      ((first-position "lsv" line) (server-ls sock line  "lsv" t nil nil))
	      ((first-position "lsf" line) (server-ls sock line  "lsf" nil t nil))
	      ((first-position "ls" line) (server-ls sock line "ls" t t nil))
	      (t (setf (server-socket-form sock) (format nil "~A~A~%" (server-socket-form sock) line))))))))





(defun server-eval-form (sock)
  (let* ((result nil)
	 (printed-result
	  (with-output-to-string (*standard-output*)
	    (setf result (handler-case
			     (loop for i in (multiple-value-list
					     (eval (read-from-string (server-socket-form sock))))
				collect (format nil "~S" i))
			   (error (condition)
			     (format nil "~A" condition)))))))
    (send-to-client sock nil (ensure-list printed-result))
    (send-to-client sock t (ensure-list result))
    (setf (server-socket-form sock) "")))


(defun server-handle-form (sock)
  (server-complet-from sock)
  (if (server-socket-key sock)
      (when (ignore-errors (read-from-string (server-socket-form sock)))
	(server-eval-form sock))
      (server-show-prompt sock)))

(defun server-handle-auth (sock)
  (loop for line = (read-from-client sock)
     while line
       do
       (if (string= line (format nil "~A~A" (server-socket-key sock)
				 (md5:md5 (server-socket-key sock))))
	   (progn
	     (setf (server-socket-auth sock) t)
	     (setf (server-socket-form sock) (format nil "~S" "You are now authenticated!"))
	     (server-handle-form sock)
	     (format t "Connection accepted~%")
	     (return-from server-handle-auth nil))
	   (progn
	     (format t "Connection closed~%")
	     (close (server-socket-stream sock))))))


(defun server-handle-connection (sock)
  (handler-case
      (when (listen (server-socket-stream sock))
	(if (server-socket-auth sock)
	    (server-handle-form sock)
	    (server-handle-auth sock)))
    (error (c)
      (format t "*** Error: ~A~%" c) (force-output)
      (close (server-socket-stream sock))
      (setf *server-connection* (remove sock *server-connection*)))))

(defun handle-server ()
  (server-handle-new-connection)
  (dolist (sock *server-connection*)
    (server-handle-connection sock)))



(defun start-server (&optional port)
  (when port
    (setf *server-port* port))
  (setf *server-socket* (port:open-socket-server *server-port*))
  (add-hook *loop-hook* 'handle-server)
  (format t "*** Server is started on port ~A and is accepting connection only from [~{~A~^, ~}].~2%"
	  *server-port* *server-allowed-host*)
  (save-new-key))




(format t "done.

You can now start a clfswm server with the command (start-server &optional port).
Only [~{~A~^, ~}] ~A allowed to login on the server. The connection is crypted.
You can start the client with the '--client' command line option.~%"
	*server-allowed-host*
	(if (or (null *server-allowed-host*) (= (length *server-allowed-host*) 1))
	    "is" "are"))

(defun server-parse-cmdline ()
  (let ((args (get-command-line-words)))
    (when (member "--client" args :test #'string-equal)
      (clfswm-client:start-client (remove "--client" args :test #'string-equal))
      (uquit))))

(defun is-started-as-client-p ()
  (member "--client" (get-command-line-words) :test #'string-equal))

(add-hook *main-entrance-hook* 'server-parse-cmdline)

