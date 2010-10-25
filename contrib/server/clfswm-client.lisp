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


(defparameter *server-port* 33333)

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


(defun start-client (args &optional (url "127.0.0.1") (port *server-port*))
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

