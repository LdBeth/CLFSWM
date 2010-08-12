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
	     #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
	     (error 'not-implemented :proc (list 'run-prog prog opts))))
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


