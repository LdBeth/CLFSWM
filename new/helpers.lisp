(in-package #:flavors)

(defun parse-body (body)
  #+asdf3.2 (uiop:parse-body body :documentation nil)
  #+sbcl (sb-int:parse-body body nil t)
  #-(or asdf3.2 sbcl) (error "Don't have UIOP"))

(defun print-flavor-instance (flavor stream)
  (print-unreadable-object (flavor stream :identity t)
    (princ (flavor-instance-class-name flavor) stream)))
