(in-package #:flavors)

(defun parse-body (body)
  #+asdf3.2 (uiop:parse-body body :documentation nil)
  #+sbcl (sb-int:parse-body body nil t)
  #-(or asdf3.2 sbcl) (error "Don't have UIOP"))

(defun print-flavor (flavor stream depth)
  (declare (ignore depth))
  (print-unreadable-object (flavor stream :type t :identity t)
    (princ (flavor-name flavor) stream)))

(defun print-flavor-instance (instance stream)
  (print-unreadable-object (instance stream :identity t)
    (princ (flavor-instance-class-name instance) stream)))

(defun find-flavor (flavor-name &optional (error-p t))
  (check-type flavor-name (and symbol (not null)))
  (check-type error-p (member t nil))
  (or (get flavor-name 'flavor-pattern)
      (and error-p (error "Flavor ~S not found." flavor-name))))

(defsetf find-flavor (flavor-name)  (store)
  `(setf (get ,flavor-name 'flavor-pattern) ,store))
