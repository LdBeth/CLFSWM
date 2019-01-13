(in-package #:flavors)

;;; I guess someone forgot to add vanilla...
;;  This part is modified from Symbolics Genera vanilla definition.
(defflavor vanilla () () :no-vanilla-flavor)

(defmethod (vanilla :describe) ()
  (format t "~a, an object of flavor ~a,
 has instance variable values:~%"
          cl::self
          (flavor-instance-class-name cl::self))
  (dolist (item (flavor-instance-vars cl::self))
    (format t " ~s   ~s~%" (car item) (cdr item))))

(defmethod (vanilla :print-self) (&optional (stream t) (depth 1))
  (declare (ignore depth))
  (format stream "#<~a ~d>"
          (flavor-instance-class-name cl::self)
          (flavor-instance-unique-number cl::self)))

