(in-package #:flavors)

;;; I guess someone forgot to add vanilla...
;;  This part is modified from Symbolics Genera vanilla definition.
(defflavor vanilla () () :no-vanilla-flavor)

(defmethod (vanilla :describe) ()
  (describe-flavor (slot-value cl::self 'class-name)))

(defmethod (vanilla :print-self) (&optional (stream t) (depth 1))
  (declare (ignore depth))
  (format stream "#<~a ~d>"
          (flavor-instance-class-name cl::self)
          (flavor-instance-unique-number cl::self)))
