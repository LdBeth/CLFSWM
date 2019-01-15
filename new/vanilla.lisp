(in-package #:flavors)

;;; I guess someone forgot to add vanilla...
;;  This part is modified from Symbolics Genera vanilla definition.
(defflavor vanilla () () :no-vanilla-flavor)

(defmethod (vanilla :describe) ()
  (format t "~a, an object of flavor ~a,
 has instance variable values:~%"
          self
          (flavor-instance-class-name self))
  (dolist (item (flavor-instance-vars self))
    (format t " ~s   ~s~%" (car item) (cdr item))))

(defmethod (vanilla :print-self) (&optional (stream t) (depth 1))
  (declare (ignore depth))
  (print-flavor-instance self stream))

(defmethod (vanilla :operation-handled-p) (operation)
  (when (handles-p self operation)
    t))

(defmethod (vanilla :send-if-handles) (message &rest args)
  (when (send self :operation-handled-p message)
    (lexpr-send self message args)))
