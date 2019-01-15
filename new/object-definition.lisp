
(in-package #:flavors)

;;; --> PATTERN DEFINITION.
;;; This is the definition of the class pattern. It attempts to provide most
;;; of the fields normally associated with a flavor. Not everything is
;;; supported. The methods are held in a hash table, and no COMBINED-METHODS
;;; function is provided: the body of SEND will collect and execute.
;;; ---
(defstruct flavor
  name
  vars
  known-lexical-ivs
  initable-instance-variables
  init-keywords
  default-init-plist
  required-init-keywords
  depends-on
  depended-on-by
  precedence
  methods)
;;; --> END PATTERN DEFINITION

;;; --> FLAVOR-PRINTER
;;; This function providies the default printing capability associated
;;; with the flavor structure
;;; ---
(defun flavor-printer (object &optional (stream t) (depth 1))
  "function interface to :print-self method"
  (if (handles-p object :print-self)
    (send object :print-self stream depth)
    (print-flavor-instance object stream)))
;;; --> END FLAVOR-PRINTER

;;; --> INSTANCE DEFINITION
;;; This is the definition of the defstruct that holds a flavor object instance.
;;; It contains fields for the class name, methods and instance variables.
;;; Note that it knows how to print itself without having a method to do it.
;;; The Vanilla Flavor HAS a :print-self method to which every flavor will
;;; then respond. The print function for the defstruct will simply print to
;;; the screen as output from a function or as part of an error message.
;;; ---
#|
(defstruct (flavor-instance :named (:print-function flavor-printer))
  class-name			           ; class name
  vars				; ((var . value) ... ) instance variables
)|#
(cl:defclass flavor-instance ()
  ((class-name :initarg :class-name :initform nil
               :accessor flavor-instance-class-name)
   (vars :initarg :vars :initform nil
         :accessor flavor-instance-vars))
  (:metaclass funcallable-standard-class))

(cl:defmethod print-object ((object flavor-instance) stream)
  (flavor-printer object stream))

(cl:defmethod initialize-instance :after ((instance flavor-instance) &rest
                                          initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (set-funcallable-instance-function
   instance
   (lambda (message &rest args) (apply #'send instance message args))))

(defun flavor-instance-p (object)
  (typep object 'flavor-instance))

(defun make-flavor-instance (&rest args)
  (apply #'cl:make-instance 'flavor-instance args))
;;; --> END INSTANCE DEFINITION
