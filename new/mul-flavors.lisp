
;;; ----------- M(ITRE)U(niversity of)L(owell)-FLAVORS PACKAGE -----------
;;; First running version by G. Pecelli, 2/86 ULowell
;;; Modified by S. Markowitz, 4/86  MITRE Corp
;;; Modified by M.J. Prelle, S. Goldkind, J. C. Fohlin 11/88 MITRE Corp
;;; Modified by G. Pecelli 12/88 ULowell
;;; Modified by J.I. Leivent to add defwhoppers 2/89 MITRE Corp
;;; ----------------------------------------------------------------------
;;; This is a joint effort, implemented as part of other projects that needed
;;; a rudimentary FLAVORS package over Lisp environments that did not possess
;;; such a package. It is put in the public domain and distributed by the
;;; Computer Science Department of the University of Lowell.
;;; Users are permitted to modify this package and to further distribute it
;;; as a public domain package without any payment of royalties. They are 
;;; required to acknowledge the provenance of this code.
;;; COPYRIGHT: MITRE Corporation and University of Lowell

;;; === WARNINGS: This package implements only part of the FLAVORS definition.
;;; It is also non-standard in the sense that its inheritance graph handling
;;; is based on CLOS (Common Lisp Object System) rather than FLAVORS. This
;;; should not be a problem in most garden variety uses, and should make
;;; code portability to CLOS easier.
;;; ===

(in-package #:flavors)

;;; The next two globals are used by CF-MULF, the file compiler function
;;; for MUL-Flavors
(defvar *compiling* nil)
(defvar *all-meth-lists* nil)
(defvar *all-flavor-names* nil)

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
  methods )
;;; --> END PATTERN DEFINITION

;;; --> FLAVOR-PRINTER
;;; This function providies the default printing capability associated
;;; with the flavor structure
;;; ---
(defun flavor-printer (object &optional (stream t) (depth 1))
  "function interface to :print-self method"
  (if (handles-p object :print-self)
    (send object :print-self stream depth)
    (format stream "#<~a ~d>" 
            (flavor-instance-class-name object) 
            (flavor-instance-unique-number object))))
;;; --> END FLAVOR-PRINTER

;;; --> INSTANCE DEFINITION
;;; This is the definition of the defstruct that holds a flavor object instance.
;;; It contains fields for the class name, methods and instance variables.
;;; Note that it knows how to print itself without having a method to do it.
;;; The Vanilla Flavor HAS a :print-self method to which every flavor will
;;; then respond. The print function for the defstruct will simply print to
;;; the screen as output from a function or as part of an error message.
;;; ---
(defstruct (flavor-instance :named (:print-function flavor-printer))
  class-name			           ; class name
  (unique-number (get-internal-real-time)) ; for identification when printing
  vars				; ((var . value) ... ) instance variables
  )
;;; --> END INSTANCE DEFINITION

;;; --> DEFFLAVOR
;;; This is the flavor definition macro. The syntax is a subset of the Zetalisp
;;; Flavors syntax. No keywords are supported at the moment.It is the same as 
;;; :settable-instance-variables in ZetaLisp. This keyword should be added
;;; on any code that is meant to be transported.
;;; ---
;;; INPUT: an object name; a list of instance variables with possible default
;;;        values; a list of parent flavors in order of access; keywords.
;;; OUTPUT: the object name.
;;; SIDE-EFFECTS: a pattern structure is built, accessible from the object, and
;;;      the flavor name is defined as a type.
;;; USER-DEFINED-CALLED: defflavor2, write-form, defflavor2-load,
;;;        defflavor2-compile.
;;; ---
;;; Example of call:
;;; (defflavor vehicle ((c 3) (d 4)) () :settable-instance-variables)
;;; (defflavor truck ((a 1) (b 2) (c 5)) 
;;;     (vehicle)
;;;     (:initable-instance-variables a b)
;;;     (:settable-instance-variables b c))
;;; ---
(defmacro defflavor (a-flavor-name class-vars depends-on &rest keyword-props)
  (cond (*compiling*            ;we are file-compiling a flavor definition
         (write-form 
          `(defflavor2-load 
             ',a-flavor-name ',class-vars ',depends-on ',keyword-props))
         `(defflavor2-compile 
            ',a-flavor-name ',class-vars ',depends-on ',keyword-props))
        (t                      ;definition in core
         `(defflavor2 
            ',a-flavor-name ',class-vars ',depends-on ',keyword-props))))
;;; --> END DEFFLAVOR 

;;; --> DEFFLAVOR2
;;; This function creates the flavor template. It creates a pattern structure, 
;;; used by make-instance. It collects all parents' variables, for convenience.
;;; ---
;;; INPUT: flavor name; list of instance variables with optional default 
;;;        initialization; depends-on list; keyword properties list.
;;; OUTPUT: flavor name, with pattern on its 'flavor-pattern property.
;;; SIDE-EFFECTS: structure creation and binding.
;;; USER-DEFINED-ACCESSED: create-predicate, make-flavor-pattern, flavor-methods,
;;;        merge-class-super-methods, get-class-methods, flavor-depends-on,
;;;        make-access-methods, flavor-precedence
;;; ASSUMPTIONS:  all instance variables are assumed initable
;;; ---
;;; Example of call (should NOT be called by the user)
;;; (DEFFLAVOR2 VEHICLE ((C 3) (D 4)) NIL (:SETTABLE-INSTANCE-VARIABLES))
;;; (DEFFLAVOR2 TRUCK ((A 1) (B 2) (C 5)) 
;;;   (VEHICLE) 
;;;   ((:INITABLE-INSTANCE-VARIABLES A B) (:SETTABLE-INSTANCE-VARIABLES B C)))
;;; ---
(defun defflavor2 (a-flavor-name class-vars depends-on keyword-props)
  (let ((pattern nil))
    (create-predicate a-flavor-name)
    (setq pattern 
          (make-flavor-pattern a-flavor-name class-vars 
                               depends-on keyword-props))
    (setf (flavor-methods pattern) 
          (merge-class-super-methods
           (get-class-methods (flavor-depends-on pattern)) 
           (flavor-precedence pattern)))
    (make-access-methods a-flavor-name class-vars keyword-props)
    ;(format t "~s~%" a-flavor-name)
    a-flavor-name))
;;; --> END DEFFLAVOR2

;;; --> MAKE-FLAVOR-PATTERN
;;; This function sets up the structure holding the flavor pattern. It returns
;;; an incomplete pattern, since it has no methods yet. The methods will be
;;; added by the appropriate version of defflavor2 (plain, load, compile..).
;;; This version understands :required-instance-variables (a nice thing to have 
;;; in real flavors, since it allows a mixin to "see" ivs from other mixins 
;;; directly without having to go through iv accessor methods).  The hack done 
;;; here is just to add the :required-instance-variable ivs in to the flavor as 
;;; if they were ivs defined directly for the flavor - this gives the desired
;;; result, but doesn't do the same type of error checking that real flavors
;;; would do (real flavors would make sure that this flavor gets mixed in with
;;; one or more that do provide the required instance variables).
;;; ---
;;; INPUT: a flavor name, class variable list with initialization values,
;;;        inheritance list in precedence order, keyword list
;;; OUTPUT: the flavor pattern structure
;;; SIDE-EFFECTS: creates a flavor structure and adds it to the property list
;;;        of a-flavor-name
;;; USER-DEFINED-CALLED: update-parents, merge-class-super-vars, get-class-vars,
;;;        make-flavor, set-precedence, option-find
;;; ---
(defun make-flavor-pattern (a-flavor-name class-vars depends-on keyword-props)
  (let ((key-class-vars nil)
        (all-key-class-vars nil)
        (i-s-vs nil))
    (if (and (null depends-on)        ;top of a hierarchy 
             (not (member :no-vanilla-flavor keyword-props)))
      (setq depends-on (list 'vanilla)))  
    (update-parents a-flavor-name depends-on)
    (setq key-class-vars           ;make everything uniform with "right" names
          (mapcar #'(lambda (x)
                      (if (consp x)
                        (cons (intern (symbol-name (car x)) 'keyword)
                              (cadr x))
                        (cons (intern (symbol-name x) 'keyword) nil)))
		  (append (option-find :required-instance-variables
				       keyword-props)
			 class-vars))) ;done with vars at bottom level
    (setq all-key-class-vars 
          (merge-class-super-vars key-class-vars (get-class-vars depends-on)))
    ;add vars from upper levels - lower names prevail
    (setq i-s-vs  
          (if (or (member :initable-instance-variables keyword-props)
                  (member :settable-instance-variables keyword-props))
            (mapcar #'car key-class-vars) ;(:A :B :C) ;no only mine???
            (option-find :initable-instance-variables keyword-props)));(A B) bug?
    (setf (get a-flavor-name 'flavor-pattern)
          (make-flavor 
           :name a-flavor-name
           :vars all-key-class-vars
           :known-lexical-ivs (mapcar #'(lambda (x) 
                                          (intern (symbol-name (car x)))) 
                                      all-key-class-vars)
           :depends-on depends-on
           :depended-on-by nil
           :precedence (set-precedence a-flavor-name depends-on)
           :initable-instance-variables i-s-vs  ;locally declared
           :init-keywords (remove-duplicates    ;all: also inherited
                           (append 
                            (option-find :init-keywords keyword-props)
                            i-s-vs
                            (apply #'append 
                                   (mapcar #'flavor-init-keywords
                                           (mapcar #'(lambda (x)
                                                       (get x 'flavor-pattern))
                                                   depends-on)))))
           :default-init-plist (option-find :default-init-plist keyword-props)
           :required-init-keywords 
           (option-find :required-init-keywords keyword-props)
           :methods nil))
    (if (not (member a-flavor-name *all-flavor-names*))
      (push a-flavor-name *all-flavor-names*))
    (get a-flavor-name 'flavor-pattern)))
;;; --> END MAKE-FLAVOR-PATTERN

;;; --> MAKE-ACCESS-METHODS
;;; This function creates the automatically generated methods for a flavor
;;; ---
;;; INPUT: a list of (var.val) pairs and the keyword options to defflavor
;;; OUTPUT: a list of (method-name (:primary method-code-pointer)) pairs
;;; SIDE-EFFECTS: none.
;;; USER-DEFINED-CALLED: create-accessor-method, create-setter-method
;;; ---
;;; Example of call (no user calls)
;;; (MAKE-ACCESS-METHODS ((:A 1) (:B 2) (:C 5)) 
;;;                      ((:INITABLE-INSTANCE-VARIABLES A B) 
;;;                       (:SETTABLE-INSTANCE-VARIABLES B C)))
;;; ---
(defun make-access-methods (a-flavor-name slots keyword-props)
  (let ((the-gets nil)
        (the-sets nil))
    (if (or (member :gettable-instance-variables keyword-props)
            (member :settable-instance-variables keyword-props)
            (member :initable-instance-variables keyword-props))
      (setq the-gets 
            (mapcar #'(lambda (x) (if (consp x) (car x) x)) slots))
      (setq the-gets 
            (remove-duplicates 
             (append 
              (option-find :initable-instance-variables keyword-props)
              (option-find :gettable-instance-variables keyword-props)
              (option-find :settable-instance-variables keyword-props)))))
    (setq the-gets           ;make keywords and intern them in right package
          (mapcar #'(lambda (item) (intern (string item) 'keyword)) 
                  the-gets))
    (if (member :settable-instance-variables keyword-props) ;all settable
      (setq the-sets 
            (mapcar #'(lambda (x) (if (consp x) (car x) x)) slots))
      (setq the-sets                                        ;any settable??
            (option-find :settable-instance-variables keyword-props)))
    (setq the-sets           ;make keywords and intern them in right package
          (mapcar #'(lambda (item) (intern (string item) 'keyword)) 
                  the-sets))
    (if (member :no-vanilla-flavor keyword-props)
      (create-init-method a-flavor-name))
    (dolist (item the-gets)
      (create-accessor-method a-flavor-name item))
    (dolist (item the-sets)
      (create-setter-method a-flavor-name item))))
;;; --> END MAKE-ACCESS-METHODS

;;; --> CREATE-INIT-METHOD
;;; This function creates the :init method for a given flavor class. It checks
;;; that the initialization is legal, while also merging any default 
;;; initialization information into the currently specified init-plist
;;; ---
;;; INPUT: a flavor name
;;; OUTPUT: undefined
;;; SIDE-EFFECTS: it creates the :init method for the given flavor
(defun create-init-method (a-flavor-name)
  (eval `(defmethod (,a-flavor-name :init) (init-plist)
           (let ((fl-patt (get (flavor-instance-class-name cl::self) 
                                'flavor-pattern)))
             (do ((def-init-plist (flavor-default-init-plist fl-patt) 
                    (cddr def-init-plist)))
                 ((null def-init-plist))
               (if (not (assoc (car def-init-plist) init-plist)) ;not there yet
                 (push (cons (car def-init-plist) (eval (cadr def-init-plist)))
                       init-plist)))
             (setf (flavor-instance-vars cl::self)
                   (nconc init-plist (flavor-instance-vars cl::self)))))))
;;; --> END CREATE-INIT-METHOD

;;; --> CREATE-ACCESSOR-METHOD
;;; This creates a unique function that will access the requested structure 
;;; field. 
;;; ---
;;; INPUT: slot name and offset number
;;; OUTPUT: the unique function assigned to read it
;;; SIDE-EFFECTS: function definition
;;; USER-DEFINED-CALLED: none.
;;; ---
(defun create-accessor-method (a-flavor-name slot-name)
  (let ((non-key-slot-name (intern (string slot-name))))
    (eval `(defmethod (,a-flavor-name ,slot-name) () ,non-key-slot-name))))
;;; --> END CREATE-ACCESSOR-METHOD

;;; --> CREATE-SETTER-METHOD
;;; This creates a unique function that will set the requested structure field.
;;; The creation of the setter method makes use of the way in which DEFMETHOD
;;; handles Instance Variables: in particular, the SETQ will be changed to a
;;; SETF called on the appropriate accessor for the flavor instance 
;;; ---
;;; INPUT: slot name and offset number
;;; OUTPUT: the unique function assigned to set it
;;; SIDE-EFFECTS: function definition
;;; USER-DEFINED-CALLED: none.
;;; ---
(defun create-setter-method (a-flavor-name slot-name)
  (let ((non-key-slot-name (intern (string slot-name)))
        (set-name 
         (intern (concatenate 'string "SET-" (string slot-name)) 'keyword)))
    (eval `(defmethod (,a-flavor-name ,set-name) (val) 
             (setq ,non-key-slot-name val)))))
;;; --> END CREATE-SETTER-METHOD

;;;--> FETCH-FLAVORS
;;; This function goes up the hierarchy and collects all the flavors in th
;;; class structure of a flavor pattern. This assumes that remove-duplicates
;;; remove the leftmost occurrences of the name...
;;; ---
;;; INPUT: a flavor name
;;; OUTPUT: a flavor list
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: flavor-depends-on, fetch-flavors.
;;; ---
(defun fetch-flavors (a-flavor-name)
  (let ((flavors (flavor-depends-on (get a-flavor-name 'flavor-pattern))))
    (if (null flavors) 
      (list a-flavor-name) ;end of the line
      (cons a-flavor-name 
            (remove-duplicates
             (apply #'append (mapcar #'fetch-flavors flavors)))))))
;;; --> END FETCH-FLAVORS

;;; --> CREATE-PREDICATE
;;; This function creates the predicate function used to check if something is
;;; a particular type of object - flavor kind
;;; ---
;;; INPUT: a flavor name
;;; OUTPUT: nothing usable
;;; SIDE-EFFECTS: two functions are created, both capable of checking that
;;;        the object in question is of the right type.
;;; USER-DEFINED-CALLED: f-inst-check, fetch-flavors, flavor-instance-class-name
;;; ---
(defun create-predicate (a-flavor-name)
  (let ((pred-name (intern (format nil "~a-P" a-flavor-name))))
    (compile pred-name `(lambda
   ;(eval `(defun ,pred-name  ; to avoid compilation, instead
                          (obj)
                          (and (typep obj 'flavor-instance)  ;an instance of something??
                               (member ',a-flavor-name ;is it the right instance
                                       (fetch-flavors (flavor-instance-class-name obj))))))
    (eval `(deftype ,a-flavor-name () '(satisfies ,pred-name)))))
;;; --> END CREATE-PREDICATE

;;; --> UPDATE-PARENTS
;;; This function updates the dependency information for the parents of a given
;;; pattern. It lets the parent patterns know that the new child has been added
;;; ---
;;; INPUT: (something evaluating to) a flavor name and to a parent list
;;; OUTPUT: nil
;;; SIDE-EFFECTS: each parent has the flavor-depended-on slot updated
;;; USER-DEFINED-CALLED: flavor-depended-on
;;; ---
(defun update-parents (a-flavor-name the-parents)
  (dolist (item the-parents)
    (push a-flavor-name (flavor-depended-on-by (get item 'flavor-pattern)))))
;;; --> END UPDATE-PARENTS

;;; --> SET-PRECEDENCE
;;; This function determines the exact inheritance precedence for the ancestor
;;; graph of a given flavor class. It uses an algorithm derived from W.&H. and
;;; modified for use here. IT DEPENDS VERY HEAVILY ON THE REMOVE-DUPLICATES
;;; function, expecting that the leftmost occurrences of of a symbol will be
;;; removed first, leaving only the rightmost. If your version of 
;;; remove-duplicates does not operate this way, please modify code accordingly
;;; ---
;;; INPUT: a flavor name and a parent list
;;; OUTPUT: the dependency list
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: flavor-precedence
;;; ---
;;; Example of call:
;;; (set-precedence 'sail-ship '(ship))
;;; ---
(defun set-precedence (a-flavor-name depends-on)
  (if (null depends-on)
    (list a-flavor-name)
    (let ((new-precedence nil))
      (dolist (item (reverse depends-on))
        (setq new-precedence 
              (append (flavor-precedence (get item 'flavor-pattern)) 
                      new-precedence)))
      (remove-duplicates (cons a-flavor-name new-precedence)))))
;;; --> END SET-PRECEDENCE

;;; --> OPTION-FIND
;;; This function extracts the variables affected by the setting of a given
;;; option.
;;; ---
;;; INPUT: a keyword option
;;; OUTPUT: a variable list (not as keywords)
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: none
;;; ---
;;; Example of call
;;; (option-find :initable-instance-variables 
;;;              '((:initable-instance-variables a b)
;;;                (:settable-instance-variables b c)))
;;;    returns --> (A B)
;;; ---
(defun option-find (option keyword-props)
  "like (cdr (assoc ...)) but ignores non-list elements in the second argument"
  ;(defflavor zoom (w x y z) () (:settable-instance-variables x w))
  (cdr (find option keyword-props :key #'(lambda(x) (and (consp x) (car x))))))
;;; --> END OPTION-FIND

;;; --> GET-CLASS-VARS
;;; This function retrieves copies of the parents' variables in the hierarchy,
;;; using the dependency list. Assumed that no reach past parents is needed.
;;; ---
;;; INPUT: a depends-on list.
;;; OUTPUT: the (copied) list of all parents' instance variables.
;;; SIDE-EFFECTS: none.
;;; USER-DEFINED-CALLED: flavor-p, flavor-vars, get-class-vars
;;; ---
;;; Example of call:
;;; (get-class-vars '(vehicle))
;;; ((:C . 3) (:D . 4))
;;; ---
(defun get-class-vars (depends-on)
  (cond ((null depends-on) nil)
        ((flavor-p depends-on) 
         (copy-alist (flavor-vars depends-on)))           ;make copy!!!
        (t (nconc (get-class-vars (get (car depends-on)   ;surgery.
                                       'flavor-pattern))
                  (get-class-vars (cdr depends-on))))))
;;; --> END GET-CLASS-VARS

;;; --> MERGE-CLASS-SUPER-VARS
;;; This function merges the class and superclass variables, so the lower left
;;; variables will override the higher and to the right ones.
;;; ---
;;; INPUT: list of variables from the hierarchy, list of class variables
;;; OUTPUT: a merged list without repetitions and with the correct scope
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: 
;;; ---
;;; Example of call:
;;;  (merge-class-super-vars '((:A . 1) (:B . 2) (:C . 5)) 
;;;                          (get-class-vars '(vehicle)))
;;;  returns --> ((:A . 1) (:B . 2) (:C . 5) (:D . 4))
;;; ---
(defun merge-class-super-vars (key-class-vars super-vars)
  (let ((final-vars key-class-vars))
    (dolist (item (reverse super-vars) final-vars)
      (if (not (assoc (car item) final-vars))
        (push item final-vars)))))
;;; --> END MERGE-CLASS-SUPER-VARS

;;; --> GET-CLASS-METHODS
;;; This function retrieves copies of the parents' methods in the hierarchy,
;;; using the dependency list. Assumed that no reach past parents is needed
;;; ---
;;; INPUT: a depends-on list.
;;; OUTPUT: the (copied) list of all parents' instance variables.
;;; SIDE-EFFECTS: none.
;;; USER-DEFINED-CALLED: flavor-p, flavor-methods, get-class-methods
;;; ---
(defun get-class-methods (depends-on)
  (if depends-on
    (mapcar #'flavor-methods 
            (mapcar #'(lambda (x) (get x 'flavor-pattern))
                    depends-on))))
;;; --> END GET-CLASS-METHODS

;;; --> MERGE-CLASS-SUPER-METHODS
;;; This function merges the local class methods with the inherited methods
;;; from the hierarchy. The major problem is the correct choice of :before
;;; and :after methods. The choice of :primary method is a little easier, since
;;; no sequencing is required, although the correct "sequencing" of the 
;;; hierarchy is necessary and non-trivial. CLOS inheritance rather than
;;; FLAVORS inheritance
;;; ---
;;; INPUT: a list of method hashtables in left to right order and a precedence
;;;        list
;;; OUTPUT: a methods hashtable
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED:
;;; ---
(defun merge-class-super-methods (super-methods precedence)
  (let ((final-methods ;make a new hash table of the right size
         (make-hash-table :size (max 30
                                     (apply #'+ 
                                       (mapcar #'hash-table-count 
                                               super-methods)))))
        (curr-method nil)
        (key-method-list nil))
    (dolist (h-table super-methods final-methods) 
      (setq key-method-list nil)
      (maphash #'(lambda (key val) 
                   (push (cons key (copy-alist val)) ;need to make copy...
                         key-method-list)) 
               h-table)    
      (dolist (item key-method-list)
        (cond ((setq curr-method  (gethash (car item) final-methods))
               (setf (gethash (car item) final-methods)
                     (adjust-methods (cadr item) curr-method precedence)))
              (t (setf (gethash (car item) final-methods)
                       (cdr item))))))))
;;; --> END MERGE-CLASS-SUPER-METHODS

;;; --> WITH-INSTANCE-VARIABLES
;;; This macro is a support function for the DEFMETHOD macro.
;;; example:
;;; (pprint (macroexpand '(with-instance-variables ship ship1 
;;;                        (print 'hello1)
;;;                        (setq zz (send ship1 :set-a (+ a 1)))
;;;                        (print zz))))
;;; ---
;;; INPUT: A symbol, which is a name of a flavor, a flavor instance and a body
;;; OUTPUT: The body code is replaced.  The replacement code begins with a
;;;         let that binds a local variable *instance-var-alist* to the function
;;;         that accesses the instance variables and there values.  The code is
;;;         wrapped in a progn.  new-body is the same as body except that every
;;;         setq is replaced by setf and every occurance of an instance variable
;;;         is replaced by
;;;         (cdr (assoc ,instance-var *instance-var-alist*)) unless it is quoted
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: none
;;; DOES NOT SUPPORT: rebinding of an instance variable to a local variable name
;;;                   function names that are the same as instance variables
;;; mjp 11-88
;;; ---
(defmacro with-instance-variables (a-flavor-name flavor-instance &body body)
  (let ((new-body nil)
        (instance-vars (flavor-known-lexical-ivs (get a-flavor-name 
                                                      'flavor-pattern))))
    (setq new-body (adjust-body instance-vars body))
    `(let ((*instance-var-alist* 
            (flavor-instance-vars ,flavor-instance)));not a gensym because of compiler bug in gclisp
       (progn ,@new-body))))
;;; --> END WITH-INSTANCE-VARIABLES

;;; --> ADJUST-BODY
;;; This function "adjusts" the body of a method so that assigments to Instance
;;; Variables are handled in a usable manner: if it is a var replace it by its 
;;; value expression; if it is involved in a SETQ, replace by the appropriate
;;; SETF; otherwise leave alone. This does not allow the appearance of local
;;; variables and function names that clash with the Instance Variables. Will
;;; have to be fixed in the future
;;; ---
;;; INPUT: a list of instance variables and a body-form
;;; OUTPUT: return the modified body.
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: adjust-body
;;; ---
#|(defun adjust-body (instance-vars body)
  (let ((key nil))
    (cond ((and (atom body) (member body instance-vars))
           (setq key (intern (string body) 'keyword))
           `(cdr (assoc ,key *instance-var-alist*)))
          ((consp body) 
           (cond ((eq (car body) 'setq)
                  (cons 'setf (adjust-body instance-vars (cdr body))))
                 ((eq (car body) 'quote)
                  body)
                 (t (cons (adjust-body instance-vars (car body)) 
                          (adjust-body instance-vars (cdr body))))))
          (t body))))|#

;;; mjp 12/89 handles lambda's that have no arguments

(defun adjust-body (instance-vars body)
  (let ((key nil))
    (cond ((and (atom body) (member body instance-vars))
           (setq key (intern (string body) 'keyword))
           `(cdr (assoc ,key *instance-var-alist*)))
          ((consp body) 
           (cond ((eq (car body) 'setq)
                  (cons 'setf (adjust-body instance-vars (cdr body))))
                 ((and (eq (car body) 'quote)
                       (consp (cadr body))
                       (eq (caadr body) 'lambda)
                       (null (second (cadr body))))
                       (list 'quote (list 'lambda nil
                        (adjust-body instance-vars (third (cadr body)))))
                       )
                 ((eq (car body) 'quote)
                  body)
                 (t (cons (adjust-body instance-vars (car body)) 
                          (adjust-body instance-vars (cdr body))))))
          (t body))))
;;; --> END ADJUST-BODY

;;; --> DEFMETHOD
;;; This macro creates a method associated with a particular flavor. The syntax
;;; is a subset of the ZetaLisp syntax. Notice the different calls to variants 
;;; of defmethod2 : FLAVOR and method compilation requires a little gymnastics
;;; ---
;;; INPUT: Three or more forms. The first two are lists, the second may be empty
;;; OUTPUT: the method name.
;;; SIDE-EFFECTS: a nameless function is created and saved on the property 
;;;         list of the flavor under the method name.
;;; USER-DEFINED-CALLED: defmethod2, defmethod2-compile.
;;; ---
(defmacro defmethod (object-key-method parameters &rest body)
  (cond (*compiling*
         `(defmethod2-compile ',object-key-method ',parameters ',body))
        (t
         `(defmethod2 ',object-key-method ',parameters ',body))))
;;; ---
(defun defmethod2 (object-key-method parameters body)     ;;body is now a list
  (let* ((a-flavor-name (car object-key-method))
         (pattern (get a-flavor-name 'flavor-pattern))
         key method-name)
    (cond ((= (length object-key-method) 3)
           (setq key (cadr object-key-method))
           (setq method-name (caddr object-key-method)))
          (t (setq key :primary)
             (setq method-name (cadr object-key-method))))
    (let* ((func-name           ;the method is made into a NAMED function
            (intern 
             (concatenate 'string (string (car object-key-method)) "-"
                          (string key) "-"
                          (string method-name))))
           (curr-method (gethash method-name (flavor-methods pattern)))
           (new-method 
            (cons key
                  (cons (car object-key-method)
                        (compile func-name `(lambda
                       ;(eval `(defun ,func-name ; to avoid compilation, instead
                                              ,(cons 'cl::self parameters) ;self is first...
                                              ,(macroexpand `(with-instance-variables 
                                                               ,a-flavor-name
                                                               cl::self
                                                               ,@body))))))))
      (if curr-method        ;there already - REPLACE
        (setf (gethash method-name (flavor-methods pattern))
              (adjust-methods new-method curr-method 
                              (flavor-precedence pattern)))
        (cond ((flavor-methods pattern) 
               (setf (gethash method-name (flavor-methods pattern))
                     (list  new-method)))
              (t (setf (flavor-methods pattern) (make-hash-table :size 30))
                 (setf (gethash method-name (flavor-methods pattern))
                     (list  new-method)))))
      (adjust-dependent-methods a-flavor-name method-name new-method))
    method-name))
;;; --> END DEFMETHOD

;;; --> ADJUST-METHODS
;;; This function gets rid of previous method definitions, allowing the dynamic
;;; change of method to be propagated to the correct descendants of a given class
;;; ---
;;; INPUT: a method, the old :before, :primary and :after list associated with 
;;;        method, and the precedence list for the flavor class
;;; OUTPUT: nothing usable
;;; SIDE-EFFECTS: alters the methods list
;;; USER-DEFINED-CALLED: primary-insert,combination-insert
;;; ---
(defun adjust-methods (new-method old-method-list precedence-list)
  (case (car new-method) 
    (:primary
     (primary-insert new-method old-method-list precedence-list))
    (:before
     (combination-insert new-method old-method-list precedence-list :before))
    (:whopper
     (combination-insert new-method old-method-list precedence-list :whopper))
    (:after
     (reverse
      (combination-insert 
       new-method (reverse old-method-list) precedence-list :after)))
    (t (error 
        "only method combinations supported are primary, before, after, and whopper~s~%"
        new-method))))
;;; --> END ADJUST-METHODS

;;; --> ADJUST-DEPENDENTS-METHODS
;;; This function looks at the dependents of a given class and changes the
;;; methods to reflect the changes higher up the hierarchy. Not all methods
;;; are changed, depending on the position of the current changes in the
;;; inheritance graph.
;;; ---
;;; INPUT: a flavor name, a method name and a new method
;;; OUTPUT: nil
;;; SIDE-EFFECTS: dependents of the given class MAY have their methods lists
;;;         altered
;;; USER-DEFINED-CALLED: flavor-depended-on, flavor-methods, adjust-methods,
;;;         flavor-precedence, adjust-dependent-methods
;;; ---
(defun adjust-dependent-methods (a-flavor-name method-name new-method)
  (let ((dependents (flavor-depended-on-by (get a-flavor-name 'flavor-pattern)))
        (dependent-pattern nil)
        (dependent-methods nil)
        (curr-method nil))
    (dolist (a-flavor-name-item dependents)
      (setq dependent-pattern (get a-flavor-name-item 'flavor-pattern))
      (cond ((setq dependent-methods ;do we have them yet???
                   (flavor-methods dependent-pattern))
             (if (setq curr-method (gethash method-name dependent-methods))
               (setf (gethash method-name dependent-methods)
                     (adjust-methods new-method 
                                     curr-method 
                                     (flavor-precedence dependent-pattern)))
               (setf (gethash method-name dependent-methods)
                     (list new-method)))
             (adjust-dependent-methods a-flavor-name-item 
                                       method-name new-method))))))    
;;; --> END ADJUST-DEPENDENTS-METHODS

;;; --> PRIMARY-INSERT
;;; This function inserts a new primary method, propagating the changes down
;;; the inheritance graph.
;;; ---
;;; INPUT: the new method, the current method info and the precedence info
;;; OUTPUT: the new method info
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: primary-insert
;;; ---
(defun primary-insert (new-method old-method-list precedence-list)
  (cond ((null old-method-list) 
         (list new-method))
        ((eq (caar old-method-list) :primary)
         (cond ((equal (cadr new-method) (cadr (car old-method-list))) ;ship
                (cons new-method (cdr old-method-list)))     ;replace old
               ((> (length (member (cadr new-method) precedence-list))
                   (length (member (cadr (car old-method-list)) 
                                   precedence-list)))        ;higher precedence
                (cons new-method (cdr old-method-list)))     ;replace old
               (t                                            ;lower precedence
                old-method-list)))                           ;forget new
        (t (cons (car old-method-list)                       ;tail recursion..
                 (primary-insert new-method 
                                 (cdr old-method-list) 
                                 precedence-list)))))
;;; --> END PRIMARY-INSERT

;;; --> COMBINATION-INSERT
;;; This function inserts the :before and :after methods in the correct position
;;; Note that no destruction takes place
;;; INPUT: a method, a method list, a precedence list, :before or :after
;;; OUTPUT: the new method list
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: combination-insert
;;; ---
(defun combination-insert (new-method old-method-list 
                                      precedence-list the-combinator)
  (cond ((null old-method-list) (list new-method))
        ((equal (caar old-method-list) the-combinator)
         (cond ((equal (cadr new-method) (cadr (car old-method-list))) ; ship
                (cons new-method (cdr old-method-list)))          ;replace old
               ((> (length (member (cadr new-method) precedence-list))
                   (length (member (cadr (car old-method-list)) 
                                   precedence-list)))   ;higher precedence
                (cons new-method old-method-list))                ;add in front
               (t (cons (car old-method-list)
                        (combination-insert new-method (cdr old-method-list) 
                                            precedence-list the-combinator)))))
        (t (cons (car old-method-list)
                 (combination-insert new-method (cdr old-method-list) 
                                     precedence-list the-combinator)))))
;;; --> END COMBINATION-INSERT

;;; --> MAKE-INSTANCE
;;; This creates an instance of a flavor. Syntax like ZetaLisp. The function
;;; does some error checking, collects the variables from the flavor
;;; pattern and calls the function that will allocate the space and initialize
;;; the instance variables.
;;; ---
;;; INPUT: a flavor name and a list of variables with initial values
;;; OUTPUT: and error message or what returned by make-instance-2
;;; SIDE-EFFECTS: no direct ones.
;;; USER-DEFINED-CALLED: flavor-p, flavor-vars, merge-class-inst-vars,
;;;        flavor-instance-vars, make-flavor-instance, eval-class-init, 
;;;        adjust-inst-vars
;;; ---
;;; Example of call:
;;;  (setq ship1 (make-instance 'ship :a 3 :b 5))
;;; ---
(defun make-instance (a-flavor-name &rest init-plist)
  (let ((flavor (get a-flavor-name 'flavor-pattern)))
    (cond ((oddp (length init-plist)) 
           (error "keyword value mismatch in make-instance ~s ~s~%" 
                  a-flavor-name init-plist))
          ((flavor-p flavor)                   ;it is a flavor pattern
           (let ((obj (make-flavor-instance :class-name a-flavor-name))) 
             (send obj         ;initialize by evaluating class vars, init-plist
                   :init       ;and then merging with precedence to locals..
                   (merge-class-inst-vars 
                    (eval-class-init        
                     (copy-alist (flavor-vars flavor)))
                    (adjust-inst-vars init-plist 
                                      (flavor-init-keywords flavor)
                                      flavor)))
             obj))
          (t (error "~s is not a flavor" a-flavor-name)))))
;;; --> END MAKE-INSTANCE

;;; --> INSTANTIATE-FLAVOR
;;; probably not a complete implementation
;;; allows the init-plist to be passed as a list
(defun instantiate-flavor (a-flavor-name init-plist)
  (let ((flavor (get a-flavor-name 'flavor-pattern)))
    (cond ((oddp (length init-plist)) 
           (error "keyword value mismatch in make-instance ~s ~s~%" 
                  a-flavor-name init-plist))
          ((flavor-p flavor)                   ;it is a flavor pattern
           (let ((obj (make-flavor-instance :class-name a-flavor-name))) 
             (send obj         ;initialize by evaluating class vars, init-plist
                   :init       ;and then merging with precedence to locals..
                   (merge-class-inst-vars 
                    (eval-class-init        
                     (copy-alist (flavor-vars flavor)))
                    (adjust-inst-vars init-plist 
                                      (flavor-init-keywords flavor)
                                      flavor)))
             obj))
          (t (error "~s is not a flavor" a-flavor-name)))))
;;; --> END INSTANTIATE-FLAVOR

;;; --> EVAL-CLASS-INIT
;;; This function takes a <variable . value> initialization list and evaluates
;;; the value part in the "current environment"
;;; ---
;;; INPUT: a list of <keyword . form> pairs:  ((:A . 1) (:B . (+ ZIP 2)))
;;; OUTPUT: the list with each <form> evaluated
;;; SIDE-EFFECTS: the second elements of the pairs are replaced by their current
;;;        values.
;;; USER-DEFINED-CALLED: none
;;; ---
(defun eval-class-init (vars)
  (dolist (item vars)
    (setf (cdr item) (eval (cdr item))))
  vars)
;;; --> END-EVAL-CLASS-INIT

;;; --> ADJUST-INST-VARS
;;; Takes the list of variable names and forms at instance creation and
;;; transforms it into a dotted pairs list with all evaluations carried out
;;; ---
;;; INPUT: a list 
;;; OUTPUT: a list
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: none
;;; ---
(defun adjust-inst-vars (inst-vars allowable-init-kwrds flavor)
  (do ((new-vars inst-vars (cddr new-vars))
       (var-list nil))
      ((null new-vars) var-list)
    (if (member (car new-vars) allowable-init-kwrds)
      (push (cons (car new-vars) (cadr new-vars)) var-list)
      (error "attempted to initialize unallowed variable in: <~s>~%" 
             (flavor-name flavor)))))
;;; --> END ADJUST-INST-VARS

;;; --> MERGE-CLASS-INST-VARS
;;; This function merges the class and instance variables, so that the 
;;; initializations at instantiation time will override the defaults.
;;; --- 
;;; INPUT: a list of (variable.value) pairs
;;; OUTPUT: a list of (variable.value) pairs
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: none
;;; ---
(defun merge-class-inst-vars (class-vars inst-vars)
  (dolist (var inst-vars class-vars) 
    (rplacd (assoc (car var) class-vars) (cdr var))))
;;; --> END MERGE-CLASS-INST-VARS

;;; --> SEND
;;; This function sends a method and a parameter list to a flavor, executes all
;;; the before methods, the leading primary method and all the after methods. 
;;; Mediates ALL communication with the object, including instance variable
;;; access and setting.
;;; ---
;;; INPUT: an flavor instance, a method name and a sequence of parameter values
;;; OUTPUT: the value of the primary method.
;;; SIDE-EFFECTS: no direct side-effects.
;;; USER-DEFINED-CALLED: a large number..., especially SEND-INTERNAL
;;; ---
(defun send (target method-name &rest arguments)
  (assert (typep target 'flavor-instance) ()
          "The object ~s is not a flavor instance" target)
  (let* ((*method-name-methods* ;get method of given name on the methods of class
           (gethash method-name 
                    (flavor-methods (get (flavor-instance-class-name target) 
                                         'flavor-pattern))))
         (*remaining-whoppers* *method-name-methods*))
    (declare (special *remaining-whoppers* *method-name-methods*))
    (cond ((null *method-name-methods*)
           (error 
            "The object ~s  received a ~s message which went unclaimed.~%The rest of the message was ~s~%"
            target method-name arguments))
          (t
           (send-internal target arguments)))))
;;; --- SEND-INTERNAL
;;; Send-internal is called from within send, and by continue-whopper and
;;; lexpr-continue-whopper.  It does most of the work of the original send
;;; routine, except for looking up the method-name-methods.  Any call to
;;; send-internal should have the special variable *method-name-methods* bound
;;; to the method-name-methods found in send.  Also, the special variable
;;; *remaining-whoppers* starts off being eq to *method-name-methods*, but it
;;; is popped while searching for the next whopper to run - the purpose of
;;; this popping is so that a recursive call to send-internal (done from
;;; within continue-whopper or lexpr-continue-whopper) will be able to
;;; determine what whopper to run next.  Notice how the processing of the
;;; normal parts of the method (the befores, primary, and afters) is done when
;;; there are no more remaining whoppers - this part of the code is only
;;; reached if every whopper calls [lexpr-]continue-whopper.
;;; ---
(defun send-internal (target args &aux a-meth primary)
  (declare (special *remaining-whoppers* *method-name-methods*))
  (do ((whops *remaining-whoppers* (cdr whops)))
      ((null whops) ;; no more whoppers, do the "regular" method stuff
       (dolist (a-meth *method-name-methods*)
	 (if (eq (car a-meth) :before)
	     (apply (cddr a-meth) target args)))
       (multiple-value-prog1
	(if (setq primary (cddr (assoc :primary *method-name-methods*)))
	    (apply primary target args))
	(dolist (a-meth *method-name-methods*)
	  (if (eq (car a-meth) :after)
	      (apply (cddr a-meth) target args)))))
    (setq a-meth (car *remaining-whoppers*))
    (when (eq (car a-meth) :whopper)
      ;; if it's a whopper, call it with *remaining-whoppers* bound to the
      ;; whoppers it is wrapped around
      (let ((*remaining-whoppers* (cdr whops)))
	(declare (special *remaining-whoppers*))
	(return (apply (cddr a-meth) target args))))))
;;; --- SETF METHOD FOR SEND
(define-setf-method send (target method-name &rest arguments)
  (let* ((temp-target (gensym))
         (old-method (gensym))
         (temp-method (gensym))
         (new-val (gensym)))
    (values
     (list temp-target old-method temp-method)
     (list target method-name  (intern 
                                (concatenate 'string "SET-" 
                                             (princ-to-string method-name))
                                'keyword))
     (list new-val)
     `(progn
        (send ,temp-target ,temp-method ,new-val)
        ,new-val)
     `(send ,temp-target ,old-method))))
;;; --> END SEND

;;; --- DEFWHOPPER
;;; defwhopper defines a whopper method for a flavor - whoppers are nice
;;; because they are a generalization of all possible types of method
;;; combination (when you have whoppers, you don't even need daemons!)
;;; ---
(defmacro defwhopper ((flavor message-name) args &body body)
  ;; define continue-whopper and lexpr-continue-whopper locally
  `(macrolet ((continue-whopper (&rest args)
		`(send-internal self (list . ,args)))
	      (lexpr-continue-whopper (&rest args)
		`(send-internal self (list* . ,args))))
      ;; the whopper is a :whopper type method
      (defmethod (,flavor :whooper ,message-name) ,args . ,body)))
;;; --- END DEFWHOPPER

;;; --- CONTINUE-WHOPPER & LEXPR-CONTINUE-WHOPPER ERROR CHECKING
;;; make continue-whopper and lexpr-continue-whopper give errors when used
;;; outside of defwhopper
(defmacro continue-whopper (&rest ignore)
  (declare (ignore ignore))
  (error "Continue-whopper can only be used inside a whopper"))

(defmacro lexpr-continue-whopper (&rest ignore)
  (declare (ignore ignore))
  (error "Lexpr-continue-whopper can only be used inside a whopper"))
;;; --- END CONTINUE-WHOPPER & LEXPR-CONTINUE-WHOPPER ERROR CHECKING

;;; ================== SOME EXPECTED SUPPORT FUNCTIONS  ==================

;;; --> LEXPR-SEND
;;; This is slightly different kind of send 
;;; ---
(defun lexpr-send (instance message arglist)
  (apply #'send (append (list instance message) arglist)))
;;; --> END LEXPR-SEND

;;; --> HANDLES-P
;;; Function checks for the existence of a :PRIMARY method of the given name
;;; associated with a given object
;;; ---
;;; INPUT: a flavor and a method name
;;; OUTPUT:nil or not nil
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: flavor-methods, flavor-instance-class-name
;;; ---
(defun handles-p (object method-name)
  "returns non-nil if the object handles the method"
  ;? (handles-p sship :speed)
  ;#<An Anonymous Compiled Function>
  (and (typep object 'flavor-instance)  ;must be a flavor instance
       (assoc :primary 
              (gethash method-name 
                       (flavor-methods 
                        (get (flavor-instance-class-name object) 
                             'flavor-pattern))))))
;;; --> END HANDLES-P

;;; --> GET-HANDLER-FOR
;;; Function checks for the existence of a :PRIMARY method of the given name
;;; associated with a given object and returns it or NIL. Result can be
;;; FUNCALLED...
;;; ---
;;; INPUT: a flavor and a method name
;;; OUTPUT:nil or a method
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: flavor-methods, flavor-instance-class-name
;;; ---
(defun get-handler-for (object method-name)
  (if (typep object 'flavor-instance)  
    (cddr (assoc :primary 
                 (gethash method-name 
                          (flavor-methods 
                           (get (flavor-instance-class-name object) 
                                'flavor-pattern)))))
    (cddr (assoc :primary 
                 (gethash method-name 
                          (flavor-methods 
                           (get object 'flavor-pattern)))))))
;;; --> END GET-HANDLER-FOR

;;; --> INSTANCEP
;;; Is this an instance?
;;; ---
;;; INPUT: a flavor instance
;;; OUTPUT: T or nil
;;; SIDE-EFFECTS: none
;;; ---
;;; Example of call: (instancep ship1)
(defun instancep (obj)
  (typep obj 'flavor-instance))
;;; --> END INSTANCEP

;;; --> INSTANCE-TYPEP
;;; Returns or verifies the type of an instance
;;; ---
;;; INPUT: a flavor instance and, optionally, a class name
;;; OUTPUT: a class name or (T or NIL)
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: flavor-instance-class-name
;;; ---
;;; Examples of call:
;;;   (instance-typep ship1)
;;;   (instance-typep ship1 'ship)
;;; ---
(defun instance-typep (obj &optional (a-flavor-name nil))
   (if a-flavor-name
     (typep obj a-flavor-name)
     (if (typep obj 'flavor-instance)
       (flavor-instance-class-name obj))))
;;; --> END INSTANCE-TYPEP

;;; --> DESCRIBE-FLAVOR
;;; This function describes a flavor instance OR a flavor class
;;; ---
;;; INPUT: a flavor name, quoted for a class, unquoted for an instance
;;; OUTPUT: nil
;;; SIDE-EFFECTS: describes the flavor instance or class on the default output
;;;        device
;;; USER-DEFINED-CALLED: flavor-vars, flavor-depends-on, flavor-methods, 
;;;        :describe.
;;; ---
(defun describe-flavor (a-flavor-name) 
  (if (typep a-flavor-name 'flavor-instance)  
    (send a-flavor-name :describe)
    (let ((fl (get a-flavor-name 'flavor-pattern)))
      (format t "<CLASS ~s> has variables and default values:~%" a-flavor-name)
      (dolist (item (flavor-vars fl))
        (format t " ~s   ~s~%" (car item) (cdr item)))
      (format t "It directly or indirectly depends on:~%")
      (format t "~s~%" (flavor-precedence fl))
      (format t "with dependents:~%")
      (format t "~s~%" (flavor-depended-on-by fl)))))
;;; --> END DESCRIBE-FLAVOR

;;; --> SET-IN-INSTANCE
;;; This allows the setting of an IV in an instance without calling SEND
;;; It is faster.
;;; ---
;;; INPUT: a flavor instance, the name of an instance variable (NOT keyword)
;;;        and a lisp form
;;; OUTPUT: undefined
;;; SIDE-EFFECTS: sets the value of the designated Instance Variable in the
;;;        given flavor instance
;;; USER-DEFINED-CALLED: flavor-instance-vars
;;; ---
(defun set-in-instance (an-instance a-symbol a-value)
  ; (set-in-instance ship1 'a 3) <=> (send ship1 :set-a 3)
  (rplacd 
   (assoc (intern (string a-symbol) 'keyword) 
          (flavor-instance-vars an-instance))
   a-value))
;;; --> END SET-IN-INSTANCE

;;; --> SYMEVAL-IN-INSTANCE
;;; This allows to extract the value of an instance variable without using
;;; SEND. It is faster
;;; ---
;;; INPUT: a flavor instance and an IV symbol
;;; OUTPUT: the value of the IV or NIL
;;; SIDE-EFFECTS: none
;;; USER-DEFINED-CALLED: flavor-instance-vars
;;; ---
(defun symeval-in-instance (an-instance a-symbol)
  ; (symeval-in-instance ship1 'a)
  (let ((sym-val-pair (assoc (intern (string a-symbol) 'keyword) 
                             (flavor-instance-vars an-instance))))
    (if sym-val-pair
      (cdr sym-val-pair)
      (error "no variable <~s> in <~s>~%" a-symbol an-instance))))
;;; --> END SYMEVAL-IN-INSTANCE

;;; =========== Some further useful functions - self explanatory  ============
 
(defun keyword-instance-environment (an-instance)
; (keyword-instance-environment ship1)
; ((:A . 3) (:B . 2))
  (if (typep an-instance 'flavor-instance)
    (flavor-instance-vars an-instance)))

(defun instance-environment (obj)
; (instance-environment ship1)
; ((A . 3) (B . 2))
; this works on the sun allegro
  (if (typep obj 'flavor-instance)
    (let ((keyword-alist (flavor-instance-vars obj)))
      (mapcar #'(lambda (pair)
                  (cons (intern (symbol-name (car pair))) (cdr pair)))
              keyword-alist))))

(defun instance-keywords (an-instance)
; (instance-keywords ship1)
; (:A :B)
  (if (typep an-instance 'flavor-instance)
    (mapcar #'car (flavor-instance-vars an-instance))))


;;; --> FLAVOR-VARIABLES
;;; Returns the lexical Instance Variables of the class
;;; Example of call:
;;; (flavor-variables 'ship)
;;; (A B)
;;; ---
(defun flavor-variables (a-flavor)
  (flavor-known-lexical-ivs (get a-flavor 'flavor-pattern)))
;;; --> END FLAVOR-VARIABLES


;;; --> MY-FLAVOR-VARIABLES
;;; Returns the lexical Instance Variables of the class of the given INSTANCE
;;; ---
(defun my-flavor-variables (an-instance)
  (flavor-known-lexical-ivs (get (instance-typep an-instance) 'flavor-pattern)))
;;; --> END MY-FLAVOR-VARIABLES


(defun flavor-default-environment (a-flavor)
  ; (flavor-default-environment 'ship)
  ; ((:A . 3) (:B . 2))
  (flavor-vars (get a-flavor 'flavor-pattern)))

  
;;; =======================  COMPILED FILES SUPPORT  ========================
;;; This section contains the code required to produce FASL files of
;;; flavors and their methods. File compilation of a flavor and its methods
;;; involves an intermediate step where a processed Common Lisp file is
;;; created and then file-compiled to the final "FASL" file. The intermediate
;;; file can be saved or can be destroyed.

(defvar *out-file*)
(defparameter *lisp-source-extension* "lisp")
(defparameter *lisp-compiled-extension* "fasl")

;;; --> CF-MULF
;;; This function will file-compile a flavor and its associated methods so
;;; that a FASL file is generated which can be FASL-Loaded. The default is to
;;; produce a FASL file, although the user may opt to stop at the intermediate
;;; file, which is nothing but legal Common Lisp. 
;;; ---
;;; INPUT: a file-name
;;; OUTPUT: nothing usable
;;; SIDE-EFFECTS: both an intermediate and a FASL file are created
;;; USER-DEFINED-CALLED: complete-name, name-to-fl, mulf
;;; ---
(defun cf-mulf (src-name &optional (cl-compile t))
  "Compile file using MITRE U Lowell Flavors.
   src-name.cl -> src-name-cm.cl [-> src-name-cm.fasl]"
  (setq *compiling* t)                             ;for the right versions..
  (setq *all-meth-lists* nil)
  (let* ((full-src-name (complete-name src-name))  ;get filenames straight
         (target-name (name-to-fl full-src-name)))
    (format t ";mulf compiling source: ~a,~%                  target: ~a~%"
            (namestring full-src-name) (namestring target-name))
    (mulf full-src-name target-name)               ;generate CL code
    (if cl-compile                                 ;do I file compile it?
      (compile-file target-name))                  ;YES, do
    (setq *compiling* nil)                         ;back to interactive mode
    ))
;;; --> END CF-MULF

;;; --> COMPLETE-NAME
;;; This function completes the name by merging two pathnames
;;; ---
(defun complete-name (src)
  "Given a possibly incomplete source filename, add default extension (if
   necessary) and return it"
  (merge-pathnames src (concatenate 'string "name." *lisp-source-extension*)))
;;; --> END COMPLETE-NAME

;;; --> NAME-TO-FL
(defun name-to-fl (in-name)
  "Return  a filename consisting of the in-name with '-fl' appended to
   the end of its name component."
  (make-pathname :name (concatenate 'string (pathname-name in-name) "-fl")
		 :defaults in-name))
;;; --> END NAME-TO-FL

;;; --> MULF
;;; Reads from the designated source file and writes the "massaged" source code
;;; to the designated target file. Since we now have *compiling* set to T, the
;;; correct portion of code in DEFFLAVOR and DEFMETHOD will be executed
;;; ---
;;; INPUT: a source file string and a target file string
;;; OUTPUT: undefined
;;; SIDE-EFFECTS: writes to file
;;; USER-DEFINED-CALLED: write-form, attach-methods
;;; ---
(defun mulf (&optional (source "/dev/tty") (target "/dev/tty"))
  (with-open-file (in-file source :direction :input)
    (with-open-file (out-file target :direction :output :if-exists :supersede)
      (setq *out-file* out-file)      
      (do ((form (read in-file nil) (read in-file nil)))
          ((null form))
        (cond ((or (eq (car form) 'defflavor)
                   (eq (car form) 'defmethod))
               (eval form))
              (t (write-form form))))
      (dolist (item (reverse *all-meth-lists*))
        (write-form (list 'apply 
                          ''attach-methods 
                          (list 'quote (reverse (eval item)))))))))
;;; --> END MULF

;;; --> WRITE-FORM
(defun write-form (form)
  "Write a form to the output stream."
  (pprint form *out-file*)
  (finish-wr-form))
;;; --> END WRITE-FORM

;;; --> FINISH-WR-FORM
(defun finish-wr-form ()
  "Finish writing object to output stream.
   This may be more efficient without (finish-output)."
  (fresh-line *out-file*)
  (terpri *out-file*)
  (finish-output *out-file*))
;;; --> END FINISH-WR-FORM

;;; --> DEFFLAVOR2-COMPILE
;;; This is the function that prepares the flavor for compilation. Note that it
;;; creates a global name to hold the methods list specific to this flavor
;;; and save this list-pointer on the global *all-meth-lists*. It makes sure
;;; that the predicate function is saved on the *out-file*
;;; ---
;;; INPUT: flavor name, all the class variables, the immediate parents and
;;;        the keyword lists
;;; OUTPUT: the flavor name
;;; SIDE-EFFECTS: creates flavor pattern, predicate functions, methods tables
;;         output to *out-file*
;;; USER-DEFINED-CALLED: create-predicate-compile, make-flavor-pattern,
;;;        make-access-methods
;;; ---
(defun defflavor2-compile (a-flavor-name class-vars depends-on keyword-props)
  (let ((pattern nil)
        (the-meth-list (intern 
                        (concatenate 'string (string a-flavor-name) "-METHODS")))
        (spec nil))
    (setq spec (list 'special the-meth-list)) ;say it is special
    (proclaim spec)                           ;and make it known
    (set the-meth-list (list a-flavor-name))
    (push the-meth-list *all-meth-lists*)
    (create-predicate-compile a-flavor-name)
    (setq pattern 
          (make-flavor-pattern a-flavor-name class-vars 
                               depends-on keyword-props))
    (make-access-methods a-flavor-name class-vars keyword-props)
    (format t "~s~%" a-flavor-name)           ;write it out
    a-flavor-name))
;;; --> END DEFFLAVOR2-COMPILE

;;; --> CREATE-PREDICATE-COMPILE
;;; This is the same as CREATE-PREDICATE, with the exception that it will
;;; write the correct function definitions to a file rather than compiling them
;;; into the environment
;;; ---
;;; INPUT: a flavor name
;;; OUTPUT: nothing usable
;;; SIDE-EFFECTS: writes CL text to *out-file*
;;; USER-DEFINED-CALLED: f-inst-check, flavor-instance-class-name, fetch-flavors
;;; ---
(defun create-predicate-compile (a-flavor-name)
  (let ((pred-name (intern (format nil "~a-P" a-flavor-name))))
    (write-form `(defun ,pred-name (obj)
                   (and (typep obj 'flavor-instance)
                        (member ',a-flavor-name 
                                (fetch-flavors 
                                 (flavor-instance-class-name obj))))))
    (write-form `(deftype ,a-flavor-name () '(satisfies ,pred-name)))))
;;; --> END CREATE-PREDICATE-COMPILE

;;; --> DEFMETHOD2-COMPILE
;;; This function creates the function for a given method and writes the
;;; definition to *out-file*. 
;;; ---
;;; INPUT: the usual method text
;;; OUTPUT: the function textual definition
;;; SIDE-EFFECTS: method name interned, method attached to it, function
;;;        definition written to *out-file*
;;; USER-DEFINED-CALLED: write-form
;;; ---
(defun defmethod2-compile (object-key-method parameters body)  ;body now a list
  (let (key method-name)
    (cond ((= (length object-key-method) 3)
           (setq key (cadr object-key-method))
           (setq method-name (caddr object-key-method)))
          (t (setq key :primary)
             (setq method-name (cadr object-key-method))))
    (let* ((func-name 
            (intern 
             (concatenate 'string (string (car object-key-method)) "-"
                          (string key) "-"
                          (string method-name))))
           (the-func `(defun ,func-name 
                             ,(cons 'cl::self parameters)
                        ,(macroexpand `(with-instance-variables 
                                         ,(car object-key-method)
                                         cl::self
                                         ,@body))))
           (the-meth-list 
            (intern 
             (concatenate 'string 
                          (string (car object-key-method)) "-METHODS"))))
      (write-form the-func)      ;write function def out
      (set the-meth-list         ;save on name 
           (cons func-name (eval the-meth-list)))
      the-func))) 
;;; --> END DEFMETHOD2-COMPILE

;;; --> DEFFLAVOR2-LOAD
;;; This function just makes a flavor pattern at load time
;;; ---
;;; INPUT: usual stuff for flavor definition
;;; OUTPUT: nil
;;; SIDE-EFFECTS: creates flavor pattern
;;; USER-DEFINED-CALLED: make-flavor-pattern
;;; ---
(defun defflavor2-load (a-flavor-name class-vars depends-on keyword-props)
  (make-flavor-pattern a-flavor-name class-vars 
                       depends-on keyword-props)
  (format t "~s~%" a-flavor-name))
;;; --> END DEFFLAVOR2-LOAD

;;; --> ATTACH-METHODS
;;; This attaches methods to a flavor pattern. It begins by inheriting from the
;;; "inheritance graph" and the adds the methods that have been defined for this
;;; particular flavor.
;;; ---
;;; INPUT: a flavor name and a methods list.
;;; OUTPUT: the methods hash table
;;; SIDE-EFFECTS: the methods hash table is completed
;;; USER-DEFINED-CALLED: flavor-methods, merge-class-super-methods,
;;;        get-class-methods, flavcor-depends-on, adjust-dependent-methods,
;;;        flavor-precedence
;;; ---
(defun attach-methods (a-flavor-name &rest meth-list)
  (let ((pattern (get a-flavor-name 'flavor-pattern))
        (key nil)
        (method-name nil)
        (item-str nil)
        (new-method nil)
        (curr-method nil)
        (where nil))
    (setf (flavor-methods pattern)           ;the local methods hash table
          (merge-class-super-methods         ;obtained from the iheritance graph
           (get-class-methods (flavor-depends-on pattern))
           (flavor-precedence pattern)))
    (dolist (item meth-list)                 ;attach local level methods
      (setq item-str (format nil "~s" item)) ;is it a legally named method?
      (cond ((setq where (search "PRIMARY-" item-str))
             (setq where (+ 8 where))
             (setq key :primary))
            ((setq where (search "BEFORE-" item-str))
             (setq where (+ 7 where))
             (setq key :before))
            ((setq where (search "AFTER-" item-str))
             (setq where (+ 6 where))
             (setq key :after))
            (t (error "method is not of appropriate type ~s~%" item)))
      (setq method-name 
            (intern (subseq item-str where) 'keyword))
      (setq new-method (cons key (cons a-flavor-name item)))
      (setq curr-method (gethash method-name (flavor-methods pattern)))
      (cond (curr-method            ;something already there
             (setf (gethash method-name (flavor-methods pattern))
                   (adjust-methods new-method curr-method 
                                   (flavor-precedence pattern))))
            (t (setf (gethash method-name (flavor-methods pattern))
                     (list new-method))))
      (adjust-dependent-methods a-flavor-name method-name new-method))
    (flavor-methods pattern)))
;;; --> END ATTACH-METHODS

(format t "mul-flavors done~%")

