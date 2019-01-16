(in-package #:common-lisp-user)

(defpackage mul-flavors
  (:nicknames #:flavors)
  (:use #:common-lisp)
  (:shadow #:defmethod #:make-instance)
  (:export #:defflavor #:defmethod #:flavor #:flavor-instance #:send
           #:find-flavor
           #:make-instance #:instantiate-flavor #:cf-mulf #:*out-file*
           #:*all-flavor-names* #:lexpr-send #:handles-p #:get-handler-for #:instancep
           #:instance-typep #:describe-flavor #:set-in-instance #:symeval-in-instance
           #:defwhopper #:continue-whopper #:lexpr-continue-whopper
           ;; --- Extra Functions #:keyword-instance-environment
           #:instance-environment #:instance-keywords #:flavor-variables
           #:my-flavor-variables #:flavor-default-environment
           #:vanilla #:self
           ))
