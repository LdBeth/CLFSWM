(in-package #:flavors)

(defun parse-body (body)
  #+asdf (uiop:parse-body body :documentation nil)
  #-asdf (error "Don't have UIOP"))
