(defpackage :word-ladder-system
  (:use :cl :asdf))

(in-package :hash-set-system)

(defsystem #:word-ladder
  :serial t
  :description "Problem Solution."
  :author "Samuel Chase"
  :licence "Unknown"
  :depends-on (#:hash-set
               #:alexandria
               #:optima)
  :components ((:file "package")
               (:file "word-ladder")))
               
