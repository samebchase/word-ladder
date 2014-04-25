(defpackage :word-ladder-system
  (:use :cl :asdf))

(in-package :word-ladder-system)

(defsystem #:word-ladder
  :serial t
  :description "Problem Solution."
  :author "Samuel Chase"
  :licence "Unknown"
  :depends-on (#:hash-set
               #:alexandria
               #:optima
               #:graph
               #:graph-json)
  :components ((:file "package")
               (:file "word-ladder")))
               
