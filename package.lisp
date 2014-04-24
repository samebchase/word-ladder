(in-package :cl-user)

(defpackage #:word-ladder
  (:use #:cl #:hash-set)
  (:export #:generate-word-ladder
           #:valid-dictionary-wordp))
