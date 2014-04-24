;;; http://www.problemotd.com/problem/word-ladder/
;;; WIP

(ql:quickload '(hash-set alexandria))

(alexandria:define-constant +dictionary+
    (with-open-file (stream #P "/home/samuel/code/exercises/problemotd/word-ladder/store/wordsEn.txt") 
      (let ((dictionary (make-instance 'hash-set:hash-set)))
        (loop for line = (read-line stream nil)
           until (eq line nil)
           do (hash-set:hs-insert dictionary (string-right-trim '(#\Return) line)))
        dictionary))
  :test #'hash-set:hs-equal)

(defun valid-dictionary-wordp (word)
  (first (hash-set:hs-memberp +dictionary+ word)))
