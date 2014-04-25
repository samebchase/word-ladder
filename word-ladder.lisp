;;; http://www.problemotd.com/problem/word-ladder/

(in-package :word-ladder)

(alexandria:define-constant +alphabet+
    "abcdefghijklmnopqrstuvwxyz"
  :test #'equalp)

(alexandria:define-constant +dictionary+
    (with-open-file (stream #P "wordsEn.txt") 
      (let ((dictionary (make-instance 'hash-set)))
        (loop for line = (read-line stream nil)
           until (eq line nil)
           do (hs-insert dictionary (string-right-trim '(#\Return) line)))
        dictionary))
  :test #'hs-equal)

(defun valid-dictionary-wordp (word)
  (first (hs-memberp +dictionary+ word)))

(defun word-neighbours (word)
  (let ((strings (strings-one-char-change word)))
    (dohashset (string strings)
      (unless (valid-dictionary-wordp string)
        (hs-delete strings string)))
    strings))

(defun strings-one-char-change (word)
  (let ((strings (make-instance 'hash-set))
        (downcased-word (string-downcase word)))
    (loop
       for char across downcased-word
       for char-idx below (length downcased-word)
       do (loop
             for alphabet-char across +alphabet+
             when (char-not-equal alphabet-char char)
             do (let ((insertee-string (copy-seq downcased-word)))
                  (setf (aref insertee-string char-idx) alphabet-char)
                  (hs-insert strings insertee-string))))
    strings))

(defun all-neighbours-from-set (hash-set)
  (let ((result (make-instance 'hash-set)))
    (dohashset (elt hash-set)
      (dohashset (i (word-neighbours elt))
        (hs-insert result i)))
    result))

(defun common-neighbours (word-a word-b)
  (let* ((word-a-neighbours (word-neighbours word-a))
         (word-b-neighbours (word-neighbours word-b)))
    (hs-intersection word-a-neighbours word-b-neighbours)))

