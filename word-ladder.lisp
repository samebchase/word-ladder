;;; http://www.problemotd.com/problem/word-ladder/

(in-package :word-ladder)

(define-constant +alphabet+
    "abcdefghijklmnopqrstuvwxyz"
  :test #'equalp)

(define-constant +dictionary+
    (with-open-file (stream #P "wordsEn.txt") 
      (let ((dictionary (make-instance 'hash-set)))
        (loop for line = (read-line stream nil)
           until (eq line nil)
           do (hs-insert dictionary (string-right-trim '(#\Return) line)))
        dictionary))
  :test #'hs-equal)

(defparameter *dictionary-graph* (populate (make-instance 'graph)))

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
       do (loop for alphabet-char across +alphabet+
             when (char-not-equal alphabet-char char)
             do (let ((insertee-string (copy-seq downcased-word)))
                  (setf (aref insertee-string char-idx) alphabet-char)
                  (hs-insert strings insertee-string))))
    strings))

(defun neighbours-from-set (hash-set)
  (let ((result (make-instance 'hash-set)))
    (dohashset (elt hash-set)
      (dohashset (i (word-neighbours elt))
        (hs-insert result i)))
    result))

(defun load-nodes-into-graph ()
  (dohashset (word +dictionary+)
    (add-node *dictionary-graph* (symbolicate word))))

(defun load-edges-into-graph ()
  (dohashset (word +dictionary+)
    (dohashset (neighbour (word-neighbours word))
      (add-edge *dictionary-graph* (list (symbolicate word) (symbolicate neighbour)) 1))))

(defun load-dictionary ()
  (format t "Loading nodes...~%")
  (load-nodes-into-graph)
  (format t "Finished loading nodes.~%")
  (format t "Loading edges...~%")
  (load-edges-into-graph)
  (format t "Finished loading edges.~%"))

(defun word-ladder (word-a word-b)
  (shortest-path *dictionary-graph* (symbolicate word-a) (symbolicate word-b)))

(defun generate-word-ladder-graph (word-a word-b)
  (let* ((edges (word-ladder word-a word-b))
         (edges-w-values (loop for edge in edges
                            collect (cons edge 2)))
         (nodes (remove-duplicates (flatten edges))))
    (populate (make-instance 'graph)
              :nodes nodes
              :edges-w-values edges-w-values)))

(defun generate-html-visualisation (word-a word-b pathspec)
  (with-output-to-file (stream pathspec
                               :if-exists :supersede
                               :if-does-not-exist :create)
               (to-html (generate-word-ladder-graph word-a word-b) :stream stream)))

(when (= 0 (length (graph:nodes *dictionary-graph*)))
    (load-dictionary))
