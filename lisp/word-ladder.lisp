#|

Problem from http://www.problemotd.com/problem/word-ladder/

Load all words of same length into a graph, and add an edge between
every word and it's 1-neighbour.

The word ladder is the shortest path between two words.

|#

(in-package :word-ladder)

(define-constant +alphabet+
    "abcdefghijklmnopqrstuvwxyz"
  :test #'equalp)

(define-constant +dictionary+
    (with-open-file (stream #P"wordsEn.txt") 
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
    (loop for char across downcased-word
       for char-idx below (length downcased-word)
       do (loop for alphabet-char across (remove char +alphabet+)
             do (let ((insertee-string (copy-seq downcased-word)))
                  (setf (aref insertee-string char-idx) alphabet-char)
                  (hs-insert strings insertee-string))))
    strings))

(defun word-ladder (word-a word-b)
  (if (/= (length word-a) (length word-b))
      (error "Words are of different length.")
      (let* ((graph (populate (make-instance 'graph)))
             (word-set (hs-filter (lambda (x) (= (length word-a) (length x))) +dictionary+)))
        (dohashset (word word-set)
          (dohashset (neighbour (word-neighbours word))
            (add-edge graph (list (symbolicate word) (symbolicate neighbour)) 1)))
        (shortest-path graph (symbolicate word-a) (symbolicate word-b)))))

(defun generate-word-neighbours-graph (word)
  (let* ((neighbours (hs-to-list (word-neighbours word)))
         (nodes (append (loop for neighbour in neighbours
                             collect (symbolicate neighbour))
                        (list (symbolicate word))))
         (edges-w-values (loop for neighbour in neighbours
                            collect (cons (list (symbolicate word) (symbolicate neighbour)) 2))))
    (populate (make-instance 'graph)
              :nodes nodes
              :edges-w-values edges-w-values)))

(defun generate-word-ladder-graph (word-a word-b)
  (let* ((edges (word-ladder word-a word-b))
         (edges-w-values (loop for edge in edges
                            collect (cons edge 2)))
         (nodes (remove-duplicates (flatten edges))))
    (populate (make-instance 'graph)
              :nodes nodes
              :edges-w-values edges-w-values)))

(defun visualise-graph (graph pathspec)
  (with-output-to-file (stream pathspec
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (to-html graph :stream stream)))

(defun visualise-word-ladder (word-a word-b pathspec)
  (visualise-graph (generate-word-ladder-graph word-a word-b) pathspec))

(defun visualise-word-neighbours (word pathspec)
  (visualise-graph (generate-word-neighbours-graph word) pathspec))
