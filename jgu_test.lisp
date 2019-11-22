
(load "jgu_src.lisp")

(defparameter *BLOCKS* '((BLOCK A) (BLOCK B) (BLOCK C) (BLOCK D) (BLOCK E) (BLOCK F)))
(defparameter *FACTS-HT* (make-hash-table :test #'equal))



(store-initial-facts)

(defparameter *STRUCTURE* nil)
(defparameter *LAYERS* nil)

(print (bw-agent))