;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LISP 3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TASK 1 MAIN FUNCTIONS
(defun leftward-of (x y)
  (if (not (check-types '(blockp blockp) x y))
    (return-from leftward-of '**ERROR**))
  (when (not (gethash `(BLOCK ,x) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" x)
  (return-from leftward-of '**ERROR**))  
  (when (not (gethash `(BLOCK ,y) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" y)
  (return-from leftward-of '**ERROR**)) 
  (if (and (on-table x) (on-table y))
  (mapcar #'(lambda (a)
            (if (eql a x)
            (return-from leftward-of t)
            (if (eql a y)
            (return-from leftward-of nil)
            )
            )
            )
  (car *LAYERS*)
  )
  (progn
  (mapcar #'(lambda (a)
    (mapcar #'(lambda (b)
      (if (leftward-of a b) (return-from leftward-of t))
    )
    (support-base y)
    )
  )
  (minus (support-base x) (my-intersection (support-base x) (support-base y)))
  )
  (mapcar #'(lambda (a)
    (mapcar #'(lambda (b)
      (if (leftward-of a b) (return-from leftward-of t))
    )
    (minus (support-base y) (my-intersection (support-base x) (support-base y)))
    )
  )
  (support-base x)  
  )
  nil
  )
  )
)

(defun rightward-of (x y)
  (if (not (check-types '(blockp blockp) x y))
    (return-from rightward-of '**ERROR**))  
  (when (not (gethash `(BLOCK ,x) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" x)
  (return-from rightward-of '**ERROR**))  
  (when (not (gethash `(BLOCK ,y) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" y)
  (return-from rightward-of '**ERROR**)) 
 (if (leftward-of x y) nil t)
)

(defun above (x y)
  (if (not (check-types '(blockp blockp) x y))
    (return-from above '**ERROR**))
  (when (not (gethash `(BLOCK ,x) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" x)
  (return-from above '**ERROR**))  
  (when (not (gethash `(BLOCK ,y) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" y)
  (return-from above '**ERROR**)) 
  (if (< (locate-layer x) (locate-layer y))
  nil
  (if (> (locate-layer x) (locate-layer y))
  t
  nil
  )
  )
)

(defun below (x y)
  (if (not (check-types '(blockp blockp) x y))
    (return-from below-of '**ERROR**))
  (when (not (gethash `(BLOCK ,x) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" x)
  (return-from below '**ERROR**))  
  (when (not (gethash `(BLOCK ,y) *FACTS-HT*))
    (format t "** The description specifies an unavailable object: ~a~%" y)
  (return-from below '**ERROR**))  
  (if (> (locate-layer x) (locate-layer y))
  nil
  (if (< (locate-layer x) (locate-layer y))
  t
  nil
  )
  )
)

;TASK 2 MAIN FUNCTION
(defun bw-agent ()


  (format t "What would you like me to build?~%")
  (setq input (read))

  (if (not (check-types '(pred-list-p) input))
  (return-from bw-agent '**ERROR**))
  (if (not (valid-des input))
  (return-from bw-agent '**ERROR**))
  
  (format t "Required blocks:~%~a~%" (find-required-blocks input))
  (setq *STRUCTURE* (particularize-description input))
  (format t "Construction steps:~%~a~%" (find-steps *STRUCTURE*))
  (setq *LAYERS* (find-layers *STRUCTURE*))
  (format t "Instantiated specification~%~a~%" *STRUCTURE*)

  (loop while t
  do (progn
  (format t "Do you have a spatial question?~%")
  (setq input (read))
  (if (eql input 'NO) (progn (format t "Goodbye!~%") (return) ) )

  (if (eql (car input) 'RIGHTWARD-OF )
    (format t "~a~%" (rightward-of (cadr input) (caddr input)))
    (if (eql (car input) 'LEFTWARD-OF)
       (format t "~a~%" (leftward-of (cadr input) (caddr input)))
      (if (eql (car input) 'ABOVE)
        (format t "~a~%" (above (cadr input) (caddr input)))
        (if (eql (car input) 'BELOW)
          (format t "~a~%" (below (cadr input) (caddr input)))
          (format t "Not a valid question.~%")
        )
      )
    )
  )
  )
  )
)

;___________________HELPER FUNCTIONS FOR LISP 3_________________________

; find the support-base of a block using recursion
(defun support-base (x)
  (flatten (recursively-find-support (list (cons x nil))))
)

;determine if a given block is on the table
(defun on-table (x)
  (if (null (find x (car *LAYERS*)))
  nil
  t)
)

;recursively find supporting blocks from top to bottom, layer by layer
(defun recursively-find-support (layers)
    (if (null (find-all-supporting-blocks (car layers)))
    (return-from recursively-find-support layers)
    (recursively-find-support (cons (find-all-supporting-blocks (car layers)) layers)  )
    )
)

;given a list of blocks, find all the blocks supporting them (in the next layer)
(defun find-all-supporting-blocks (layer)
  (setq ret nil)
  (mapcar #'(lambda (a)
            (progn
              (setq under-layer (find-all-blocks-under *STRUCTURE* a))
              (mapcar #'(lambda (b)
                (if (null (find b ret))
                (push b ret)
                )                        
              )
              under-layer
              )
            )
            )
  layer
  )
  (if (eql (car ret) '*TABLE* )
  nil
  ret
  )
)

;given a block, find the layer id where the block is located
(defun locate-layer (x)
  (setq counter 1)
  (mapcar #'(lambda (a)
  (if (null (find x a))
  (setq counter (1+ counter))
  (return-from locate-layer counter)
  )
  )
  *LAYERS*
  )
  -1
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LISP 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TASK 1 MAIN FUNCTION
(defun find-steps (description)
  ;Safety checking
  (if (not (check-types '(pred-list-p) description))
  (return-from find-steps '**ERROR**))
  (if (not (valid-des description))
  (return-from find-steps '**ERROR**))
  ;Build the base layer
  (setq ret nil)
  (setq layers-list (find-layers description))
  (setq block (find-left-most-block description (car layers-list)))
  (push (cons 'PUT-ON-TABLE (cons (find-left-most-block description (car layers-list)) nil)) ret)
  (setq layer (car layers-list))
  (setq rest-layers (cdr layers-list))
  (setq layer (remove block layer))
  (loop while (not (null layer))
    do (progn
    (setq next-b (find-next-block description block))
    (push (cons (if (right-of-pred-p next-b) 'PUT-RIGHT 'PUT-ADJACENT) (cdr next-b)  )    ret)
    (setq block (cadr next-b))
    (setq layer (remove block layer))
    )
  )
  ;Build the rest of the layers
  (setq layer (car rest-layers))
  (setq rest-layers (cdr rest-layers))
  (loop while (not (null layer))
    do(progn
    (mapcar #'(lambda (x) 
                (if (= 1 (length (find-all-blocks-under description x)))
                (push (cons 'PUT-ON (cons x (cons (car (find-all-blocks-under description x))nil))) ret)
                (push (cons 'PUT-ON-BOTH (cons x (cons (car (find-all-blocks-under description x)) (cons (cadr (find-all-blocks-under description x)) nil)))) ret)
                )
              )     
      layer
    )
    (setq layer (car rest-layers))
    (setq rest-layers (cdr rest-layers))
    )
  )    
  (reverse ret)
)

;TASK 1 HELPER FUNCTIONS
;find the block that is not on the right of anything
(defun find-left-most-block (description base-layer)
  (setq right-list (remove-if-not #'right-of-pred-p description))
  (setq right-list (append right-list (remove-if-not #'right-adjacent-pred-p description)))
  (mapcar #'(lambda (x) (setq base-layer (remove (cadr x) base-layer))) right-list)
  (car base-layer)
)
;given a block, find the block that is on its right and return the relation containing it
(defun find-next-block (description block)
  (setq ret1 nil)
  (mapcar #'(lambda (x) 
            (if (and (or (right-of-pred-p x) (right-adjacent-pred-p x)) (eql block (caddr x)))                 
            (push x ret1)
            ))
    description
  )
  (car ret1)
)
;given a block, find all blocks right under it
(defun find-all-blocks-under (description block)
  (setq ret2 nil)
  (mapcar #'(lambda (x)
            (if (and (on-pred-p x) (eql block (cadr x)))                 
            (push (caddr x) ret2)
            ))
    description
  )
  ret2
)

;;PART 2 MAIN FUNCTION
(defun build-goal-structure (description)
  (if (not (check-types '(pred-list-p) description))
  (return-from build-goal-structure '**ERROR**))
  (format t "I will use the following blocks: ~a~%" (find-required-blocks description))
  (setq des (particularize-description description))
  (find-steps des)
)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS FROM BEN'S (MODIFIED);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ben Kane

(defun check-types (types &rest args)
; `````````````````````````````````````
; A short and sweet type checking function. Takes a list of preds (e.g. '(atom listp)) in 'types',
; and evaluates it on the corresponding item in 'args'. If all preds are satisfied, return true.
; Note that if you have more complex types (e.g. "a list of exactly 3 items"), you can *define* your
; own predicate as a function, like '(defun list-3p (x) (and (listp x) (= 3 (length x))))'.
;
  (every
    (lambda (pred arg)
      (if (funcall (eval `(lambda (x) (,pred x))) arg) t
        (format t "** The following argument is not type ~a : ~a~%" pred arg)))
    types args)
) ; END check-types


; Here we define a bunch of custom predicates for type-checking.
; ````````````````````````````````````````````````````````````````
(defun variablep (x)
  (and (symbolp x) (char-equal #\? (car (coerce (string x) 'list)))))
(defun globalp (x)
  (and (symbolp x) (char-equal #\* (car (coerce (string x) 'list)))))
(defun blockp (x)
  (and (symbolp x) (not (variablep x)) (not (globalp x))))
(defun predp (x)
  (and (listp x) (every #'symbolp x) (not (variablep (car x)))
       (every (lambda (s) (not (equal s (car x)))) (cdr x))))
(defun unary-pred-p (x)
  (and (predp x) (= 2 (length x))))
(defun binary-pred-p (x)
  (and (predp x) (= 3 (length x))))
(defun unary-description-p (x)
  (and (unary-pred-p x) (variablep (second x))))
(defun pred-list-p (x)
  (and (listp x) (every #'predp x)))
(defun pred-ground-p (x)
  (and (predp x) (not (some #'variablep x))))
(defun pred-ground-list-p (x)
  (and (listp x) (every #'pred-ground-p x)))
(defun var-assignment-p (x)
  (and (listp x) (equal 2 (length x)) (variablep (first x)) (blockp (second x))))
(defun on-pred-p (x)
  (and (binary-pred-p x) (eql 'ON (car x)))
)
(defun right-of-pred-p (x)
  (and (binary-pred-p x) (eql 'RIGHT-OF (car x)))
)
(defun right-adjacent-pred-p (x)
  (and (binary-pred-p x) (eql 'RIGHT-ADJACENT (car x)))
)
(defun valid-des (x)
  (setq right-list (remove-if-not #'right-of-pred-p x))
  (setq right-list (append right-list (remove-if-not #'right-adjacent-pred-p x)))
  (setq base-layer (car (find-layers x)))
  (mapcar #'(lambda (x) (setq base-layer (remove (cadr x) base-layer))) right-list)
  (if (not (= 1 (length base-layer)))
    (return-from valid-des nil))
  (setq base-layer (car (find-layers x)))
  (mapcar #'(lambda (x) (if 
  (not (and (member (cadr x) base-layer) (member (caddr x) base-layer) )) 
   (format t "Warning: Relation ~a will be ignored.~%" x)
  )) right-list)
  t
)
(defun list-or-atom-p (x)
  (listp x)
)

(defun store-initial-facts ()
; `````````````````````````````
; Stores an initial set of facts in *FACTS-HT* using the *BLOCKS* list.
;
  (store-facts *BLOCKS* *FACTS-HT*)
  (store-facts (mapcar (lambda (b) `(BLOCK ,(second b))) *BLOCKS*) *FACTS-HT*)
) ; END store-initial-facts


(defun find-required-blocks (description)
; `````````````````````````````````````````
; Takes a structural description as an argument, and returns a list of blocks (e.g. B1) and
; variable assignments (e.g. (?x B2)) satisfying the description, or an error if the description
; cannot be satisfied with the blocks in *BLOCKS*.
;
  ; Check types
  (if (not (check-types '(pred-list-p) description))
    (return-from find-required-blocks '**ERROR**))

  (let (result assigned-blocks candidate-block)
    ; Retrieve all blocks explicitly mentioned in the description (or an error if a name is unavailable)
    (mapcar (lambda (block)
        (when (not (gethash `(BLOCK ,block) *FACTS-HT*))
          (format t "** The description specifies an unavailable object: ~a~%" block)
          (return-from find-required-blocks '**ERROR**))
        (setq assigned-blocks (append assigned-blocks (list block))))
      (get-unique-blocks description))
    (setq result assigned-blocks)

    ; Go through all unary descriptions (first making sure every variable in the description
    ; has a unary description, by adding (BLOCK ...) if a variable does not have a color specified)
    (mapcar (lambda (fact)
        (setq candidate-block
          (second (car (remove-assigned assigned-blocks (gethash (car fact) *FACTS-HT*)))))
        (when (not candidate-block)
          (format t "** Not enough blocks available satisfying the following description: ~a~%" fact)
          (return-from find-required-blocks '**ERROR**))
        (setq assigned-blocks (append assigned-blocks (list candidate-block)))
        (setq result (append result (list `(,(second fact) ,candidate-block)))))
      (remove-if-not #'unary-description-p (add-implicit-block-predicates description)))
    result)
) ; END find-required-blocks


(defun particularize-description (description)
; ``````````````````````````````````````````````
; Using find-required-blocks, get a set of variable assignments satisfying the description, and particularize
; the given description by replacing those variables with the assigned blocks.
;
  ; Check types
  (if (not (check-types '(pred-list-p) description))
    (return-from particularize-description '**ERROR**))
  
  ; Get the list of required blocks, or return **ERROR** if that function fails
  (let ((required-blocks (find-required-blocks description)))
    (when (equal required-blocks '**ERROR**)
      (return-from particularize-description '**ERROR**))
    ; Get all required blocks that involve variable assignments (e.g. (?x B1)). For each variable assignment,
    ; substitute the block for the variable in the current result, starting with the original description.
    (reduce (lambda (result var-assignment) (subst (second var-assignment) (first var-assignment) result))
      (remove-if-not #'var-assignment-p required-blocks)
      :initial-value description))
) ; END particularize-description


(defun find-layers (description)
; ````````````````````````````````
; Takes in a structural description and finds what blocks should appear at each level. Recursively constructs
; a list of blocks at each level, starting from *TABLE*. After the final list of levels is created, we can do
; a few checks on the sub-lists to catch any of the errors that the prompt mentions.
;
  ; Check types
  (if (not (check-types '(pred-list-p) description))
    (return-from find-layers '**ERROR**))

  ; Get all unique blocks and vars in the description (for later error checking),
  ; and a list of only the on-predicates in the description
  (let ((blocks-and-vars (get-unique-blocks-and-vars description))
        (on-description (remove-if-not #'on-pred-p description))
        used-descriptions result)
    ; Recursive function to find the current layer, given the previous layer
    (labels ((find-layers-recur (desc prev-layer)
        ; Get new layer by removing all on-predicates where the third element isn't in
        ; the previous layer, and retrieving all the second elements from the remaining list
        ; NOTE: to avoid recursion spiralling out of control with bad definitions, we also make sure each
        ; definition is only used once, by maintaining a used-descriptions list outside the scope of recursion
        (let* ((layer-preds (remove-if-not (lambda (on-pred)
                (and (member (third on-pred) prev-layer)
                     (not (member on-pred used-descriptions :test #'equal)))) on-description)) 
              (new-layer (remove-duplicates (mapcar #'second layer-preds))))
          ; Update used-descriptions list
          (setq used-descriptions (append used-descriptions layer-preds))
          ; If the new layer is non-nil, append it to the result list, otherwise return nil
          (if new-layer
            (append (list new-layer) (find-layers-recur desc new-layer))
            nil))))
      ; Begin recursion with *TABLE* as the first layer
      (setq result (find-layers-recur description '(*TABLE*)))
      
      ; If some block is at multiple levels (i.e. the intersection of all the levels
      ; is non-nil), return an error
      (when (intersection-any result)
        (format t "** Some block is specified at 2 different levels: ~a~%"
          (car (intersection-any result)))
        (return-from find-layers '**ERROR**))

      ; If some block is not on any block (i.e. the unique blocks and vars in the description
      ; do not all occur in the result), return an error
      (when (mutual-set-difference (flatten result) blocks-and-vars)
        (format t "** Some block is not specified to be on anything: ~a~%"
          (car (mutual-set-difference (flatten result) blocks-and-vars)))
        (return-from find-layers '**ERROR**))
      
      result))
) ; END find-layers


(defun get-unique-blocks (description)
; `````````````````````````````````````
; Gets all unique blocks (for now, we assume that any symbols which aren't variables
; or globals are block names) in a description.
; e.g. (get-unique-blocks '((red B1) (on B1 ?x) (blue ?x) (on B2 *TABLE*))) => '(B1 B2)
;
  (let (blocks)
    (mapcar (lambda (fact)
        (setq blocks (append blocks (remove-if-not #'blockp (cdr fact)))))
      description)
    (remove-duplicates blocks))
) ; END get-unique-blocks


(defun get-unique-variables (description)
; `````````````````````````````````````````
; Gets all unique variables, in a similar way to get-unique-blocks.
;
  (let (variables)
    (mapcar (lambda (fact)
        (setq variables (append variables (remove-if-not #'variablep (cdr fact)))))
      description)
    (remove-duplicates variables))
) ; END get-unique-variables


(defun get-unique-blocks-and-vars (description)
; ``````````````````````````````````````````````
; Gets all unique blocks and variables from a description.
;
  (append (get-unique-blocks description) (get-unique-variables description))
) ; END get-unique-blocks-and-vars


(defun remove-assigned (assigned facts)
; ```````````````````````````````````````
; Removes assigned symbols (in list 'assigned') from a list of facts.
; e.g. (remove-assigned '(B1 B2) '((red B1) (red B2) (red B3))) => '((red B3))
;
  (remove-if (lambda (fact) (some (lambda (x) (member x fact)) assigned)) facts)
) ; END remove-assigned


(defun add-implicit-block-predicates (description)
; ``````````````````````````````````````````````````
; If a predicate in a description contains a variable with no color description,
; we want to add an implied (BLOCK ?x) predicate to the block description, so we
; can treat it the same as if a color description was given when finding candidates.
; e.g. (add-implicit-block-predicates '((RED ?x) (ON ?x ?z))) => '((BLOCK ?z) (RED ?x) (ON ?x ?z))
  (let (preds-to-add vars vars-with-desc)
    ; Get a list of all variables, and a list of all variables which have a unary description
    (mapcar (lambda (fact)
        (setq vars (append vars (remove-if-not #'variablep (cdr fact))))
        (if (and (unary-pred-p fact) (variablep (second fact)))
          (setq vars-with-desc (cons (second fact) vars-with-desc))))
      description)
    ; Get the variables without a description using set difference, and add (BLOCK ...) for each
    (append description (mapcar (lambda (x) `(BLOCK ,x))
      (remove-duplicates (set-difference vars vars-with-desc)))))
) ; END add-implicit-block-predicates


(defun flatten (lst)
; ````````````````````````
; Flattens an arbitrary list using mapcar. Note that we can do this fairly easily
; using recursion: use mapcar to flatten each sublist (or if an atom is reached, create a
; list consisting of that atom). Then just append all of the flattened sublists together.
;
  ; Check types
  (if (not (check-types '(listp) lst))
    (return-from flatten '**ERROR**))

  ; Recursively flatten list
  (labels
    ((flatten-recur (p)
      (if (atom p)
        (list p)
        (apply #'append (mapcar #'flatten-recur p)))))
    (flatten-recur lst))
) ; END flatten


(defun intersection-any (lst)
; ````````````````````````````
; Gets any intersection between any two sublists of lst, or returns nil if there is no intersection.
;
  (labels
    ((intersection-any-recur (l1 l2 i j)
      (cond
        ((null l1) nil)
        ((null l2) (intersection-any-recur (cdr l1) lst (1+ i) 0))
        ((= i j) (intersection-any-recur l1 (cdr l2) i (1+ j)))
        (t (let ((intersect (intersection (car l1) (car l2))))
          (if intersect intersect
            (intersection-any-recur l1 (cdr l2) i (1+ j))))))))
    (intersection-any-recur lst lst 0 0))
) ; END intersection-any


(defun mutual-set-difference (l1 l2)
; ````````````````````````````````````
; Mutual set difference, used to determine if two lists have exactly the same elements.
;
  (or (set-difference l1 l2)
      (set-difference l2 l1))
) ; END mutual-set-difference


(defun store-fact (fact ht)
; ```````````````````````````
; Stores a fact in hash table ht, hashing on both the predicate (in which case we cons it to the existing facts under
; that predicate), as well as on the whole predication (in which case we simply store t).
;
  ; Check types
  (if (not (check-types '(pred-ground-p hash-table-p) fact ht))
    (return-from store-fact '**ERROR**))

  ; Hash on the predicate and the full predication
  (setf (gethash (car fact) ht) (append (gethash (car fact) ht) (list fact)))
  (setf (gethash fact ht) t)
) ; END store-fact


(defun store-facts (facts ht)
; `````````````````````````````
; Stores a list of facts in hash table ht. Very easy with mapcar!
;
  ; Check types
  (if (not (check-types '(pred-ground-list-p hash-table-p) facts ht))
    (return-from store-facts '**ERROR**))

  (mapcar #'(lambda (fact) (store-fact fact ht)) facts)
) ; END store-facts

(defun my-intersection (l1 l2)
; ``````````````````````````````
; Finds top-level intersection of two lists (or symbols, in which case we do a separate check
; to see if the arguments are equal). To do this, we can use nested mapcars to combine all possible
; pairs of top-level elements between the two lists (note the use of apply #'append - without this the
; nested mapcars would form a 2D matrix, but in this case we just want a flat list). If a pair is equivalent
; include the element in the resulting list, otherwise the pair becomes nil. Then we remove all nil values from
; the final list.
;
  ; Check types
  (if (not (check-types '(list-or-atom-p list-or-atom-p) l1 l2))
    (return-from my-intersection '**ERROR**))

  ; Find intersection of top-level elements
  (cond
    ((or (atom l1) (atom l2)) (if (equal l1 l2) l1))
    (t (remove-duplicates (remove nil
      (apply #'append
        (mapcar (lambda (x)
          (mapcar (lambda (y)
            (if (equalp x y) x nil))
          l2))
        l1))) :test #'equal)))
) ; END my-intersection

;find l1 minus l2, i.e. return all elements that is in l1 but not in l2
(defun minus (l1 l2)
  (if (not (check-types '(list-or-atom-p list-or-atom-p) l1 l2))
    (return-from my-intersection '**ERROR**))
  (my-intersection (set-difference l1 l2) l1)
)







