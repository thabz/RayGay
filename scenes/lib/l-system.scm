;; Performs a L-system expansion given an axiom, a set of rules
;; and a recursion depth.
;;
;; EXAMPLE L-SYSTEM
;;    Axiom: a b c
;;    Rules: a -> a b
;;           b -> b a
;; RESULT
;;    Depth 1: a b b a c
;;    Depth 2: a b b a b a a b c
;;    Depth 3: a b b a b a a b b a a b a b b a c 
;;
;; The above rules should be formulated in Scheme as an association list:
;;
;; (define rules
;;   '((a . (a b))
;;    (b . (b a))))
;;
;; The the expansion of the axiom (a b c) at depth 3 is done as
;; 
;; (l-system-expand '(a b c) rules 3)
;; 
(define (l-system-expand axiom rules depth)
  (if (zero? depth) 
    axiom
    (l-system-expand 
      (let iter 
	((item (car axiom))
	 (rest (cdr axiom)))
	(append 
	  (if (assq-ref rules item)
	    (assq-ref rules item)
	    (list item))
	  (if (not (null? rest))
	    (iter (car rest) (cdr rest))
	    '())))
      rules (- depth 1))))
