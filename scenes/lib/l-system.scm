
(define (l-system-expand axiom depth)
  (if (zero? depth) 
    axiom
    (l-system-expand 
      (let iter 
	((item (car axiom))
	 (rest (cdr axiom)))
	(append 
	  (cond
	    ((eq? item 'a) '(a b))
	    ((eq? item 'b) '(b x a b x c))
	    (else (list item)))
	  (if (not (null? rest))
	    (iter (car rest) (cdr rest))
	    '())))
      (- depth 1))))
