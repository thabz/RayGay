;; Performs a L-system expansion given an axiom, a set of production rules
;; and a wanted generation.
;;
;; EXAMPLE L-SYSTEM
;;    Axiom: a b c
;;    Rules: a -> a b
;;           b -> b a
;; RESULT
;;    Generation 1: a b b a c
;;    Generation 2: a b b a b a a b c
;;    Generation 3: a b b a b a a b b a a b a b b a c 
;;
;; The above rules should be formulated in Scheme as an association list:
;;
;; (define rules
;;   '((a . (a b))
;;    (b . (b a))))
;;
;; The the expansion of the axiom (a b c) at generation 3 is done as
;; 
;; (l-system-expand '(a b c) rules 3)
;; 
(define (l-system-expand axiom rules generation)
  (if (zero? generation) 
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
      rules (- generation 1))))

(define-macro (pop stack)
  (let ((result (car stack)))
    (set! stack (cdr stack))
    result))

(define-macro (push stack element)
  (set! stack (cons element stack)))

(define (make-l-system-object angle length thickness axiom rules depth material)
  (let ((job (l-system-expand axiom rules depth))
	(stack '()))
    (display job)
    (newline)
    (let iter
      ((item (car job))
       (rest (cdr job))
       (up '(0 1 0))
       (right '(1 0 0))
       (position '(0 0 0)))
      (append
	(let ((thing '()))
	  (display item)
	  (cond ((eqv? item 'F) 
		 (let ((end-position (v+ position (vscale up length))))
		   (display "!")
		   (set! thing (list (make-cylinder position end-position
						    thickness material)))
		   (set! position end-position)))
		((eqv? item '+) 
		 (set! up (rotate up '(0 0 1) angle)))
		((eqv? item '-) 
		 (set! up (rotate up '(0 0 1) (- angle))))
		((eqv? item '[)
		 (begin
		   (set! stack (cons (list position up right) stack))))
		((eqv? item '])
		 (let ((item (car stack)))
		   (set! stack (cdr stack))
		   (set! position (car item))
		   (set! up (car (cdr item)))
		   (set! right (car (cdr (cdr item)))))))
	  (display thing)
	  thing)
	(if (not (null? rest))
	  (iter (car rest) (cdr rest) up right position)
	  '())))))



