
;
; A collection of utility functions for handling 3D-vectors
;

(define x-axis #(1 0 0))
(define y-axis #(0 1 0))
(define z-axis #(0 0 1))

(define (.x v) (vector-ref v 0))
(define (.y v) (vector-ref v 1))
(define (.z v) (vector-ref v 2))

(define (v+ a b)
  "Add two vectors"        
  (vector (+ (.x a) (.x b))
          (+ (.y a) (.y b))
          (+ (.z a) (.z b))))
          
(define (v- a b)
  "Subtract b from a"        
  (vector (- (.x a) (.x b))
          (- (.y a) (.y b))
          (- (.z a) (.z b))))          
          
(define (vscale v s)
  "Scale a vector. v is a vector and s a scalar - or vice versa."
  (if (vector? v)
    (vector (* (.x v) s)
            (* (.y v) s)
            (* (.z v) s))
    (vector (* (.x s) v)
            (* (.y s) v)
            (* (.z s) v))))
                                

(define v* vscale)
          
(define (vdot a b)
  "Dotproduct between two vectors"        
  (+ (* (.x a) (.x b))
     (* (.y a) (.y b))
     (* (.z a) (.z b))))

(define (vlength v)
  "Length of a vector"        
  (sqrt (vdot v v)))        

(define (vdist v1 v2)
 "Distance between two vectors"
 (vlength (v- v1 v2)))

(define (vcrossproduct b c)
 "Crossproduct of two vectors"
 (vector
  (- (* (.y b) (.z c)) (* (.z b) (.y c)))
  (- (* (.z b) (.x c)) (* (.x b) (.z c)))
  (- (* (.x b) (.y c)) (* (.y b) (.x c))))) 

(define (vnormalize v)
  "Normalize a vector"        
  (let ((l (vlength v)))        
    (vector (/ (.x v) l)
            (/ (.y v) l)
            (/ (.z v) l))))

(define (vlerp a b t)
  "Calculates a point between two points at a specific increment. The t parameter is the 
   amount to interpolate between the two points where 0.0 equal to the first point, 0.1 is 
   very near the first point, 0.5 is half-way in between, etc. The lerp function is convenient 
   for creating motion along a straight path and for drawing dotted lines."
  (v+ a (vscale (v- b a) t)))


(define identity-matrix #(#(1 0 0 0) #(0 1 0 0) #(0 0 1 0) #(0 0 0 1)))

(define (vector-of-four-numbers? v)
 (and
  (= 4 (vector-length v))
  (number? (vector-ref v 0)) 
  (number? (vector-ref v 1)) 
  (number? (vector-ref v 2)) 
  (number? (vector-ref v 3))))

(define (vector-of-three-numbers? v)
 (and
  (= 3 (vector-length v))
  (number? (vector-ref v 0)) 
  (number? (vector-ref v 1)) 
  (number? (vector-ref v 2)))) 

(define (matrix? m)
 (and
  (= 4 (vector-length m))
  (vector-of-four-numbers? (vector-ref m 0))
  (vector-of-four-numbers? (vector-ref m 1))
  (vector-of-four-numbers? (vector-ref m 2))
  (vector-of-four-numbers? (vector-ref m 3))))

(define (*internal*m*v m v)
 "Internal matrix-vector multiplication"
 (let loop ((prod (make-vector 4 #f))
            (r 0))
  (if (= r 4)
   (vector
    (/ (vector-ref prod 0) (vector-ref prod 3))
    (/ (vector-ref prod 1) (vector-ref prod 3))
    (/ (vector-ref prod 2) (vector-ref prod 3)))
   (begin
    (vector-set! prod r
     (+ (* (vector-ref v 0)
	   (vector-ref (vector-ref m 0) r))
        (* (vector-ref v 1)
	   (vector-ref (vector-ref m 1) r))
        (* (vector-ref v 2)
	   (vector-ref (vector-ref m 2) r))
	(vector-ref (vector-ref m 3) r)))
    (loop prod (+ r 1))))))

(define (m* arg1 arg2)
 "Flexible matrix multiplication. Handles M*M, v*M and M*v."
 (let ((arg1-matrix? (matrix? arg1))
       (arg2-matrix? (matrix? arg2)))
  (cond 
   ((and arg1-matrix? arg2-matrix?)
    ; Matrix mult
    (let loop ((m (vector (make-vector 4 #f)
		          (make-vector 4 #f)
		          (make-vector 4 #f)
		          (make-vector 4 #f)))
	       (c 0)
	       (r 0))
     (if (= 4 c) m
      (begin
      (vector-set! (vector-ref m c) r
        (+ 
 	 (* (vector-ref (vector-ref arg1 c) 0)
 	    (vector-ref (vector-ref arg2 0) r))
	 (* (vector-ref (vector-ref arg1 c) 1)
	    (vector-ref (vector-ref arg2 1) r))
	 (* (vector-ref (vector-ref arg1 c) 2)
	    (vector-ref (vector-ref arg2 2) r))
	 (* (vector-ref (vector-ref arg1 c) 3)
	    (vector-ref (vector-ref arg2 3) r))))
      (if (= r 3)
       (loop m (+ c 1) 0)
       (loop m c (+ r 1)))))))
   ((and arg1-matrix?
         (vector-of-three-numbers? arg2))
     ; M * v
     (*internal*m*v arg1 arg2))
   ((and (vector-of-three-numbers? arg1)
         arg2-matrix?)
     (*internal*m*v arg2 arg1))
   ((and (number? arg1)
         arg2-matrix? arg2)
     #f ; r * M
   )
   ((and arg1-matrix?
         (number? arg2))
     #f ; M * r 
   )
   (else
    (error!)))))




