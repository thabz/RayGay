
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

