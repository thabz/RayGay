
(define (v+ a b)
  "Add two vectors"        
  (vector (+ (vector-ref a 0) (vector-ref b 0))
          (+ (vector-ref a 1) (vector-ref b 1))
          (+ (vector-ref a 2) (vector-ref b 2))))
          
(define (v- a b)
  "Subtract b from a"        
  (vector (- (vector-ref a 0) (vector-ref b 0))
          (- (vector-ref a 1) (vector-ref b 1))
          (- (vector-ref a 2) (vector-ref b 2))))          
          
(define (vscale v s)
  "Scale a vector"        
  (vector (* (vector-ref v 0) s)
          (* (vector-ref v 1) s)
          (* (vector-ref v 2) s)))                    
          
(define (vdot a b)
  "Dotproduct between two vectors"        
  (+ (* (vector-ref a 0) (vector-ref b 0))
     (* (vector-ref a 1) (vector-ref b 1))
     (* (vector-ref a 2) (vector-ref b 2))))

(define (vlength v)
  "Length of a vector"        
  (sqrt (vdot v v)))        

(define (vnormalize v)
  "Normalize a vector"        
  (let ((l (vlength v)))        
    (vector (/ (vector-ref v 0) l)
            (/ (vector-ref v 1) l)
            (/ (vector-ref v 2) l))))
