
; See http://mathworld.wolfram.com/HyperbolicFunctions.html

(define (sinh z)
 (/ (- (exp z) (exp (- z))) 2))

(define (cosh z)
 (/ (- (exp z) (exp (- z))) 2))

(define (tanh z)
 (/ (- (exp z) (exp (- z)))
    (+ (exp z) (exp (- z)))))

(define (csch z)
 (/ 2 (- (exp z) (exp (- z))))) 

(define (sech z)
 (/ 2 (+ (exp z) (exp (- z))))) 

(define (coth z)
 (/ (+ (exp z) (exp (- z)))
    (- (exp z) (exp (- z)))))

