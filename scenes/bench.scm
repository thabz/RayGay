
; Stuff for benchmarking guile against mzscheme

(define (square x)
  (* x x))

(define (iso-chair k a b x y z) 
  (- (square (+ (square x)
                (square y)
                (square z)
             (- (* a k k))))
     (* b
        (- (square (- z k))
           (* 2 x x))
        (- (square (+ z k))
           (* 2 y y )))))

(define (iso-func1 x y z) 
  (iso-chair 5 0.95 0.8 x y z))

(define sum 1.0)    

(define cc (compile (+ sum (iso-func1 1.1 2.1 3.01))))
(define cc '(+ sum (iso-func1 1.1 2.1 3.01)))

(define (bench)
  (begin
  (do ((i 0 (+ 1 i)))
      ((= i 1000000))
      (set! sum (eval cc)))
  (display sum)
  (newline)))



