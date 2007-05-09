; Stuff for benchmarking schemes
;
; 2007-05-09:
; guile ../../../scenes/bench.scm  0.58s user 0.03s system 99% cpu 0.613 total
; ./repl ../../../scenes/bench.scm  19.45s user 0.02s system 99% cpu 19.571 total

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

;(define cc (compile (+ sum (iso-func1 1.1 2.1 3.01))))
(define cc '(+ sum (iso-func1 1.1 2.1 3.01)))

(define (bench)
  (begin
  (do ((i 0 (+ 1 i)))
      ((= i 100000))
      (set! sum (+ sum (iso-func1 1.1 2.1 3.01))))
  (display sum)
  (newline)))

(bench)

