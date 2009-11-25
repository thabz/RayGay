
;; ---------------------------------------------
;; Helpers
;; ---------------------------------------------

(define (near-equal? a b)
 (cond
  ((and (list? a) (list? b))
   (let loop ((l (map near-equal? a b)))
    (cond
     ((null? l) #t)
     ((not (car l)) #f)
     (else (loop (cdr l))))))
  ((and (real? a) (real? b)) 
   (> 0.0001 (abs (- a b))))
  ((vector? a) 
   (and
    (near-equal? (vector-ref a 0) (vector-ref b 0))
    (near-equal? (vector-ref a 1) (vector-ref b 1))
    (near-equal? (vector-ref a 2) (vector-ref b 2))))))

;; ---------------------------------------------
;; The testing framework
;; ---------------------------------------------

(define tests-run 0)
(define tests-failed 0)
(define test-suite-name "")

(define (test descr result)
 (set! tests-run (+ tests-run 1))
 (if (not result)
  (begin
   (set! tests-failed (+ tests-failed 1))
   (display "FAILED ")
   (display test-suite-name)
   (display ":")
   (display descr)
   (newline))))

(define (run-test name func)
 (set! tests-run 0)
 (set! tests-failed 0)
 (set! test-suite-name name)
 (func)
 (display name)
 (display " ")
 (display (- tests-run tests-failed))
 (display "/")
 (display tests-run)
 (newline))

