
;; ---------------------------------------------
;; Helpers
;; ---------------------------------------------

(define (near-equal? a b)
 (cond
  ((and (null? a) (null? b)) #t)
  ((and (list? a) (list? b))
   (and 
    (= (length a) (length b))
    (near-equal? (car a) (car b))
    (near-equal? (cdr a) (cdr b))))
  ((and (real? a) (real? b)) 
   (> 0.0001 (abs (- a b))))
  ((and (vector? a) (vector? b))
   (near-equal? (vector->list a) (vector->list b)))
  (else #f)))

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

