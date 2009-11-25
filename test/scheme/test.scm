
(define tests-run 0)
(define tests-failed 0)

(define (test descr result)
 (set! tests-run (+ tests-run 1))
 (if (not result)
  (begin
   (set! tests-failed (+ tests-failed 1))
   (display "Failed: ")
   (display descr)
   (newline))))

(define (run-test name func)
 (set! tests-run 0)
 (set! tests-failed 0)
 (func)
 (display name)
 (display " ")
 (display (- tests-run tests-failed))
 (display "/")
 (display tests-run)
 (newline))

