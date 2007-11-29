
; guile (1.8.1)   30,73s user 0,53s system 99% cpu 31,467 total
; python (2.5.1)  36,45s user 0,04s system 99% cpu 36,716 total
; scheme48        37,05s user 0,07s system 99% cpu 37,288 total
; raygay-repl     72,50s user 0,08s system 99% cpu 1:12,63 total
; perl (5.8.8)    74,30s user 0,07s system 99% cpu 1:15,08 total
; elk (3.0)      108,10s user 0,15s system 99% cpu 1:48,32 total
; ruby (1.8.6)   183,87s user 0,16s system 99% cpu 3:04,28 total

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) 
       (fib (- n 2)))))

(do ((i 0 (+ i 1)))
  ((= i 36))
  (display i) (display " => ")
  (display (fib i)) (newline))
