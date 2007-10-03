; Stuff for benchmarking schemes
;
; 2007-05-09:
; guile ../../../scenes/bench.scm  0.58s user 0.03s system 99% cpu 0.613 total
; ./repl ../../../scenes/bench.scm  19.45s user 0.02s system 99% cpu 19.571 total
;
; 2007-05-10:
; ./repl ../../../scenes/bench.scm  17.72s user 0.02s system 97% cpu 18.129 total
; ./repl ../../../scenes/bench.scm  16.62s user 0.03s system 96% cpu 17.269 total
; ./repl ../../../scenes/bench.scm  16.27s user 0.03s system 99% cpu 16.443 total

; 2007-05-18 (compacted size of SchemeObject to 12 bytes)
; ./repl ../../../scenes/bench.scm  13.64s user 0.22s system 96% cpu 14.356 total
; 2007-05-18 (made heap an array instead of a list<>)
; ./repl ../../../scenes/bench.scm  11.59s user 0.15s system 99% cpu 11.836 total
; 2007-05-18 (made stack a vector<> instead of a list<>)
; ./repl ../../../scenes/bench.scm  2.32s user 0.12s system 98% cpu 2.467 total
; ./repl ../../../scenes/bench.scm  2.23s user 0.12s system 98% cpu 2.467 total

; 2007-05-19 (stopped leak of binding-envts and 10 times more iterations of bench
; ./repl ../../../scenes/bench.scm  26.41s user 0.03s system 99% cpu 26.628 total
; ./repl ../../../scenes/bench.scm  25.38s user 0.03s system 95% cpu 26.640 total
; ./repl ../../../scenes/bench.scm  24.73s user 0.03s system 99% cpu 24.943 total
; ./repl ../../../scenes/bench.scm  24.33s user 0.07s system 98% cpu 24.664 total
; ./repl ../../../scenes/bench.scm  22.89s user 0.05s system 98% cpu 23.208 total
; ./repl ../../../scenes/bench.scm  24.24s user 0.04s system 97% cpu 24.908 total
;
; ./src/tracer scenes/benchmark-scheme.scm out.png  22.37s user 0.10s system 99% cpu 22.604 total

; 2007-06-11 bucket_map instead of stl::map
;./src/tracer scenes/benchmark-scheme.scm out.png  20.94s user 0.08s system 98% cpu 21.265 total
;
; 2007-07-13 No cons'ing when calling built-ins
; ./repl tests/benchmark.scm  18.06s user 0.08s system 98% cpu 18.386 total
;
; 2007-07-16 Cache hash of symbol inside symbol
; ./repl -d tests/bench*  17,22s user 0,04s system 98% cpu 17,458 total
;
; 2007-07-27 Use & instead of % to find bucket
; ./repl -d tests/bench*  15.70s user 0.03s system 99% cpu 15.788 total
; ./repl -d tests/bench*  15.36s user 0.02s system 99% cpu 15.431 total
; ./repl -d tests/bench*  14.71s user 0.02s system 99% cpu 15.431 total
; 
; 2007-07-27 Avoid called rest() three times in eval_built_in_procedure_call()
; ./repl -d tests/bench*  14.21s user 0.02s system 99% cpu 14.265 total
; ./repl -d tests/bench*  14.05s user 0.02s system 99% cpu 14.102 total
;
; 2007-09-26 Introducing simple environments (list of binding-pairs) and other smaller optimizations
; ./repl scheme/tests/benchmark.scm  9.61s user 0.07s system 99% cpu 9.767 total
; guile scheme/tests/benchmark.scm  5.52s user 0.13s system 99% cpu 5.708 total
; ./repl scheme/tests/benchmark.scm  9.35s user 0.05s system 99% cpu 9.429 total
; ./repl scheme/tests/benchmark.scm  9.20s user 0.05s system 99% cpu 9.280 total
;
; ./repl scheme/tests/benchmark.scm  9.06s user 0.05s system 99% cpu 9.139 total

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

(define (bench)
  (begin
  (do ((i 0 (+ 1 i)))
      ((= i 1000000))
      (set! sum (+ sum (iso-func1 1.1 2.1 3.01))))
  (display sum)
  (newline)))

(bench)

