
(define-macro (section title . tests)
  `(let ((problems 0))
    (display "Section ")
    (display ,title)
    ,@tests
    (if (= 0 problems)
        (display " OK"))
    (newline)))

(define-macro (assert e)
  `(if (not ,e)
    (begin
      (set! problems (+ 1 problems))
      (newline)
      (display "   Failed: ")
      (display (quote ,e))
      )))

(section "equal?"
  (assert (eqv? 1/2 1000/2000))
  (assert (eqv? 1/2 100/2000))
  (assert (eqv? 1/2 100/2000))
  (assert (equal? (number->string 1/2) "1/2")))
  
(section "equal?"
  (assert (eqv? 1/2 1000/2000))
  (assert (eqv? 1/2 100/2000))
  (assert (eqv? 1/2 100/2000))
  (assert (equal? (number->string 1/2) "1/2")))
    