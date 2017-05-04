;;; -*-Scheme-*-

(define (p n)
  (let f ((n n) (i 2))
    (cond
     ((> i n) '())
     ((integer? (/ n i))
      (cons i (f (/ n i) i)))
     (else
      (f n (+ i 1))))))

(display (p 9050349))(newline)
(display (p 36288009))(newline)
(display (p 41943041))(newline)
