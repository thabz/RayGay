
(define (+1 x)
 (+ x 1))

(define (-1 x)
 (- x 1))

(define x-axis '(1 0 0))
(define y-axis '(0 1 0))
(define z-axis '(0 0 1))

(define (.x vec) (car vec))
(define (.y vec) (list-ref vec 1))  
(define (.z vec) (list-ref vec 2))
