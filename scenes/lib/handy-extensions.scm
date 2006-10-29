
;(define (+1 x)
; (+ x 1))

;(define (-1 x)
; (- x 1))

(define x-axis '(1 0 0))
(define y-axis '(0 1 0))
(define z-axis '(0 0 1))

(define (.x vec) (car vec))
(define (.y vec) (list-ref vec 1))  
(define (.z vec) (list-ref vec 2))

; Shortcut for rotating around the x-axis
(define (rotate-x obj angle)
    (rotate obj x-axis angle))

; Shortcut for rotating around the y-axis        
(define (rotate-y obj angle)
    (rotate obj y-axis angle))

; Shortcut for rotating around the z-axis
(define (rotate-z obj angle)
    (rotate obj z-axis angle))
                  
; Print followed by a newline                                            
(define (displayln t)
    (display t)
    (newline ))

; Returns a random element from a list
(define (pick-random-from-list l)
    (list-ref l (random (length l))))        