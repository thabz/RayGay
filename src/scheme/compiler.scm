

(define (PUSH-INTEGER x)
 (list 'PUSH-INTEGER x))

(define (ADD-INTEGERS)
 (list 'ADD-INTEGERS))

(define (SUBTRACT-INTEGERS)
 (list 'SUBTRACT-INTEGERS))

(define (compile-function symbol)
 (case symbol
  ((+) (ADD-INTEGERS))
  ((-) (SUBTRACT-INTEGERS))
  (else (list 'UNKNOWN-SYMBOL symbol))))

(define (compile-expression expression) 
 (cond 
  ((list? expression)
   (let* ((function (car expression))
	  (args (cdr expression))
          (function-c (list (compile-function function)))
	  (args-c (compile-list args))
          (result (append args-c function-c)))
    (display result)(newline)
    result))
  ((integer? expression)
     (PUSH-INTEGER expression))))

(define (compile-list expressions)
 (map compile-expression expressions))

(define (compile proc)
  (let ((source ($source proc)))
   (compile-list source)))

(define (compile-and-dump proc)
 (display (compile proc))
 (newline))

(define (test0)
 (- (+ 1 (+ 2 3)) 4))

(define (test1 x)
 (+ x 1))

(define (test2)
 (+ 1 2))

(compile-and-dump test0)

(display (flatten '(1 2 3)))    

(define (flatten lists)

