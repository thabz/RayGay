
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
 (let  ((bytecode '()))
  (define (compile-expression-internal expression)
   (cond 
    ((list? expression)
     (for-each compile-expression-internal (cdr expression))
     (set! bytecode (cons (compile-function (car expression)) bytecode)))
    ((integer? expression)
     (set! bytecode (cons (PUSH-INTEGER expression) bytecode)))))
  (compile-expression-internal expression)
  (reverse bytecode)))

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
 (+ 1 2)
 (+ 3 4))


(compile-and-dump test0)    

