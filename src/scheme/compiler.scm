
(define (uint64->list n)
 (let ((v (make-bytevector 8)))
  (bytevector-u64-native-set! v 0 n)
  (bytevector->u8-list v)))

(define (PUSH-INTEGER x)
 (list 'PUSH-INTEGER x)
 ; MOVL $xxyyxxyyxxyyxxyy,%rax
 ; PUSH %rax
 (list #x48))


(define (ADD-INTEGERS)
 (list 'ADD-INTEGERS))

(define (SUBTRACT-INTEGERS)
 (list 'SUBTRACT-INTEGERS))

(define (compile-function symbol)
 (case symbol
  ((+) (ADD-INTEGERS))
  ((-) (SUBTRACT-INTEGERS))
  (else (list 'UNKNOWN-SYMBOL symbol))))

(define (compile-special-form form)
 (list 'SPECIALE-FORM))

(define (special-form? expression)
 (and (list? expression)
      (member (car expression) '(if let map or and not let*))))

(define (compile-expression expression) 
 (let  ((bytecode '()))
  (define (compile-expression-internal expression)
   (cond 
    ((special-form? expression)
     ; FIXME: Won't work if compile-special-form returns a list...
     (set! bytecode (cons (compile-special-form expression) bytecode)))
    ((list? expression)
     (for-each compile-expression-internal (cdr expression))
     (set! bytecode (cons (compile-function (car expression)) bytecode)))
    ((integer? expression)
     (set! bytecode (cons (PUSH-INTEGER expression) bytecode)))))
  (compile-expression-internal expression)
  (reverse bytecode)))

(define (compile proc)
 (let ((source ($source proc)))
  (map compile-expression source)))

(define (display-bytecode bytecode)
 (for-each 
  (lambda (code)
   (display code)(newline))
  bytecode))

(define (compile-and-dump proc)
 (display-bytecode (car (compile proc))))

(define (test0)
 (- (+ 1 (+ 2 3)) 4))

(define (test1 x)
 (+ x 1))

(define (test2)
 (+ 1 2)
 (+ 3 4))


(compile-and-dump test0)    

