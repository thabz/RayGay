
;
; Compile bytecode to machine-code
;

(define (assemble bytecode)
 (let ((snippets '()))
  #vu8(
    #x55
    #x48 #x89 
    #xe5
    #x90 #x90 #x90 #x90
    #x48 #xB8 #x10 #x20 #x30 #x40 #x50 #x60 #x70 #x80 ; MOV
    #x90 #x90 #x90 #x90
    #xc9
    #xC3)))

(define (uint64->list n)
 (let ((v (make-bytevector 8)))
  (bytevector-u64-set! v 0 n (endianness 'little))
  (bytevector->u8-list v)))

(define (PUSH-INTEGER x)
 (list 'PUSH-INTEGER x)
 ; MOVL $xxyyxxyyxxyyxxyy,%rax
 ; PUSH %rax
 (list #x48
    #x48 #xB8 #x10 #x20 #x30 #x40 #x50 #x60 #x70 #x80 ; MOV
 ))


(define (ADD-INTEGERS)
 (list 'ADD-INTEGERS))

(define (SUBTRACT-INTEGERS)
 (list 'SUBTRACT-INTEGERS))

(define (POP-AND-RETURN)
 (list 'POP-AND-RETURN))


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

(define (compile-to-bytecode proc)
 (let ((source ($source proc)))
  (map compile-expression source)))

(define (display-bytecode bytecode)
 (for-each 
  (lambda (code)
   (display code)(newline))
  bytecode))

(define (compile-and-dump proc)
 (display-bytecode (car (compile-to-bytecode proc))))

(define (compile proc)
 ($make-native-procedure (assemble (compile-to-bytecode proc)) proc)) 

(define (test0)
 (- (+ 1 (+ 2 3)) 4))

(define (test1 x)
 (+ x 1))

(define (test2)
 (+ 1 2)
 (+ 3 4))


(compile-and-dump test0)    

(define compiled (compile test0))    

(display (compiled))
(newline)
