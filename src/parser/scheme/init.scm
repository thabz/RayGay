

(define-macro (when test . consequent)
;   "A Common LISP style when macro"        
   `(if ,test (begin ,@consequent)))    

(when #t (display "kaj")(newline))