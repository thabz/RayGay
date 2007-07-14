
(define-macro (values x)
   `(list ,@x))

(define-macro (call-with-values f g)
   `(apply ,g (,f)))

