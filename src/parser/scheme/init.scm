

(define-macro (when test . consequent)
;   "A Common LISP style when macro"        
   `(if ,test (begin ,@consequent)))    

(when #t (display "kaj")(newline))

(define-macro (do vars endtest . body)
  (lambda (do-macro)
    (apply (lambda (do vars endtest . body)
             (let ((do-loop (gensym)))
               `(letrec ((,do-loop
                           (lambda ,(map (lambda (x)
                                           (if (pair? x) (car x) x))
                                      `,vars)
                             (if ,(car endtest)
                               (begin ,@(cdr endtest))
                               (begin
                                 ,@body
                                 (,do-loop
                                   ,@(map (lambda (x)
                                            (cond
                                              ((not (pair? x)) x)
                                              ((< (length x) 3) (car x))
                                              (else (car (cdr (cdr x))))))
                                       `,vars)))))))
                  (,do-loop
                    ,@(map (lambda (x)
                             (if (and (pair? x) (cdr x))
                               (car (cdr x))
                               '()))
                        `,vars)))))
      do-macro)))

(display
(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
	  (vector-set! vec i i)))
(newline)