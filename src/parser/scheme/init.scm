
#|

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


(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
	  (vector-set! vec i i))
(newline)


(define-macro (let name vars . body)
	(cond
	   ((symbol? name)
			  (let* ((varnames (map car vars))
				       (varvals (map cadr vars)))
		      `(let* ((,name (lambda ,varnames ,@body)))
                  (,name ,@varvals))))
		 ((or (pair? name) (null? name))
			   (let* ((body (cons vars body))
  			        (vars name))
            `(let* ,vars ,@body)))))


(display "Hej")(newline)

(let loop ((i 10)(j '()))
   (if (= 0 i)
     j
     (loop (- i 1) (cons i j))))

(let ((i 20)) (display i)(newline)) 
|#

(define (char-cmp? cmp l)
     (apply cmp (map char->integer l)))
(define (char-ci-cmp? cmp l)
     (apply cmp (map char->integer (map char-downcase l))))

(define (char=? . l) (char-cmp? = l))
(define (char<? a b) (char-cmp? < l))
(define (char>? a b) (char-cmp? > l))
(define (char<=? a b) (char-cmp? <= l))
(define (char>=? a b) (char-cmp? >= l))

(define (char-ci=? a b) (char-ci-cmp? = l))
(define (char-ci<? a b) (char-ci-cmp? < l))
(define (char-ci>? a b) (char-ci-cmp? > l))
(define (char-ci<=? a b) (char-ci-cmp? <= l))
(define (char-ci>=? a b) (char-ci-cmp? >= l))

; Note the trick of returning (cmp x y)
(define (string-cmp? chcmp cmp a b)
     (let ((na (string-length a)) (nb (string-length b)))
          (let loop ((i 0))
               (cond
                    ((= i na)
                         (if (= i nb) (cmp 0 0) (cmp 0 1)))
                    ((= i nb)
                         (cmp 1 0))
                    ((chcmp = (string-ref a i) (string-ref b i))
                         (loop (succ i)))
                    (else
                         (chcmp cmp (string-ref a i) (string-ref b i)))))))


(define (string=? a b) (string-cmp? char-cmp? = a b))
(define (string<? a b) (string-cmp? char-cmp? < a b))
(define (string>? a b) (string-cmp? char-cmp? > a b))
(define (string<=? a b) (string-cmp? char-cmp? <= a b))
(define (string>=? a b) (string-cmp? char-cmp? >= a b))

(define (string-ci=? a b) (string-cmp? char-ci-cmp? = a b))
(define (string-ci<? a b) (string-cmp? char-ci-cmp? < a b))
(define (string-ci>? a b) (string-cmp? char-ci-cmp? > a b))
(define (string-ci<=? a b) (string-cmp? char-ci-cmp? <= a b))
(define (string-ci>=? a b) (string-cmp? char-ci-cmp? >= a b))


(define (seed) 0)

(define generate-name                            
  (let ((count (seed)))
    (lambda (symbol)
      (set! count (+ 1 count))
      (string->symbol
       (string-append (symbol->string symbol)
                      "|"
                      (number->string count))))))

'aa
