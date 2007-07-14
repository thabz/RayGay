
(define (*internal*-char-ci-cmp? cmp l)
     (apply cmp (map char->integer (map char-downcase l))))

(define (char-ci=? . l) (*internal*-char-ci-cmp? = l))
(define (char-ci<? . l) (*internal*-char-ci-cmp? < l))
(define (char-ci>? . l) (*internal*-char-ci-cmp? > l))
(define (char-ci<=? . l) (*internal*-char-ci-cmp? <= l))
(define (char-ci>=? . l) (*internal*-char-ci-cmp? >= l))

; Note the trick of returning (cmp x y)
(define (*internal*-two-string-cmp? chcmp cmp a b)
     (let ((na (string-length a)) (nb (string-length b)))
          (let loop ((i 0))
               (cond
                    ((= i na)
                         (if (= i nb) (cmp 0 0) (cmp 0 1)))
                    ((= i nb)
                         (cmp 1 0))
                    ((chcmp = (list (string-ref a i) (string-ref b i)))
                         (loop (+ 1 i)))
                    (else
                         (chcmp cmp (list (string-ref a i) (string-ref b i))))))))

(define (string-cmp? chcmp cmp l)
	(if (null? (cdr l)) 
		#t
		(and (*internal*-two-string-cmp? chcmp cmp (car l) (cadr l))
		     (string-cmp? chcmp cmp (cdr l)))))

(define (string-ci=? . l) (string-cmp? *internal*-char-ci-cmp? = l))
(define (string-ci<? . l) (string-cmp? *internal*-char-ci-cmp? < l))
(define (string-ci>? . l) (string-cmp? *internal*-char-ci-cmp? > l))
(define (string-ci<=? . l) (string-cmp? *internal*-char-ci-cmp? <= l))
(define (string-ci>=? . l) (string-cmp? *internal*-char-ci-cmp? >= l))

(define-macro (values x)
   `(list ,@x))

(define-macro (call-with-values f g)
   `(apply ,g (,f)))

