

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



