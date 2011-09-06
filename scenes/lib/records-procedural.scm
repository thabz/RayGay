(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
 (vector 'rtd name parent uid sealed? opaque? field))

(define (record-type-descriptor? obj)
 (and (vector? obj)
      (= 7 (vector-length obj))
      (eqv? 'rtd (vector-ref obj 0))))

(define (make-record-constructor-descriptor rtd parent-constructor-descriptor protocol)
 (vector 'rcd rtd parent-constructor-descriptor protocol))

(define (record-constructor rcd)
 (lambda args
  (list->vector (cons (vector-ref rcd 1) args))))

(define (record-predicate rtd)
 (lambda (obj)
  (and (vector? obj)
       (< 1 (vector-length obj))
       (eq? rtd (vector-ref obj 0)))))

; Boundary checking sacrificed for speed.
(define (record-accessor rtd k)
 (lambda (obj)
  (vector-ref obj (+ 1 k))))

; Boundary checking sacrificed for speed.
(define (record-mutator rtd k)
 (lambda (obj new-value)
  (vector-set! obj k new-value)))


