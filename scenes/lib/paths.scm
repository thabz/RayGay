

; First define a path-class or record-type-description in Scheme-speak
(define path-rtd
 (make-record-type-descriptor 'path #f #f #f #f
  '#((mutable position) (mutable tangent) (mutable closed?))))

(define path-rcd
 (make-record-constructor-descriptor path-rtd #f #f))

; tangent-function and closed? are derived from position-function
; if not supplied
; (define (make-path position-function tangent-function closed?) #)
(define make-path (record-constructor path-rcd))

(define path-position (record-accessor path-rtd 0))

(define path-tangent (record-accessor path-rtd 1))    

(define path-closed? (record-accessor path-rtd 2))

(define path? (record-predicate path-rtd))

; -------------------------
; The paths implementations
; -------------------------

(define (make-linesegment from to)
 (let ((tangent (normalize (v- to from))))
  (make-path
   (lambda (t)
    (+ (* from (- 1 t)) (* t to)))
   (lambda (t)
    tangent)
   #f)))
   
(define (make-linesegment from to)
  (make-path
   (lambda (t) 
    (+ (* from (- 1 t)) (* t to)))
   (lambda (t) 
    to)
   #f))
    
(make-path (lambda (t) 2) (lambda (t) 3) #f)


(define (make-circle center radius normal)
 (let* ((n (normalize normal))
	(y #(0 1 0))
	(x #(1 0 0))
	(a ((= n y) x y)))
 (translate	
  (orient
     (make-path 
      (lambda (t)
       (let ((radians (* 2 PI t)))
       (vector (* radius (cos radians)) (* radius (sin radians)) 0)))
      (lambda (t)
       (let ((radians (* 2 PI t)))
       (vector (- (sin radians)) (cos radians) 0)))
      #t)
   n (cross-product a n))
  center)))

(define (make-ellipse center radius1 radius2 normal) #f)
(define (make-linesegment from to) #f)
(define (make-spiral path radius windings offset) #f)
(define (make-bezier-spline . vector) #f)
(define (make-catmullrom-spline . vector) #f)

(define (point-on-path path t) 
 ((path-position path) t))
 
(define (tangent-to-path path t) 
 ((path-tangent path) t))

(define (transform-path path transformation) #f)


