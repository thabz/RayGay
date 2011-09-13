
(load "records-procedural.scm")
(load "vector-math.scm")
(load "handy-extensions.scm") ; For π

; First define a path class or record-type in Scheme-speak
(define path-rtd
 (make-record-type-descriptor 'path #f #f #f #f
  '#((mutable position-func) 
     (mutable tangent-func) 
     (mutable closed?) 
     (mutable matrix))))

(define path-rcd
 (make-record-constructor-descriptor path-rtd #f #f))

; TODO: tangent-function and closed? are derived from 
; position-function if not supplied.
; Matrix is the identity if not supplied.
; (define (make-path position-function tangent-function closed?) #)
(define make-path (record-constructor path-rcd))

(define path-position (record-accessor path-rtd 0))

(define path-tangent (record-accessor path-rtd 1))    

(define path-closed? (record-accessor path-rtd 2))

(define path-matrix (record-accessor path-rtd 3))
(define path-set-matrix! (record-mutator path-rtd 3))

(define (path-transform path m)
 (path-set-matrix! path 
  (m* (path-matrix path) m)))

(define path? (record-predicate path-rtd))
    
(define (point-on-path path t) 
 ((path-position path) t))

; Returns num points along path as a list
(define (points-on-path path num)
 (let loop ((result '())
	    (i 0))
  (if (= i num)
   result
   (loop (cons (point-on-path path (- 1 (/ i num))) result)
         (+ i 1)))))
 
(define (tangent-to-path path t) 
 ((path-tangent path) t))


; -------------------------
; The paths implementations
; -------------------------

(define (make-linesegment from to)
 (let ((tangent (vnormalize (v- to from))))
  (make-path
   (lambda (t)
    (vlerp from to t))
   (lambda (t)
    tangent)
   #f)))

(define (make-circle center radius)
 (make-path 
  (lambda (t)
   (let ((radians (* 2π t)))
   (vector (* radius (cos radians)) 
           (* radius (sin radians)) 0)))
  (lambda (t)
   (let ((radians (* 2π t)))
   (vector (- (sin radians)) (cos radians) 0)))
  #t))

(define (make-ellipse center radius1 radius2 normal)
 (make-path
  (lambda (t)
   (let ((radians (* 2 PI t)))
    (vector (* radius1 (cos radians))
            (* radius2 (sin radians)) 0)))
  (lambda (t)
   (let ((radians (* 2 PI t)))
    (vector (- (sin radians))
            (cos radians)) 0))
  #t))

(define (make-super-ellipse center a b r normal) 'todo)    

; A spiral around another path
(define (make-spiral center-path radius windings offset) 
 (make-path
  (lambda (t)
   (let* ((c (point-on-path center-path t))
	  (n (tangent-to-path center-path t))
          (circle (make-circle c radius n))
	  (tn (+ (* t windings) offset))
	  (tn2 (- tn (floor tn))))
    (point-on-path circle tn2)))
  (lambda (t)
   (let* ((c (point-on-path center-path t))
	  (n (tangent-to-path center-path t))
          (circle (make-circle c radius n))
	  (tn (+ (* t windings) offset))
	  (tn2 (- tn (floor tn)))
	  (circle-tangent (tangent-to-path circle t))
	  (path-tangent (tangent-to-path center-path t)))
    (vnormalize (v+ circle-tangent path-tangent))))))
    
; TODO: Expose Math::bernsteinPolynomial as a Scheme-function
(define (make-bezier-spline . vector) 'todo)

(define (make-catmullrom-spline . vector) 'todo)





