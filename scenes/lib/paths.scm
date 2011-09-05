
; For at meget af nedenstående rigtigt kan lade sig gøre
; skal vi have basal OO på plads med R6RS records.

; tangent-function and closed? are derived from position-function
; if not supplied
(define (make-path position-function tangent-function closed?))    

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

(define (make-ellipse center radius1 radius2 normal))
(define (make-linesegment from to))
(define (make-spiral path radius windings offset))
(define (make-bezier-spline vector...)
(define (make-catmullrom-spline vector...)

(define (point-on-path path t))
(define (tangent-to-path path t))
(define (transform-path path transformation))
(define (path? path))

