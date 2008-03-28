; The orthocircles implicit surface is defined by
; ((x² + y² – 1)² + z²) * ((y² + z² – 1)² + x²) * ((z² + x² – 1)² + y²) – 0.0375² * (1 + 3 * (x² + y² + z²)) = 0 
;
; See http://www.flickr.com/photos/mylaboratory/417717535/in/set-72157603650366417/

(define (sqr n) (* n n))

(define (orthocircles x y z)
  (let ((x² (* x x))
        (y² (* y y))
        (z² (* z z)))
   (-
    (* (+ (sqr (+ x² y² –1)) z²)
       (+ (sqr (+ y² z² –1)) x²)
       (+ (sqr (+ z² x² –1)) y²)) 
    (* (sqr 0.0375) 
       (+ 1 
         (* 3 (+ x² y² z²)))))))

         