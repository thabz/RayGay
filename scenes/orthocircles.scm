; The orthocircles implicit surface is defined by
; ((x² + y² – 1)² + z²) * ((y² + z² – 1)² + x²) * ((z² + x² – 1)² + y²) – 0.0375² * (1 + 3 * (x² + y² + z²)) = 0 
;
; See http://www.flickr.com/photos/mylaboratory/417717535/in/set-72157603650366417/


(load "lib/raygay.scm")
(load "lib/colors.scm")

(set-image-size image-size-1080-hd)
(set-image-size image-size-360-hd)

(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(14 13 10)
       lookat #(0 -2 0)
       up #(0 1 0)
       fov 45
       aa 0)))

(define brown
  (make-material
    '( diffuse #(0.7 0.4 0.2)
       kd 0.9
       specular #(1 1 1)
       ks 0.1
       specpow 30)))

(define grey85
  (make-material
    '( diffuse #(0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define chrome
  (make-material
    '( diffuse #(0.9 0.7 0.8)
       kd 0.4
       specular #(1.0 1.0 1.0)
       ks 0.6
       specpow 15)))


(add-to-scene (make-pointlight #(500 1300 1300)))
(add-to-scene (make-pointlight #(-500 1500 1300)))

;(add-to-scene (make-box #(-1700 -51 -1700) #(1700 -5 1700) brown))

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



(add-to-scene
  (make-isosurface 
    orthocircles
    #(-5 -5 -5)
    #(5 5 5)
    0.0         ; iso-value
    100         ; steps
    0.000001    ; accuracy
    chrome))
         