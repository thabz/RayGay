; The orthocircles implicit surface is defined by
; ((x² + y² – 1)² + z²) * ((y² + z² – 1)² + x²) * ((z² + x² – 1)² + y²) – 
; 0.0375² * (1 + 3 * (x² + y² + z²)) = 0 
;
; See http://www.flickr.com/photos/mylaboratory/417717535/in/set-72157603650366417/

(define high-quality #f)


(load "lib/raygay.scm")
(load "lib/colors.scm")

(if high-quality
  (set-image-size image-size-23-inch-apple-cinema)
  (set-image-size image-size-360-hd))

(set-background (color 'azure))

(set-renderer "raytracer")
(set-renderer "pathtracer")
(set-camera 
  (make-pinhole-camera (list 
       'pos #(2 3 6)
       'lookat #(0 0 0)
       'up #(0 1 0)
       'fov 45
       'sampler (make-halton-sampler (if high-quality 1000 10))
       )))

(define brown
  (make-material (list
    'diffuse (color 'cornsilk)
    'kd 1.0
    'ks 0.0)))

(define grey85
  (make-material (list
     'diffuse (color 'burlywood)
     'kd 1.0
     'ks 0.0)))

(define chrome
  (make-material (list 
       'diffuse (color 'white)
       'kd 0.4
       'specular (color 'white)
       'ks 0.6
       'specpow 15)))


(define (sqr n) (* n n))

(define (orthocircles x y z)
  (let ((x² (* x x))
        (y² (* y y))
        (z² (* z z)))
   (-
    (* (+ (sqr (+ x² y² -1)) z²)
       (+ (sqr (+ y² z² -1)) x²)
       (+ (sqr (+ z² x² -1)) y²)) 
    (* (sqr 0.0375) 
       (+ 1 
         (* 3 (+ x² y² z²)))))))

(add-to-scene (make-arealight #(1300 1300 1300) #(-1 -1 -1) 200 256 0.1))

(add-to-scene 
  (translate 
  (make-continuous-studio-backdrop #(12 10 10) 0.01 grey85)
#(0 -2 -2)))

;(add-to-scene (make-sphere #(0 0 0) 0.5 chrome)) 

(add-to-scene  
 (translate 
  (make-marching-cubes
      (make-isosurface 
        orthocircles
        #(-1 -1 -1)
        #(1 1 1)
        0.0         ; iso-value
        100         ; steps
        0.000001    ; accuracy
        brown) 
    (if high-quality 200 50) 
    #t)
#(0 0 0))) 
