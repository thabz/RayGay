; The orthocircles implicit surface is defined by
; ((x² + y² – 1)² + z²) * ((y² + z² – 1)² + x²) * ((z² + x² – 1)² + y²) – 
; 0.0375² * (1 + 3 * (x² + y² + z²)) = 0 
;
; See http://www.flickr.com/photos/mylaboratory/417717535/in/set-72157603650366417/

(define high-quality #t)

(load "lib/raygay.scm")
(load "lib/colors.scm")

(if high-quality
  (set-image-size image-size-23-inch-apple-cinema)
  (set-image-size image-size-360-hd))

(set-background #(1 1 1))

(set-renderer "pathtracer")
(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera (list 
       'pos #(2 6 16)
       'lookat #(0 1.5 1)
       'up #(0 1 0)
       'fov 45
       'sampler (make-halton-sampler (if high-quality 1000 10))
       )))


(define mat 
  (make-material (list
     'diffuse #(1 1 1)
     'kd 1.0
     'ks 0.0)))


(if high-quality
 (add-to-scene (make-arealight #(1300 1300 1300) #(-1 -1 -1) 200 256 0.1))
 (add-to-scene (make-pointlight #(1300 1300 1300))))

(add-to-scene 
  (translate 
  (make-continuous-studio-backdrop #(20 10 10) 0.01 mat)
  #(0 -2 -2)))

; Returns a parameterized torus knot function that 
; takes one parameter t in [0,1]
; See http://en.wikipedia.org/wiki/Torus_knot
(define (make-torus-knot-path p q)
 (make-path 
 (lambda (t) 
  (let* ((phi (* t 2π))
         (r (+ (cos (* q phi)) 2)))
   (vector (* r (cos (* p phi)))
           (* r (sin (* p phi)))
	   (sin (* q phi)))))))


(define (s) 
 (make-sphere #(0 0 0) 0.1 mat))

(add-to-scene
 (translate (make-necklace (make-torus-knot-path 3 7) 1000 s)
#(0 1.5 1)))
