
(load "lib/raygay.scm")
;(load "objects.scm")
(load "iso-functions.scm")

(set-image-size '(1024 768))
;(set-background (make-texture "gfx/goodmorning.jpg" 1 1 "bilinear"))
(set-background '(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (14 13 10)
       lookat (0 -2 0)
       up (0 1 0)
       fov 45
       aa 0)))

(define brown
  (make-material
    '( diffuse (0.7 0.4 0.2)
       kd 0.9
       specular (1 1 1)
       ks 0.1
       specpow 30)))

(define grey85
  (make-material
    '( diffuse (0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define chrome
  (make-material
    '( diffuse (0.9 0.7 0.8)
       kd 0.4
       specular (1.0 1.0 1.0)
       ks 0.6
       specpow 15)))


(add-to-scene (make-pointlight '(500 1300 1300)))
;(add-to-scene (make-pointlight '(-500 1500 1300)))

;(append! scene (list (make-box '(-1700 -51 -1700) '(1700 -5 1700) brown)))

(define (my-iso-chair x y z) 
 (iso-chair 5 0.95 0.8 x y z))

(define (my-iso-torus x y z) 
 (iso-torus 3 2 x y z))

(define (my-tangledcube x y z) 
 (iso-tanglecube x y z))

(add-to-scene    
 (make-marching-cubes
  (make-isosurface 
   my-tangledcube
   '(-6 -6 -6)
   '(6 6 6)
   0.0	; iso-value
   500 		; steps
   0.000001 	; accuracy
   grey85)
 40
 #t))

(display "End of mobius.scm")
(newline)
