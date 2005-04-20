

(load "lib/raygay.scm")

(set-image-size '(800 600))
;(set! background '(0.3 0.6 0.7))
(set-background (make-texture "gfx/goldensunset.jpg" 1 1 "bilinear"))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (-2000 2000 30)
       lookat (0 200 0)
       up (0 1 0)
       fov 35
       aa 4)))

(define brown
  (make-material
    '( diffuse (0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define grey85
  (make-material
    '( diffuse (0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define chrome
  (make-material
    '( diffuse (0.8 0.8 0.8)
       kd 0.2
       specular (1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


(add-to-scene (make-pointlight '(500 1300 1300)))
;(append! scene (list (make-pointlight '(-500 1500 1300))))

;(append!
;  scene 
;  (list 
;    (make-box 
;      '(-1700 -51 -1700) 
;      '(1700 1 1700) 
;      brown)))

(define num 50)
(define twirls 3.5)

(add-to-scene    
  (make-extrusion 
   (make-circle '(0 300 0) 400 '(0 1 0))
   (make-ellipse '(0 0 0) 100 200 '(0 0 1))
   5 100 500 chrome))

