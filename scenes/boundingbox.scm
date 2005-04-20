
(load "lib/raygay.scm")

(set-image-size '(640 480))
(set-background '(0.3 0.6 0.7 0.0))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (100 1000 2000)
       lookat (0 230 0)
       up (0 1 0)
       fov 55
       aa 0)))

(define brown
  (make-material
    '( diffuse (0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define green
  (make-material
    '( diffuse (0.4 0.7 0.2)
       kd 1.0
       ks 0.0)))

(define sesam
  (make-material
    '( diffuse (0.9 0.9 0.6)
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


(add-to-scene (list
  (make-pointlight '(500 1300 1300))
  (make-pointlight '(-500 1300 1300))))

(add-to-scene 
 (make-box '(-1000 -100 -1000) '(1000 -90 1000) brown))

(define sp (make-sphere '(0 400 0) 500))
(define cy 
 (make-difference
  sp
  (make-sphere '(0 1000 0) 900)
  grey85
 ))

(add-to-scene cy)
(add-to-scene (make-bounding-box cy 10 brown))
(add-to-scene (make-bounding-box sp 5 green))

(display (bounding-box cy))
