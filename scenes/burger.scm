
(load "lib/raygay.scm")
(load "lib/objects/make-rounded-box.scm")

(set-image-size '(500 500))
(set-background '(0.3 0.6 0.7 0.0))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (100 500 1000)
       lookat (0 0 0)
       up (0 1 0)
       fov 45
       aa 4)))

(define brown
  (make-material
    '( diffuse (0.7 0.4 0.2)
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

;(add-to-scene
;    (make-box 
;      '(-1700 -51 -1700) 
;      '(1700 1 1700) 
;      brown))

(add-to-scene
  (make-difference
    (make-ellipsoid '(0 0 0) '(300 300 300))
    (make-solid-box '(-400 -40 -400) '(400 40 400))
    brown))
  
(add-to-scene
 (make-cylinder
  '(0 -10 0) '(0 10 0) 280 grey85))

(let loop 
 ((n 1000))
 (if (> n 0)
  (begin
   (add-to-scene
   (make-sphere (vscale (vrandomunit) 300)
    5
    sesam))
   (loop (- n 1)))))


(display (bounding-box     
 (make-cylinder
  '(0 -10 0) '(0 10 0) 280 grey85)))

