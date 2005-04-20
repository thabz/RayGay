
(load "lib/raygay.scm")
(load "lib/l-system.scm")

(set-image-size '(640 480))
(set-background '(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (1000 1000 2000)
       lookat (0 500 0)
       up (0 1 0)
       fov 45
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

(define chrome
  (make-material
    '( diffuse (0.8 0.8 0.8)
       kd 0.2
       specular (1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


(add-to-scene (make-pointlight '(500 1300 1300)))
(add-to-scene (make-pointlight '(-500 1300 1300)))

(add-to-scene
    (make-box 
      '(-11700 -51 -11700) 
      '(11700 1 11700) 
      green))


(define rules
 '((F . (F + G - G))
   (G . ( [ F + F + F ] [ F - F - F ] ))))

(define rules
 '(( X . (F - [ [ X ] + X ] + F [ + F X ] - X))
     (F . (F F))))

(add-to-scene
 (make-l-system-object 
  22.5  ; angle
  15 ; length
  4  ; thickness
  '(X) ; axiom
  rules
  5   ; depth
  brown))

