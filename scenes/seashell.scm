
(load "lib/raygay.scm")
(load "lib/objects/make-pill.scm")

(set-image-size '(800 600))
(set-background '(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (0 20 0)
       lookat (0 0 -6)
       up (0 0 1)
       fov 45
       aa 3)))

(define brown
  (make-material
    '( diffuse (0.15 0.31 0.38)
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

;(define globus-texture (make-texture "gfx/test-texture.png" 15 5 "bilinear"))
(define tiles-texture (make-texture "gfx/js_suelo1.jpg" 30 5 "bilinear"))
(define tiles-material
  (make-material
   (list
       'diffuse tiles-texture
       'kd 0.8
       'specular '(1.0 1.0 1.0)
       'ks 0.2
       'specpow 30)))


(add-to-scene (make-pointlight '(500 2600 1300)))
(add-to-scene (make-pointlight '(-500 2600 1300)))

(add-to-scene
    (make-box 
      '(-1700 -100 -1700) 
      '(1700 -99 1700) 
      brown))

(define PI 3.141592654)

(define (func u01 v01)
  (let* ((u (* u01 8 PI))
	 (v (* v01 2 PI)) 
	 (e6 (exp (/ u (* 6 PI))))
	 (e3 (exp (/ u (* 3 PI))))
	 (c (cos (* 0.5 v)))
	 (c2 (* c c)))
    (list
      (* 2 (- 1 e6) (cos u) c2)
      (* 2 (- e6 1) (sin u) c2)
      (- 1 e3 (sin v) (* -1 e6 (sin v)))
      ))) 

(add-to-scene
    (make-parametrized-surface 
     func 100 100 #f #f 
     tiles-material))

