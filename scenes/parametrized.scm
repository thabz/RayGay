
(load "lib/raygay.scm")
(load "lib/objects/make-pill.scm")

(set-image-size '(800 600))
(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(1000 1000 2000)
       lookat #(0 100 0)
       up #(0 1 0)
       fov 45
       aa 4)))

(define brown
  (make-material
    '( diffuse #(0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define grey85
  (make-material
    '( diffuse #(0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))

(define globus-texture (make-texture "gfx/test-texture.png" 2 15 'bilinear))
(define globus-mat
  (make-material
   (list
       'diffuse globus-texture
       'kd 1.0
       'specular #(1.0 1.0 1.0)
       'ks 0.0
       'specpow 30)))


(add-to-scene (make-pointlight #(500 2600 1300)))
(add-to-scene (make-pointlight #(-500 2600 1300)))

(add-to-scene
    (make-box 
      #(-1700 -51 -1700) 
      #(1700 -1 1700) 
      brown))

(define PI 3.141592654)

(define (func u v)
 (let* ((phi (* 4 PI v)))
  (vector
   (* u 400 (cos phi))
   (* 700 v)
   (* u 400 (sin phi))

   )))

(add-to-scene (make-parametrized-surface func 10 100 #f #f globus-mat))

