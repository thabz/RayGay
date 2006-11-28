
(load "lib/raygay.scm")

(set-image-size '(1600 1200))
(set-image-size '(640 480))
(set-background #(0.8 0.7 0.3))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(3 3 -4)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 3)))

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

(define shinyred
  (make-material
    '( diffuse #(0.85 0.2 0.1)
       kd 0.8
       specular #(1.0 1.0 1.0)
       specpow 30
       ks 0.2)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


(add-to-scene (list
  (make-pointlight #(500 1300 -1300))
  (make-pointlight #(-500 1300 1300))))

(add-to-scene
 (make-julia
;  #(-0.2 0.6 0.2 0.2)
  #(-0.08 0.0 -0.8 -0.03)
;  #(-0.450,-0.447,0.181,0.306)
  10
  5000
  0.00001
  shinyred))


