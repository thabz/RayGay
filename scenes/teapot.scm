
(load "lib/raygay.scm")
(load "lib/objects/make-teapot.scm")

(set-image-size '(640 480))
(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(200 200 400)
       lookat #(0 50 0)
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

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


(add-to-scene (make-pointlight #(500 1300 1300)))
(add-to-scene (make-pointlight #(-500 1500 1300)))

(add-to-scene (make-box #(-1700 -51 -1700) #(1700 1 1700) brown))

(add-to-scene (make-teapot 20 20 chrome))
