
(load "lib/raygay.scm")

(set-image-size image-size-720-hd)
(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(0 1000 2000)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 2)))

(define brown
  (make-material
    '( diffuse #(0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(add-to-scene (list
  (make-pointlight #(500 1300 1300))
  (make-pointlight #(-500 1300 1300))))

(add-to-scene
  (make-torus 600 200 brown))