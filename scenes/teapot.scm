
(load "lib/raygay.scm")
(load "lib/colors.scm")
(load "lib/objects/make-teapot.scm")

(set-image-size '(640 480))
(set-background color-tan)

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(0 200 600)
       lookat #(0 20 0)
       up #(0 1 0)
       fov 45
       aa 3)))

(define brown
  (make-material
    (list 'diffuse color-moccasin
          'kd 1.0
          'ks 0.0)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


;(add-to-scene (make-pointlight #(500 1300 1300)))
;(add-to-scene (make-pointlight #(-500 1500 1300)))
;(add-to-scene (make-arealight #(500 1300 1300) #(-1/2 -1 -1) 200 64 0.1))
;(add-to-scene (make-arealight #(-500 1500 1300) #(1/2 -1 -1) 200 64 0.1))
(add-to-scene (make-arealight #(0 1000 1000) #(0 -1 -1) 200 256 0.1))

;(add-to-scene (make-box #(-1700 -51 -1700) #(1700 1 1700) brown))

(add-to-scene (make-continuous-studio-backdrop #(800 400 400) 15 brown))  

(add-to-scene 
  (translate
    (rotate
    (make-teapot 20 20 chrome)
    y-axis 30)
  #(0 0 200)))
