(load "lib/raygay.scm")

(set-image-size '(1024 768))
(set-background #(1.0 1.0 1.0))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(1000 500 1800)
       lookat #(0 100 0)
       up #(0 1 0)
       fov 45
       aa 2)))


(add-to-scene (make-pointlight #(100 1300 1300)))
;(add-to-scene (make-arealight #(100 1300 1300) #(-0.1 -1 -1) 500 80 0.1 #(1 1 1)))

(define ground 
  (make-material
    '( diffuse #(1.0 1.0 1.0)
       kd 0.5
       specular #(1.0 1.0 1.0)
       ks 0.5
       specpow 15)))

(define chrome
  (make-material
    '( diffuse #(0.85 0.85 0.85)
       kd 0.5
       specular #(0.85 0.85 0.85)
       ks 0.5
       specpow 45)))

(define blue 
  (make-material
    '( diffuse #(0.01 0.2 0.9)
       kd 0.8
       specular #(0.5 0.5 0.5)
       ks 0.2
       specpow 45)))

(add-to-scene (make-sphere #(0 -100200 0) 100000 ground))

(define (halvkugle)
 (make-intersection
   (make-sphere #(0 0 0) 300)
   (make-solid-box #(-301 0 -301) #(301 301 301))
   blue))

(add-to-scene (translate (halvkugle) #(0 400 0)))
(add-to-scene (translate (flip-x (halvkugle)) #(0 100 0)))

(define spiral
 (make-spiral
  (make-linesegment #(0 100 0) #(0 400 0))
  280 6 0))

(add-to-scene
 (make-extrusion 
  spiral 
  (make-circle #(0 0 0) 20 y-axis)
  0
  12
  300
  blue))

(define (foot)
    (make-ellipsoid #(0 0 0) #(150 100 200) blue))

(add-to-scene
 (translate
   (rotate-y (foot) -30)
   #(200 -200 100)))

(add-to-scene
 (translate
   (rotate-y (foot) 30)
   #(-200 -200 100)))
 
(add-to-scene (make-sphere #(0 530 300) 100 blue))

