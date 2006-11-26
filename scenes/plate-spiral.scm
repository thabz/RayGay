
(load "lib/raygay.scm")
(load "lib/objects/make-rounded-plate.scm")

(set-image-size '(1024 768))
(set-background (make-texture "gfx/goodmorning.jpg" 1 1 "bilinear"))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    `(pos (-2700 2700 20)
      lookat (0 -200 0)
      up ,y-axis
      fov 45
      dof (150.0 100 (-750 0 0))
      aa 0)))

(define chrome
  (make-material
    '( diffuse (0.8 0.8 0.8)
       kd 0.2
       specular (1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


(add-to-scene (make-pointlight '(500 1300 1300)))

(define num 70)
(define twists 2)    

(dotimes i num
  (add-to-scene
     (rotate 
       (translate 
         (rotate 
           (make-rounded-plate 150 20 chrome)
           z-axis (* twists 360 (/ i num)))
         '(600 0 0))
     y-axis (* (/ i num) 360))))
