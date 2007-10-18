
(load "lib/raygay.scm")

(set-image-size '(800 600))
(set-background #(0.1 0.1 0.3))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(500 530 1000)
       lookat #(200 200 400)
       up #(0 1 0)
       fov 45
       dof (60 100 #(0 200 500))
       aa 0)))

(define brown
  (make-material
    '( diffuse #(0.15 0.31 0.38)
       kd 1.0
       ks 0.0)))

(define blue
  (make-material
    '( diffuse #(0.2 0.2 0.8)
       kd 0.4
       specular #(1.0 1.0 1.0)
       ks 0.5
       specpow 15)))

(define chrome
  (make-material
    '( diffuse #(0.2 0.2 0.2)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 40)))

(define slate-tiles
  (make-material
   (list
       'diffuse (make-texture "gfx/js_suelo1.jpg" 6 6 'bilinear)
       'kd 1.0
       'ks 0.0)))


(add-to-scene (make-pointlight #(100 1300 1300)))

(add-to-scene
    (make-box
      #(-3000 -50 -3000) 
      #(3000 0 3000) 
      slate-tiles))

(define circle
   (make-circle #(0 200 0) 400 y-axis))

(add-to-scene 
 (make-necklace circle 9 
  (lambda() (make-sphere #(0 0 0) 100 blue)))
 (translate
    (make-torus 400 20 chrome)
    #(0 200 0)))

