
(load "../lib/raygay.scm")

(set-image-size '(640 480))
(set-background #(0.1 0.1 0.3))

(define cam-pos-path 
  (make-bezierspline (list
    #(200 2000 3000)
    #(200 1000 3000)
    #(200 700 2000)
    #(200 530 1000)
    #(200 530 1000)
    #(200 530 1000))))

(define focal-point-path 
  (make-bezierspline (list
    #(0 200 2000)
    #(0 200 2000)
    #(0 200 1000)
    #(0 200 500)
    #(0 200 500)
    #(0 200 500))))


(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    (list 'pos (point-on-path cam-pos-path clock)
          'lookat #(200 200 400)
          'up #(0 1 0)
          'fov 45
          'dof (list 60 100 (point-on-path focal-point-path clock))
          'aa 0)))

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
       'diffuse (make-texture "../gfx/js_suelo1.jpg" 6 6 "bilinear")
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
 (rotate 
    (make-necklace circle 9 
       (lambda() (make-sphere #(0 0 0) 100 blue)))
    y-axis
    (* clock 360))
 (translate
    (make-torus 400 20 chrome)
    #(0 200 0)))

