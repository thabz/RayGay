
(load "lib/raygay.scm")

(set-image-size '(1024 768))
(set-background '(1.0 1.0 1.0))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    (list 'pos '(1200 1000 2000)
       'lookat '(0 300 0)
       'up '(0 1 0)
       'fov 45
       'sampler (make-uniform-jitter-sampler 10))))

(define chess-material
  (make-material
    (list 'diffuse (make-texture "gfx/chess.png" 200 200 "none")
	  'kd 1.0
	  'ks 0.0)))

(define red
  (make-material
    '( diffuse (0.8 0.2 0.3)
       kd 0.5
       specular (1.0 1.0 1.0)
       ks 0.5
       specpow 30)))

(define chrome
  (make-material
    '( diffuse (0.0 0.0 0.0)
       kd 0.1
       specular (1.0 1.0 1.0)
       ks 0.9
       specpow 30)))


(add-to-scene (make-pointlight '(500 1300 1300)))
;(append! scene (list (make-pointlight '(-500 1500 1300))))

(add-to-scene
    (make-box 
      '(-37000 -51 -37000) 
      '(37000 0 37000) 
      chess-material))

(add-to-scene (list
 (make-sphere '(-2000 200 -1000) 200 red)
 (make-sphere '(-500 300 -200) 300 chrome)
 (make-sphere '(500 200 -800) 200 chrome)
 (make-sphere '(-500 200 -1400) 200 chrome)
 (make-sphere '(-100 200 400) 200 chrome)
 (make-sphere '(100 200 -500) 200 chrome)
 (make-sphere '(300 100 200) 100 chrome)))
