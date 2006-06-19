
(load "lib/raygay.scm")
(load "lib/objects/make-rounded-box.scm")

(set-image-size '(800 600))
(set-image-size '(1200 600))
(set-image-size '(600 600))
(set-background '(0.3 0.6 0.7))
(set-background (make-texture "probes/galileo_probe.hdr" 1 1 "bilinear"))
(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera
    '( pos (2000 800 1700)
       lookat (0 0 0)
       up (0 1 0)
       fov 45
       aa 3)))
(set-camera 
  (make-lat-long-camera
    '( pos (0 40 0)
       lookat (0 0 -1000)
       up (0 1 0)
       fov 45
       aa 3)))
(set-camera 
  (make-fisheye-camera
    '( pos (0 40 0)
       lookat (0 0 -1000)
       up (0 1 0)
       fov 180
       aa 4)))

(define brown
  (make-material
    '( diffuse (0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define grey85
  (make-material
    '( diffuse (0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define chrome
  (make-material
    '( diffuse (0.8 0.8 0.8)
       kd 0.2
       specular (1.0 1.0 1.0)
       ks 0.8
       specpow 30)))

(define red
  (make-material
    '( diffuse (0.8 0.2 0.3)
       kd 0.5
       specular (1.0 1.0 1.0)
       ks 0.5
       specpow 30)))

(define green
  (make-material
    '( diffuse (0.2 0.8 0.3)
       kd 0.5
       specular (1.0 1.0 1.0)
       ks 0.5
       specpow 30)))

(define blue
  (make-material
    '( diffuse (0.3 0.2 0.8)
       kd 0.5
       specular (1.0 1.0 1.0)
       ks 0.5
       specpow 30)))

(define yellow
  (make-material
    '( diffuse (0.8 0.9 0.2)
       kd 0.5
       specular (1.0 1.0 1.0)
       ks 0.5
       specpow 30)))

(add-to-scene (list
  (make-pointlight '(10 1300 10))
  (make-pointlight '(-500 1300 1300))))


;(add-to-scene
;    (make-box 
;      '(-1700 -51 -1700) 
;      '(1700 1 1700) 
;      brown))
;
;(add-to-scene
;    (make-box 
;      '(-1700 -51 1) 
;      '(1700 3 10) 
;      grey85))

;(add-to-scene
;    (make-box 
;      '(-1700 -51 -10) 
;      '(1700 3 -1) 
;      grey85))


(add-to-scene
(translate
  (make-rounded-wire-box '(-300 0 -300) '(300 300 300) 20 chrome)
  '(-350 0 -350)))
(add-to-scene
(translate
  (make-rounded-wire-box '(-300 0 -300) '(300 300 300) 20 chrome)
  '(350 0 -350)))
(add-to-scene
(translate
  (make-rounded-wire-box '(-300 0 -300) '(300 300 300) 20 chrome)
  '(-350 0 350)))
(add-to-scene
(translate
  (make-rounded-wire-box '(-300 0 -300) '(300 300 300) 20 chrome)
  '(350 0 350)))

(add-to-scene
 (list
 (make-sphere '(0 0 500) 200 red)
 (make-sphere '(0 0 -500) 200 green)
 (make-sphere '(500 0 0) 200 yellow)
 (make-sphere '(-500 0 0) 200 blue)
 ))



