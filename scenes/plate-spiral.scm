
(load "globals.scm")
(load "objects.scm")

(set! image-size '(1024 768))
;(set! background '(0.3 0.6 0.7))
(set! background (make-texture "gfx/goodmorning.jpg" 1 1 "bilinear"))

(set! renderer "raytracer")
(set! camera 
  (make-pinhole-camera 
    '( pos (-2700 2700 20)
       lookat (0 -200 0)
       up (0 1 0)
       fov 45
       dof (150.0 500 (-750 0 0))
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


(set! scene (list (make-pointlight '(500 1300 1300))))

(define num 70)
(define twists 2)    

(let loop ((i num))
  (if (positive? i)
    (begin
      (append! scene
	       (rotate 
		 (translate 
		   (rotate 
		     (make-rounded-plate 150 20 chrome)
		     '(0 0 1) (* twists 360 (/ i num)))
		   '(600 0 0))
		 '(0 1 0) (* (/ i num) 360)))
      (loop (- i 1)))))


