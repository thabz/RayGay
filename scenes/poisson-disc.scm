
(load "lib/raygay.scm")

(set-image-size '(1024 1024))
(set-background '(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (10 1000 3000)
       lookat (0 0 0)
       up (0 1 0)
       fov 45
       aa 3)))

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


(add-to-scene (make-pointlight '(500 1300 1300)))

(define img (make-texture "gfx/larry.jpg" 1.0 1.0 "none"))

(define num 48)

    
(define radius 20)
(define w 2000)    
(define num 2000)
(define pset (make-poisson-disc-set w w radius num))

(display (length pset))    
(newline)    

(do ((i 0 (+ i 1)))
  ((= (length pset) i) i)
 ; (display (list-ref pset i))
 ; (newline)
  (add-to-scene
    (make-sphere 
      (v- (append (list-ref pset i) '(0))
	  (list (* 0.5 w) (* 0.5 w) (* 0.5 w)))
   radius
   brown)))


   
