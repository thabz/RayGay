
(load "lib/raygay.scm")

; 00:31.78

(define balls 20000)
(define boxsize 1200)
(define radius 5)    

    

(set-image-size '(1024 768))
(set-background #(0.94 0.7 0.06))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(0 0 400)
       lookat #(0 0 -100)
       up #(0 1 0)
       fov 45
       aa 3)))

(define ground
  (make-material
    '( diffuse #(1.0 1.0 0.7)
       kd 0.9
       specular #(1.0 1.0 1.0)
       ks 0.1
       specpow 15)))

(define sphere-mat 
  (make-material
    '( diffuse #(1.0 1.0 0.7)
       kd 1.0)))

;(add-to-scene (make-arealight #(500 500 1300) #(-0.5 -0.5 -1) 500 80 0.1))
(add-to-scene (make-pointlight #(500 500 1300)))

(define half-boxsize (/ boxsize 2))

(do ((i 0 (+ i 1)))
 ((= i balls))
    (let ((x (random2 (- half-boxsize) half-boxsize))
          (y (random2 (- half-boxsize) half-boxsize))
	  (z (random2 (- half-boxsize) half-boxsize)))
      (add-to-scene
          (make-sphere
       	    (vector x y z)
    	      radius
    	      sphere-mat))))
