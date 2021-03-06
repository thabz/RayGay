
(load "lib/raygay.scm")

(define balls 3000)
(define size 3)
(define radius (* 100 100 100))    

(set-image-size '(640 480))
(set-background #(0.94 0.7 0.06))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(0 600 1800)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 2)))

(define ground
  (make-material
    '( diffuse #(1.0 1.0 0.7)
       kd 0.9
       specular #(1.0 1.0 1.0)
       ks 0.1
       specpow 15)))

(add-to-scene (make-arealight #(500 500 1300) #(-0.5 -0.5 -1) 500 80 0.1))

(add-to-scene
  (make-box #(-1000000.0 -500.0 -1000000.0) #(1000000.0 -400.0 1000000.0) ground))


(let loop ((i 0))
  (if (not(= i balls) )
    (let* ((x (random (- size) size))
 	         (y (random (- size) size))
	         (z (random (- size) size))
 	         (v (- (expt (+ (* 2 x x) (* y y) (* z z) -1) 3)
		             (* x x z z z 0.1)
		             (* y y z z z))))
      (if (negative? v)
	(begin
	 (add-to-scene
	      (make-ellipsoid 
		(vector (* y 400) (* z 400) (* x 400))
		(vector (random 10 30) (random 10 30) (random 10 30))
		(make-material
		  (list 
		    'diffuse
		    (vector (random 0.9 1) (random 0.3 0.5) (random 0.3 0.35))
		    'kd 0.8
		    'specular (vector 0.5 0.5 0.5)
		    'ks 0.2
		    'specpow 25))))
	  (loop (+ i 1)))
	(loop i)))))


