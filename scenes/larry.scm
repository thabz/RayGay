
(load "globals.scm")
(load "objects.scm")

(set! image-size '(1024 1024))
(set! background '(0.3 0.6 0.7))

(set! renderer "raytracer")
(set! camera 
  (make-pinhole-camera 
    '( pos (10 1000 3000)
       lookat (0 0 0)
       up (0 1 0)
       fov 45
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
;(append! scene (list (make-pointlight '(-500 1500 1300))))

(define img 
 (make-texture 
  "gfx/larry.jpg" 1.0 1.0 "none"))

(define num 48)

(let itery ((y (* -1 num)))
  (if (not(= y num))
    (begin 
      (let iterx ((x (* -1 num)))
	(if (not(= x num))
	  (begin
	    (append! 
	      scene
	      (list 
		(make-sphere
		  (list (* x 20) (* y -20) 0) 15.0
		  (make-material 
		    (list 
		      'diffuse 
		      (get-pixel 
			img 
			(/ (+ x num) (* 2 num)) 
			(/ (+ y num) (* 2 num)))
		      'kd 1.0
		      'ks 0.0)))))
	    (iterx (+ x 1)))))
      (itery (+ y 1)))))



(display "End of patch.work")
(newline)
