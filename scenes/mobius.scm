
(load "lib/objects/make-pill.scm")

(set! image-size '(640 480))
(set! background '(0.3 0.6 0.7))

(set! renderer "raytracer")
(set! camera 
  (make-pinhole-camera 
    '( pos (1000 1000 2000)
       lookat (0 100 0)
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
(append! scene (list (make-pointlight '(-500 1500 1300))))

(append!
  scene 
  (list 
    (make-box 
      '(-1700 -51 -1700) 
      '(1700 1 1700) 
      brown)))

(define num 50)
(define twirls 1.5)

(let iter ((i num))
  (if (not(= i 0))
    (begin
      (append! 
	scene 
	(rotate 
	  (translate 
	    (rotate
	      (make-pill '(0 -200 0) '(0 200 0) 20 chrome)
	      '(1 0 0) 
	      (* i (/ (* 360 twirls) num)))
	    '(0 250 500))
	  '(0 1 0 )
	  (* i (/ 360 num))))
      (iter (- i 1)))))


(display "End of mobius.scm")
(newline)
