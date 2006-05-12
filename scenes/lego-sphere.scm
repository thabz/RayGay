
(load "lib/raygay.scm")

(set-image-size '(640 480))
(set-image-size '(1600 1200))
(set-background '(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (190 190 1000)
       lookat (0 0 0)
       up (0 1 0)
       fov 50
       aa 4)))

(define brown
  (make-material
    '( diffuse (0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define blue 
  (make-material
    '( diffuse (0.3 0.6 0.7)
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


(add-to-scene (list
;  (make-pointlight '(500 1300 1300))
  (make-pointlight '(-500 1300 1300))))

;(add-to-scene
;    (make-box 
;      '(-1700 -51 -1700) 
;      '(1700 1 1700) 
;      brown))

(define (square x)
  (* x x))

(define num 5)

(do ((z (- num) (+ z 1)))
  ((= z num))
  (do ((y (- num) (+ y 1)))
    ((= y num))
    (do ((x (- num) (+ x 1)))
      ((= x num))
      (if (< (+ (* x x) (* y y) (* z z))
	     (square num))
	(add-to-scene
	  (make-rounded-box
	    (list (* x 20) 
		  (* y 20) 
		  (* z 20))
	    (list (* (+ x 0.8) 20)
		  (* (+ y 0.8) 20) 
		  (* (+ z 0.8) 20))
	    5 blue))))))


