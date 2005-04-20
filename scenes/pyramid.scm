
(define layers 8)
(define width 1000)
(define o (/ width layers))

(set! image-size '(800 600))
(set! background '(0.95 0.8 0.06))

(set! renderer "raytracer")
(set! camera 
  (make-pinhole-camera 
    '( pos (1000 500 1800)
       lookat (250 180 0)
       up (0 1 0)
       fov 45
       aa 4)))

(define ground
  (make-material
    '( diffuse (0.8 0.3 0.264)
       kd 0.5
       specular (1 1 1)
       ks 0.0
       specpow 15)))

(define gred
  (make-material
    '( diffuse (0.9 0.2 0.01)
       kd 0.8
       specular (0.5 0.5 0.5)
       ks 0.2
       specpow 45)))

(define chrome
  (make-material
    '( diffuse (0.85 0.85 0.85)
       kd 0.5
       specular (1.0 1.0 1.0)
       ks 0.5
       specpow 45)))

(set! scene (list (make-pointlight '(100 1300 1300))))

(append! scene
 (list (make-sphere '(0 -100000 0) 100000 ground)))

    (define barsize 7)
    (define jointsize 30)
(define ballsize 40)

    (let h-loop 
     ((h 0))
     (if (< h layers)
      (begin
       (let x-loop 
	((p (* o (- layers h) 0.5))
	 (x 0))
	(if (< x h)
	 (begin
	  (let z-loop
	   ((z 0))
	   (if (< z h)
	    (begin
	     (make-sphere 
	      (list (+ p (* x o))
	       (* o 0.7 (- layers h))
	       (+ p (* z o)))
	      jointsize chrome) 
	     (if (not (= x (- h 1)))
	      (make-cylinder 
	       (list 
		(+ p (* x o))
		(* o 0.7 (- layers h))
		(+ p (* z o)))
	       (list
		(+ p (* o (+ x 1)))
		(* o 0.7 (- layers h))
		(+ p (* z o)))
	       barsize chrome))
	     (if (and 
		  (not (= x (- h 1)))
		  (not (= z 0)))
	      (begin
	       (cylinder)
	       (sphere)))
    (if (and 
	 (not (= x 0))
	 (not (= z 0)))
     (cylinder))
    (if (not (= z (- h 1)))
     (cylinder))
    (if (and
	 (not (= z (- h 1)))
	 (not (= x 0)))
     (cylinder))
    (if (and 
	 (not (= z (- h 1)))
	 (not (= x (- h 1))))
     (cylinder))
    (z-loop (+ 1 z)))))
    (x-loop (+ 1 x)))))
(h-zoop (+ 1 h)))))



