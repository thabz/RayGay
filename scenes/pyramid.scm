
(set-image-size '(800 600))
(set-background #(0.95 0.8 0.06))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (1000 500 1800)
       lookat (250 180 0)
       up (0 1 0)
       fov 45
       aa 1)))

(define ground
  (make-material
    '( diffuse (0.8 0.3 0.264)
       kd 0.5
       specular (1 1 1)
       ks 0.0
       specpow 15)))

(define red
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

(add-to-scene (make-pointlight '(100 1300 1300)))

(add-to-scene (make-sphere '(0 -100000 0) 100000 ground))

(define layers 8)
(define width 1000)
(define o (/ width layers))

(define barsize 7)
(define jointsize 30)
(define ballsize 40)

(dotimes h layers
  (let ((p (* o (- layers h) 0.5)))
    (do times x h           
      (do times z h
        (let* ((top-center (vector (+ p (* x o))
      	                           (* o 0.7 (- layers h))
    	                           (+ p (* z o))))
	       (right-front (v+ top-center (vector (* 0.5 o) (* o 0.7) (* 0.5 o))))
	       (left-front  (v+ top-center (vector (* -0.5 o) (* o 0.7) (* 0.5 o))))
	       (right-back  (v+ top-center (vector (* 0.5 o) (* o 0.7) (* -0.5 o))))
	       (left-back   (v+ top-center (vector (* -0.5 o) (* o 0.7) (* -0.5 o)))))

          (make-sphere top-center jointsize chrome)
           
	  (unless (= x (- h 1))
            (cylinder top-center ...))
          (unless (or (= x (- h 1)) (= z 0))
	    (cylinder top-center ...)
	    (sphere (v- top-center `#(,(* 0.5 o) 0 (* -0.5 o))) red))
          (unless (or (= x 0) (= z 0))
            (cylinder top-center left-back barsize chrome))
          (unless (= z (- h 1))
            (cylinder top-center ...))
          (unless (or (= z (- h 1)) (= x 0))
            (cylinder top-center left-front barsize chrome))
          (unless (or (= z (- h 1)) (= x (- h 1)))
            (cylinder top-center right-front barsize chrome))))))

