(load "lib/raygay.scm")

(set-image-size '(1024 1024))
(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(10 1000 3000)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 4)))

(define brown
  (make-material
    '( diffuse #(0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define grey85
  (make-material
    '( diffuse #(0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


(add-to-scene (make-pointlight #(500 1300 1300)))

(define img (make-texture "gfx/larry.jpg" 1.0 1.0 'none))

(define num 48)

(do ((y (- num) (+ 1 y)))
  ((= y num))
  (do ((x (- num) (+ 1 x)))
    ((= x num))
    (add-to-scene
      (make-sphere `#(,(* x 20) ,(* y -20) 0) 15.0
	(make-material 
	  (list 
	    'diffuse 
	    (texture-get-pixel 
	      img 
	      (/ (+ x num) (* 2 num)) 
	      (/ (+ y num) (* 2 num)))
	    'kd 1.0
	    'ks 0.0))))))

