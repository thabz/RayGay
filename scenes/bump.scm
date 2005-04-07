
(load "globals.scm")
(load "lib/objects/make-rounded-box.scm")

(set! image-size '(1024 768))
;(set! background (make-texture "gfx/goodmorning.jpg" 1 1 "bilinear"))
(set! background '(0.76 0.62 0.42))

(set! renderer "raytracer")
(set! camera 
  (make-pinhole-camera 
    '( pos (4 15 40)
       lookat (0 0 0)
       up (0 1 0)
       fov 45
       aa 4)))

(define (perturb-noise P N)
 (let ((pnoise (noise3d (vscale P 5) 0.001)))
  (v+ N (vscale pnoise 0.1))))
    
(define (perturb P N)
  (list (+ (.x N) (* 0.05 (+ 1 (sin (* (.x P) 6 )))))
	(+ (.y N) (* 0.05 (+ 1 (sin (* (.y P) 5 )))))
	(+ 0.0 (.z N))))

(define chrome
  (make-material
    (list 'diffuse '(1.0 1.0 1.0)
	  'kd 0.4
	  'specular '(1.0 1.0 1.0)
	  'ks 0.6
       'specpow 35
       'normal perturb-noise 
       )))

(define brown1
 (make-material
  '( diffuse (0.76 0.62 0.42)
     kd 1.0
     ks 0.0)))

(define brown2
 (make-material
  '( diffuse (0.51 0.36 0.14)
     kd 1.0
     ks 0.0)))



(set! scene (list (make-arealight '(1300 1300 1300) '(-1 -1 -1) 200 80 0.1)))
(append! scene (list (make-arealight '(1300 1300 1300) '(-1 -1 -1) 200 80 0.1)))

(append!
  scene
  (list 
   (make-sphere '(8 2 8) 2 chrome)
   (make-cylinder '(-8 0 9) '(-8 4 9) 3 chrome)
   (translate
    (make-torus 3 1 chrome)
    '(2 1 14))
   (make-sphere '(0 4 0) 4 chrome)))

(define num 20)
(define offset 4)
(define width (- offset 0.02))
(define radius (/ width 10))

(let loopx
  ((x 0))
  (if (< x num)
    (begin
      (let loopz
	((z 0))
	(if (< z num)
	  (begin 
	    (append! scene
		     (translate
		       (make-rounded-box 
			 (list (* offset x) 0 (* offset z))
			 (list (+ width (* offset x)) 
			       width 
			       (+ width (* offset z)))
			 radius
			 (if (eq? 
			       (modulo x 2)
			       (modulo z 2))
			   brown1
			   brown2))
		       (list (* num width -0.5) (- width) (* num width -0.5))))
	    (loopz (+ 1 z)))))
      (loopx (+ 1 x)))))




