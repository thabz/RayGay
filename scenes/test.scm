
(load "globals.scm")
(load "lib/objects/make-rounded-cylinder.scm")

(set! image-size '(1600 1200))
;(set! background (make-texture "gfx/goodmorning.jpg" 1 1 'bilinear))
;(set! background (make-texture "gfx/alltrans.png" 1 1 'none))
(set! background '(0.337 0.417 0.527))

(set! renderer "raytracer")
(set! camera 
  (make-pinhole-camera 
    '( pos (2 10 20)
       lookat (0 2.5 0)
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
	  'kd 0.8
	  'specular '(1.0 1.0 1.0)
	  'ks 0.2
       'specpow 35
       ;'normal perturb-noise 
       )))


(set! scene (list (make-pointlight '(300 1300 1300))))

(append!
  scene
   (make-rounded-cylinder 7 5 0.05 chrome))
