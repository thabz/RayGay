
(load "lib/objects/make-venus.scm")

(set! image-size '(1024 768))
;(set! background (make-texture "gfx/goodmorning.jpg" 1 1 "bilinear"))
(set! background '(0.3 0.6 0.7))

(set! renderer "raytracer")
(set! camera 
  (make-pinhole-camera 
    '( pos (1 10 20)
       lookat (0 0 0)
       up (0 1 0)
       fov 45
       aa 4)))

(define chrome
  (make-material
    (list 'diffuse '(0.9 0.7 0.8)
	  'kd 0.8
	  'specular '(1.0 1.0 1.0)
	  'ks 0.2
       'specpow 35
       )))


(set! scene (list (make-pointlight '(1300 1300 1300))))

(append!
  scene
  (make-venus chrome))
    
