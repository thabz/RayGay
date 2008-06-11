
(load "lib/raygay.scm")
(load "lib/colors.scm")
(load "lib/hyperbolic-functions.scm")

; http://www.geom.uiuc.edu/zoo/diffgeom/pseudosphere/eqns.html

(set-image-size image-size-360-hd)
(set-image-size image-size-1080-hd)

(set-background (color 'tan))
(set-background (color 'white))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(15 5 7.5)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 3)))

(define brown
  (make-material
    (list 'diffuse (color 'moccasin)
          'kd 1.0
          'ks 0.0)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))

(define checkered 
  (make-material
    (list 'diffuse 
       (make-texture "gfx/chess.png" 12 5 'none)
       'kd 1.0
       'specular #(1.0 1.0 1.0)
       'ks 0.0
       'specpow 30)))

(add-to-scene (make-pointlight #(500 1000 1000)))
;(add-to-scene (make-pointlight #(-500 1500 1300)))
;(add-to-scene (make-arealight #(500 1000 1000) #(-0.5 -1 -1) 200 256 0.1))

;(add-to-scene (make-box #(-1700 -51 -1700) #(1700 1 1700) brown))

;(add-to-scene (make-continuous-studio-backdrop #(800 400 400) 15 brown))  

(define (reparameterize x a b)
 (+ a (* x (- b (* 2 a)))))


(define (pseudo-sphere _u _v)
  (let* ((u (reparameterize _u -5 5))
	 (v (reparameterize _v 0 (* 2 π)))
	 (x (* 4 (sech u) (cos v)))
	 (y (* 4 (sech u) (sin v)))
	 (z (* 4 (- u (tanh u)))))
   (vector x y z)))

(add-to-scene 
 (make-parametrized-surface pseudo-sphere 400 100 #f #f checkered))

