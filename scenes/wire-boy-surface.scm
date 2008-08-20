
(load "lib/raygay.scm")
(load "lib/colors.scm")
(load "lib/hyperbolic-functions.scm")
(load "lib/objects/wireframing.scm")

(set-image-size image-size-360-hd)
(set-image-size image-size-720-hd)
(set-image-size image-size-1080-hd)
(set-image-size image-size-24-inch-intel-imac)

(set-background (color 'white))
(set-background (color 'blanched-almond))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(4.5 1.5 2.75 )
       ;pos #(0 4.5 3.5)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 35
       aa 3)))

(define brown
  (make-material
    (list 'diffuse (color 'tan)
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
       (make-texture "gfx/chess.png" 40 20 'none)
       'kd 1.0
       'specular #(1.0 1.0 1.0)
       'ks 0.0
       'specpow 30)))

;(add-to-scene (make-pointlight #(500 1000 1000)))
(add-to-scene (make-arealight #(500 1000 1000) #(-0.5 -1 -1) 200 256 0.1))

(define (reparameterize x a b)
 (+ a (* x (- b (* 2 a)))))

;; Equation found by Apéry (1986).
;; See http://mathworld.wolfram.com/BoySurface.html 
(define (boy-surface _u _v)
  (let* ((u (reparameterize _u 0 π))
      	 (v (reparameterize _v 0 π))
      	 (x (* (cos u) (sin v)))
      	 (y (* (sin u) (sin v)))
      	 (z (cos v))
      	 (x² (* x x))
      	 (y² (* y y))
      	 (z² (* z z)))  
    (vector
      (* 1/2 (+ (* (- (* 2 x²) y² z²)
                   (+ x² y² z²))
                (* 2 y z (- y² z²))
                (* z x (- x² z²))
                (* x y (- y² x²)))) 
      (* 1/2 (sqrt 3) (+ (* (- y² z²) (+ x² y² z²))
                         (* z x (- z² x²))
                         (* x y (- y² x²)))) 
      (* 1/8 (+ x y z) (+ (expt (+ x y z) 3) 
                        (* 4 (- y x) (- z y) (- x z)))))))

(define (flower _u _v)
  (let* ((u (reparameterize _u 0 (* 2 π)))
      	 (v (reparameterize _v 0.1 1)))
  (vector
    (* v (cos u))
    (* v (sin u))
    (* v (cos (* 7 u)) 1/4))))

;(add-to-scene (make-parametrized-surface boy-surface 100 100 #f #f checkered))

(add-to-scene (make-parametric-surface-as-wireframe boy-surface 120 60 200 200 0.004 brown))
