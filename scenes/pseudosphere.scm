
(load "lib/raygay.scm")
(load "lib/colors.scm")

; http://www.geom.uiuc.edu/zoo/diffgeom/pseudosphere/eqns.html

(set-image-size image-size-1080-hd)
(set-image-size image-size-360-hd)

(set-background (color 'tan))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(15 5 7.5)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 0)))

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
       (make-texture "gfx/chess.png" 1 1 'none)
       'kd 0.2
       'specular #(1.0 1.0 1.0)
       'ks 0.8
       'specpow 30)))

(add-to-scene (make-pointlight #(500 1000 1000)))
;(add-to-scene (make-pointlight #(-500 1500 1300)))
;(add-to-scene (make-arealight #(500 1000 1000) #(-0.5 -1 -1) 200 256 0.1))

;(add-to-scene (make-box #(-1700 -51 -1700) #(1700 1 1700) brown))

;(add-to-scene (make-continuous-studio-backdrop #(800 400 400) 15 brown))  

(define (pseudo-sphere _u _v)
 (let* ((b 0.01)
	(u (* 2 π _u))
;        (v (+ b (* π _v (- 1 (* 2 b)))))
        (v (* 0.99 _v))
	(a 10000)
	(d (+ (atan (- a) (/ π 2 ))))
	(w (+ (atan (- (* 2 a v (/ 1 π)) a)) (/ π 2 )))
	(v1 (+ (/ (* (- π (* 2 b))(- w d)) (- π (* 2 d))) b))
        (x (* 4 (sin u) (sin v1)))
        (y (* 4 (cos u) (sin v1)))
	(z (* 4 (+ (cos v1) (log (tan (/ v1 2)))))))
  (vector x y z)))

(add-to-scene 
 (make-parametrized-surface pseudo-sphere 100 100 #f #f checkered))

