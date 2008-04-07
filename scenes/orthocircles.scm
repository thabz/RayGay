; The orthocircles implicit surface is defined by
; ((xx + yy ñ 1)≤ + zz) * ((yy + zz ñ 1)≤ + xx) * ((zz + xx ñ 1)≤ + yy) ñ 
; 0.0375≤ * (1 + 3 * (xx + yy + zz)) = 0 
;
; See http://www.flickr.com/photos/mylaboratory/417717535/in/set-72157603650366417/


(load "lib/raygay.scm")
(load "lib/colors.scm")

(set-image-size image-size-360-hd)
(set-image-size image-size-1080-hd)

(set-background (color 'azure))

(set-renderer "raytracer")
(set-renderer "pathtracer")
(set-camera 
  (make-pinhole-camera (list 
       'pos #(2 3 6)
       'lookat #(0 0 0)
       'up #(0 1 0)
       'fov 45
       'sampler (make-halton-sampler 4000)
       )))

(define brown
  (make-material (list
    'diffuse (color 'cornsilk)
    'kd 1.0
    'ks 0.0)))

(define grey85
  (make-material (list
     'diffuse (color 'burlywood)
     'kd 1.0
     'ks 0.0)))

(define chrome
  (make-material (list 
       'diffuse (color 'white)
       'kd 0.4
       'specular (color 'white)
       'ks 0.6
       'specpow 15)))


(define (sqar n) (* n n))

(define (orthocircles x y z)
  (let ((xx (* x x))
        (yy (* y y))
        (zz (* z z)))
   (- (* (+ (sqar (+ xx yy -1)) zz)
         (+ (sqar (+ yy zz -1)) xx)
         (+ (sqar (+ zz xx -1)) yy))
      (* (sqar 0.0375)
         (+ 1 (* 3 (+ xx yy zz)))))))

(add-to-scene (make-arealight #(1300 1300 1300) #(-1 -1 -1) 200 256 0.1))

(add-to-scene 
  (translate 
  (make-continuous-studio-backdrop #(12 10 10) 0.01 grey85)
#(0 -2 -2)))

;(add-to-scene (make-sphere #(0 0 0) 0.5 chrome)) 

(add-to-scene  
 (translate 
  (make-marching-cubes
      (make-isosurface 
        orthocircles
        #(-1 -1 -1)
        #(1 1 1)
        0.0         ; iso-value
        100         ; steps
        0.000001    ; accuracy
        brown) 
    200 #t)
#(0 0 0))) 
