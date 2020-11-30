; A benchmark scene for blob rendering.
;
; Rendertimes
; 2005-04-01:	5:07
; 2005-04-03:	1:15

(load "lib/raygay.scm")

(set-image-size '(1024 768))
(set-background (make-texture "gfx/goodmorning.jpg" 1 1 'bilinear))
;(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(10 17 20)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 0)))

(define chrome
  (make-material
    '( diffuse #(0.9 0.7 0.8)
       kd 0.4
       specular #(1.0 1.0 1.0)
       ks 0.6
       specpow 45)))

(define flat
  (make-material
    '( diffuse #(0.9 0.7 0.8)
       kd 0.9
       specular #(1.0 1.0 1.0)
       ks 0.1
       specpow 45)))


(add-to-scene (make-pointlight #(-500 1300 1300)))

(define union-spheres (make-union 
  (make-sphere #(-5 0 0) 2.5)
  (make-sphere #(0 0 0) 2.5)
  (make-sphere #(5 0 0) 2.5)))

(define solid-box (make-solid-box
  #(-2 -2 -2)
  #(2 2 2)))

(add-to-scene
    (make-sdf-object
      solid-box
      0.02  ; grow
      100    ; steps
      0.0001 ; accuracy
      flat))

