; A benchmark scene for blob rendering.
;
; Rendertimes
; 2005-04-01:	5:07
; 2005-04-03:	1:15

(load "lib/raygay.scm")
(load "lib/image-sizes.scm")

;(set-image-size image-size-4k-hd)
(set-image-size image-size-1080p-hd)
;(set-image-size image-size-720p-hd)
(set-background (make-texture "gfx/goodmorning.jpg" 1 1 'bilinear))
;(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(8 17 20)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 3)))

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

(define flatter
  (make-material
    '( diffuse #(0.7 0.8 0.9)
       kd 1
       specular #(1.0 1.0 1.0)
       ks 0
       specpow 45)))


;(add-to-scene (make-pointlight #(-500 1300 1300)))
(add-to-scene (make-arealight #(500 1300 1300) #(-0.5 -1 -1) 500 80 0.1))

(define union-spheres (make-union 
  (make-sphere #(-5 2.5 0) 2.5)
  (make-sphere #(0 2.5 0) 2.5)
  (make-sphere #(5 2.5 0) 2.5)))

(define solid-box (make-difference 
  (make-solid-box #(-2 0.2 -2 ) #(2 4.2 2))
  (make-solid-box #(-1 -1 -1 ) #(1 5 1))))

(define rotated (rotate-y solid-box (* clock 360)))
;(define rotated (rotate-y solid-box (* 0 360)))

(add-to-scene
    (make-sdf-object
      rotated
      0.1  ; grow
      0.001 ; accuracy
      flat))

(add-to-scene
  (make-halfspace #(0 1 0) 0 flatter))
