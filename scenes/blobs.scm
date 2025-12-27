; A benchmark scene for blob rendering.
;
; Rendertimes
; 2005-04-01:	5:07
; 2005-04-03:	1:15

(load "lib/raygay.scm")

(set-image-size '(1024 768))
(set-background (make-texture "gfx/desert.jpg" 1 1 'bilinear))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(40 0 40)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 1)))

(define chrome
  (make-material
    '( diffuse #(0.7 0.7 0.7)
       kd 0.1
       specular #(1.0 1.0 1.0)
       ks 0.7
       specpow 20)))


(add-to-scene (make-pointlight #(-500 1300 1300)))

(define radius 4.5)
(define weight 1)
(define num 30)

(define atoms '())

(dotimes i num
   (set! atoms (cons (list (vector (random -5 5) (random -5 5) (random -5 5)) 
                           radius weight) 
                     atoms)))

;(for-each display atoms)
        
(add-to-scene
    (make-blob
      1.0  ; iso
      50    ; steps
      0.0001 ; accuracy
      chrome
      atoms))

