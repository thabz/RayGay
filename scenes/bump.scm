
; TODO: Use vectors #(x y z) instead of lists (x y z) for Vector

(load "lib/raygay.scm")
(load "lib/objects/make-rounded-box.scm")
(load "lib/objects/make-rounded-cylinder.scm")

;(set-image-size #(1600 1200))
(set-image-size '(640 480))
;(set! background (make-texture "gfx/goodmorning.jpg" 1 1 'bilinear))
(set-background #(0.76 0.62 0.42))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(4 15 40)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 0)))

(define (perturb-noise P N)
 (let ((pnoise (noise3d (vscale P 5) 0.001)))
  (v+ N (vscale pnoise 0.1))))
    
(define (perturb P N)
  (vector (+ (.x N) (* 0.05 (+ 1 (sin (* (.x P) 6 )))))
	  (+ (.y N) (* 0.05 (+ 1 (sin (* (.y P) 5 )))))
	  (+ 0.0 (.z N))))

(define chrome
  (make-material
    `(diffuse  #(1.0 1.0 1.0)
      kd 0.4
      specular #(1.0 1.0 1.0)
      ks 0.6
      specpow 35
      normal ,perturb-noise)))

(define brown1
 (make-material
  '( diffuse #(0.76 0.62 0.42)
     kd 1.0
     ks 0.0)))

(define brown2
 (make-material
  '( diffuse #(0.81 0.71 0.54)
     kd 1.0
     ks 0.0)))

(add-to-scene (make-arealight #(1300 1300 1300) #(-1 -1 -1) 200 80 0.1))
;(add-to-scene (make-arealight #(1300 1300 1300) #(-1 -1 -1) 200 80 0.1))

(add-to-scene
    (make-sphere #(8 2 8) 2 chrome)
    (translate
      (make-torus 3 1 chrome)
      #(2 1 14))
    (make-sphere #(0 4 0) 4 chrome))

(add-to-scene
 (translate
  (make-rounded-cylinder 5 3 0.1 chrome)
  #(-8 0 9)))

(define num 20)
(define offset 4)
(define width (- offset 0.02))
(define radius (/ width 10))

; Tiled floor
(dotimes x num
   (dotimes z num        
      (add-to-scene
        (translate
           (make-rounded-box 
	     (vector (* offset x) 0 (* offset z))
	     (vector (+ width (* offset x)) width (+ width (* offset z)))
	     radius
	     (if (eq? (modulo x 2) (modulo z 2))
	       brown1
	       brown2))
	   (vector (* num width -0.5) (- width) (* num width -0.5))))))
