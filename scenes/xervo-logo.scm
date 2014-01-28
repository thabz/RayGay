
(load "lib/raygay.scm")
(load "lib/mesh.scm")

(define preview-quality #f)

(set-background #(1 1 1 0))

(if preview-quality
  (set-image-size '(512 512))
  (set-image-size '(4096 4096)))

(set-renderer "raytracer")

(set-camera 
  (make-pinhole-camera 
    `(pos #(260.5 80 260)
      lookat #(100 40 0)
      up ,y-axis
      fov 45
      aa 3)))

(define grey
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 1.0
       ks 0.0)))
       
(define trans
  (make-material
    '( diffuse #(1.0 1.0 1.0)
       kd 1.0
       alpha-shadows #t
       ks 0.0)))       

(define red
  (make-material
    '( diffuse #(1.0 0.0 0.0)
       kd 1.0
       ks 0.0)))       

;(add-to-scene (make-pointlight #(500 1300 1300)))
;(add-to-scene (make-skylight 10 250 #(1 1 1)))
(add-to-scene (make-arealight #(1000 4000 4000) #(-0.5 -1 -1) 700 (if preview-quality 100 300) 0.1))

; Transparent floor to receive the shadow
(add-to-scene
  (make-solid-box 
    #(-200 -20 -200) #(200 -0.5 200) trans))

(define d 37.67)

(define y1 0)
(define y2 20.052)
(define y3 (+ 12.84 y2))
(define y4 (+ 20.052 y3))
(define y5 (+ d y4))

(define x1 0)
(define x2 (- 74.4 20.052))
(define x3 74.4)
(define x4 (+ d x3 0.1))

(define bottom-box-points (list
  (vector 0 y1 0) (vector 0 y1 d) (vector 74.4 y1 0) (vector 74.4 y1 d)	
  (vector 0 y2 0) (vector 0 y2 d) (vector 74.4 y2 0) (vector 74.4 y2 d)))	
(define top-box-points (list
  (vector 0 y3 0) (vector 0 y3 d) (vector 74.4 y3 0) (vector 74.4 y3 d)	   
  (vector 0 y4 0) (vector 0 y4 d) (vector 74.4 y4 0) (vector 74.4 y4 d)))	
(define vertical-box-points (list
  (vector x2 y1 0) (vector x3 y1 0) (vector x2 y1 d) (vector x3 y1 d)
  (vector x2 y4 0) (vector x3 y4 0) (vector x2 y4 d) (vector x3 y4 d)))

(define edge-radius 0.5)

(add-to-scene (make-rounded-box (vector 0 y1 0) (vector x3 y2 d) edge-radius red))
(add-to-scene (make-rounded-box (vector 0 y3 0) (vector x3 y4 d) edge-radius red))
(add-to-scene (make-rounded-box (vector x2 y1 0) (vector x3 y4 d) edge-radius red))
(add-to-scene (make-rounded-box (vector x3 y4 0) (vector x4 y5 d) edge-radius red))
