;; http://en.wikipedia.org/wiki/Icosahedron

(load "lib/raygay.scm")
(load "lib/objects/wireframing.scm")
(load "lib/colors.scm")

(set-background #(0.95 0.95 0.95))

(set-image-size '(1600 1200))
(set-image-size '(800 600))
(set-renderer "raytracer")

(set-camera 
  (make-pinhole-camera 
    `(pos #(2 1 4)
      lookat #(0 0 0)
      up ,y-axis
      fov 45
      aa 0)))

(define chrome
  (make-material
    '( diffuse #(0.9 0.8 0.8)
       kd 0.5
       kt 0.5
       ks 0.0)))

(add-to-scene (make-pointlight #(500 1300 1300)))
;(add-to-scene (make-skylight 10000 512 #(1 1 1)))  
;(add-to-scene (make-arealight #(1000 2000 2000) #(-0.5 -1 -1) 500 64 0.1))

(define (mat t)
(make-material
  `( diffuse ,(hsv->rgb (vector t 0.5 0.5))
     kd 0.3
     specular #(1.0 1.0 1.0)
     ks 0.1
     kt 0.3
     specpow 30)))

(define φ (/ (+ 1 (sqrt 5)) 2 ))  ; The golden ratio

(define verts (vector
   #(-0.525731 0.000000 -0.850651 1.000000)
   #(0.525731 0.000000 -0.850651 1.000000)
   #(0.525731 0.000000 0.850651 1.000000)
   #(-0.525731 0.000000 0.850651 1.000000)
   #(-0.850651 -0.525731 0.000000 1.000000)
   #(-0.850651 0.525731 0.000000 1.000000)
   #(0.850651 0.525731 0.000000 1.000000)
   #(0.850651 -0.525731 0.000000 1.000000)
   #(0.000000 -0.850651 0.525731 1.000000)
   #(0.000000 -0.850651 -0.525731 1.000000)
   #(0.000000 0.850651 -0.525731 1.000000)
   #(0.000000 0.850651 0.525731 1.000000)))

(define faces '(
   (2 10 1)
   (11 2 1)
   (6 11 1)
   (5 6 1)
   (10 5 1)
   (9 3 4)
   (5 9 4)
   (6 5 4)
   (12 6 4)
   (3 12 4)
   (12 3 7)
   (11 12 7)
   (2 11 7)
   (8 2 7)
   (3 8 7)
   (12 11 6)
   (10 9 5)
   (8 3 9)
   (10 8 9)
   (2 8 10)))

(define inner-radius 1)    
(define outer-radius 1)    
(define outer-offset 2)    

(define (middle-point tri)
  (vscale (v+ (v+ (car tri) (cadr tri)) (caddr tri)) 1/3))

(define (move-to-unit-sphere p)
  (vscale p (/ (vlength p))))

(define (handle-tri tri)
 (let* ((middle (middle-point tri))
	(c (vscale outer-offset middle)))
	;(c (vscale outer-offset (move-to-unit-sphere middle))))
  (make-sphere c outer-radius chrome)))

(define (add-face face)
  (let ((tri 
    (list
      (vector-ref verts (- (car face) 1))
      (vector-ref verts (- (cadr face) 1))
      (vector-ref verts (- (caddr face) 1)))))
    (handle-tri tri)))

(define outers     
(let loop ((faces faces)
	   (result '()))
  (if (null? faces)
    result
      (loop (cdr faces) (cons (add-face (car faces)) result))))
)

(add-to-scene
 (make-difference
  (make-sphere #(0 0 0) inner-radius chrome)
  (apply make-union outers)
  (mat 0.7)))
