;; http://en.wikipedia.org/wiki/Icosahedron

(load "lib/raygay.scm")
(load "lib/objects/wireframing.scm")
(load "lib/colors.scm")

(set-background #(0.95 0.95 0.95))

(set-image-size '(1600 1200))
(set-renderer "raytracer")

(set-camera 
  (make-pinhole-camera 
    `(pos #(2 1 4)
      lookat #(0 0 0)
      up ,y-axis
      fov 45
      aa 3)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 1.0
       ks 0.0)))

;(add-to-scene (make-pointlight #(500 1300 1300)))
;(add-to-scene (make-skylight 10000 512 #(1 1 1)))  
(add-to-scene (make-arealight #(1000 2000 2000) #(-0.5 -1 -1) 500 64 0.1))

(define (mat t)
(make-material
  `( diffuse ,(hsv->rgb (vector t 0.5 1.0))
     kd 0.6
     specular #(1.0 1.0 1.0)
     ks 0.4
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

(define (middle-point tri)
  (vscale (v+ (v+ (car tri) (cadr tri)) (caddr tri)) 1/3))

(define (point-on-line p1 p2 t)
  (v+ p1 (vscale (v- p2 p1) t)))

(define (shrink-tri tri percentage)
  (let ((c (middle-point tri)))
    (list
      (point-on-line (car tri) c percentage)
      (point-on-line (cadr tri) c percentage)
      (point-on-line (caddr tri) c percentage))))

(define (move-to-unit-sphere p)
  (vscale p (/ (vlength p))))

(define (split-tri tri)
  (let* ((p1 (car tri))
         (p2 (cadr tri))
         (p3 (caddr tri))
         (m1 (move-to-unit-sphere (point-on-line p1 p2 0.5)))
         (m2 (move-to-unit-sphere (point-on-line p2 p3 0.5)))
         (m3 (move-to-unit-sphere (point-on-line p3 p1 0.5))))
    (list
      (list p1 m1 m3)
      (list m1 p2 m2)
      (list m2 p3 m3)
      (list m1 m2 m3))))

(define (split-tris tris)
  (let loop ((tris tris)
             (result '()))
    (if (null? tris) 
      result
      (loop (cdr tris) (append result (split-tri (car tris)))))))
      
(define (add-tubular-bell tri)
  (let ((s (shrink-tri tri 0.2))
        (r 0.01)
        (m (mat 0.4)))
    (add-to-scene
      (make-sphere (car s) r m)
      (make-cylinder (car s) (cadr s) r m)
      (make-sphere (cadr s) r m)
      (make-cylinder (cadr s) (caddr s) r m)
      (make-sphere (caddr s) r m)
      (make-cylinder (caddr s) (car s) r m))))

(define (add-stub tri)
  (let* ((s (shrink-tri tri 0.3))
         (p1 (car s))
         (p2 (cadr s))
         (p3 (caddr s))
         (a1 (point-on-line #(0 0 0) p1 0.8))
         (a2 (point-on-line #(0 0 0) p2 0.8))
         (a3 (point-on-line #(0 0 0) p3 0.8)))
  (add-to-scene
    (make-mesh 
      (mat 0.2) 
      ;      0  1  2  3  4  5
      (list p1 p2 p3 a1 a2 a3)
      (list #(0 1 2) #(5 4 3) 
            #(1 4 5) #(5 2 1)
            #(3 4 0) #(0 4 1)
            #(2 5 3) #(3 0 2)
        )))))

(define (handle-tris tris)
  (if (not (null? tris))
    (begin
      (add-stub (car tris))
      (handle-tris (cdr tris)))))

(define (handle-tri tri)
  (handle-tris
   (split-tris (split-tris (split-tri tri)))))

(define (add-face face)
  (let ((tri 
    (list
      (vector-ref verts (- (car face) 1))
      (vector-ref verts (- (cadr face) 1))
      (vector-ref verts (- (caddr face) 1)))))
    (handle-tri tri)))

(let loop ((faces faces))
  (if (not (null? faces))
    (begin
      (add-face (car faces))
      (loop (cdr faces)))))

(add-to-scene
  (make-sphere #(0 0 0) 0.9 (mat 0.2)))