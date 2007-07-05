
(load "lib/raygay.scm")

;(set-background (make-texture "gfx/paledawn.jpg" 1 1 "bilinear"))
(set-background #(0.6 0.7 0.8))
(set-background (make-texture "gfx/goldensunset.jpg" 1 1 "bilinear"))

(set-image-size '(1024 768))
(set-renderer "raytracer")

(set-camera 
  (make-pinhole-camera 
    `(pos #(0 600 1700)
      lookat #(0 0 0)
      up ,y-axis
      fov 45
      aa 3)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 1.0
       ks 0.0)))

(add-to-scene (make-pointlight #(500 1300 1300)))

; Makes a sinus-circle in the xy-plane
(define (make-sinus-circle radius amplitude periods period-offset)
  (lambda (t) 
    (let ((a (* amplitude (sin (+ period-offset (* t periods TWO-PI))))))
       (vector
          (* (+ a radius) (cos (* t TWO-PI)))
          (* (+ a radius) (sin (* t TWO-PI)))
          0))))

(define (make-villarceau-circle R r dir r-offset amplitude periods period-offset)
   (lambda (t)
      (let* ((tR (* t 360))
	     (tr (* dir (+ r-offset (* t TWO-PI))))
             (a (* amplitude (sin (+ period-offset (* t periods TWO-PI)))))
             (p (rotate 
 		  (translate
		    (vector 0 (* r (sin tr)) (* r (cos tr)))
		    (vector 0 0 R))
		  y-axis tR))
             (n (vnormalize (v- p (vscale (vnormalize (vector (.x p) 0 (.z p))) R)))))
       (v+ (vscale n a) p))))

; Stroke a path with cylinders with spheres as joints 
(define (stroke-path path radius mat num)
  (let ((result '()))
    (dotimes i num
      (let* ((t1 (/ i num))
        (t2 (/ (+ i 1) num))
        (p1 (path t1))
        (p2 (path t2)))
   (set! result (cons (make-sphere p1 radius mat) result))
   (set! result (cons (make-cylinder p1 p2 radius mat) result))))
 result))  


(define num-around 20)
(define torus-R 500)
(define torus-r 100)
(define a 10)
(define tube-r 5)    

(dotimes i num-around
 (add-to-scene
    (stroke-path
      (make-villarceau-circle torus-R torus-r 1
       (* i (/ TWO-PI num-around)) a num-around (+ PI (* i PI)))
      tube-r chrome 300))
 (add-to-scene
    (stroke-path
      (make-villarceau-circle torus-R torus-r -1
       (* i (/ TWO-PI num-around)) a num-around (+ HALF-PI (* i PI)))
      tube-r chrome 300)))

