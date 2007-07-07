
(load "lib/raygay.scm")

;(set-background (make-texture "gfx/paledawn.jpg" 1 1 "bilinear"))
(set-background (make-texture "gfx/goldensunset.jpg" 1 1 "bilinear"))
(set-background #(0.95 0.95 0.95))

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

;(add-to-scene (make-pointlight #(500 1300 1300)))
;(add-to-scene (make-skylight 10000 512 #(1 1 1)))
(add-to-scene (make-arealight #(1000 2000 2000) #(-0.5 -1 -1) 500 64 0.1))

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

; See http://en.wikipedia.org/wiki/HSV_color_space
(define (hsv->rgb hsv)
  (let* ((H (vector-ref hsv 0))
         (S (vector-ref hsv 1))
         (V (vector-ref hsv 2))
         (Hi (modulo (floor (/ H 60)) 6))
         (f (- (/ H 60) Hi))
         (p (* V (- 1 S)))
         (q (* V (- 1 (* f S))))
         (t (* V (- 1 (* (- 1 f) S)))))
    (case Hi 
      ((0) (vector V t p))     
      ((1) (vector q V p))     
      ((2) (vector p V t))     
      ((3) (vector p q V))     
      ((4) (vector t p V))     
      ((5) (vector V p q)))))


(define num-around 16)
(define torus-R 500)
(define torus-r 100)
(define a 10)
(define tube-r 20.5)
(define sections 500)    

;(add-to-scene (make-solid-box #(-700 -1 -700) #(700 0 700) chrome))

(dotimes i num-around
 (add-to-scene
    (translate 
    (stroke-path
      (make-villarceau-circle torus-R torus-r 1
       (* i (/ TWO-PI num-around)) a num-around HALF-PI)
      tube-r
      (make-material
        `( diffuse ,(hsv->rgb (vector (* i (/ 360 num-around)) 0.5 1.0))
           kd 0.6
           specular #(1.0 1.0 1.0)
           ks 0.4
           specpow 30))
      sections)
      #(0 120 0))))

    
    
    

