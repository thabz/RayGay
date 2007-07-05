
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
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))

(add-to-scene (make-pointlight #(500 1300 1300)))

; Makes a sinus-circle in the xy-plane
(define (make-sinus-circle radius amplitude periods period-offset)
  (lambda (t) 
    (let ((a (* amplitude (sin (+ period-offset (* t periods TWO-PI))))))
       (vector
          (* (+ a radius) (cos (* t TWO-PI)))
          (* (+ a radius) (sin (* t TWO-PI)))
          0))))

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


(define num-around 60)
(define num-bands 15)
(define torus-R 500)
(define torus-r 100)

(dotimes i num-around
   (add-to-scene 
     (rotate 
        (translate 
           (stroke-path (make-sinus-circle torus-r 20 (/ num-bands 2) (+ HALF-PI (* i PI))) 10 chrome 300)
           (vector torus-R 0 0))
        y-axis
        (* (/ i num-around) 360))))
        
        
(dotimes i num-bands
  (add-to-scene
    (let* ((t (/ i num-bands))
           (a (* torus-r (cos (* t TWO-PI))))
           (b (* torus-r (sin (* t TWO-PI)))))
    (translate
       (rotate
          (stroke-path (make-sinus-circle (+ torus-R a) 0 num-around 0) 10 chrome 100)
           x-axis 90)
        (vector 0 b 0)))))
      
      
      
      