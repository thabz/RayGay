
; Poisson dartthrowing tog 1:39.92 og fandt 1782 punkter
; Poisson boundarysampling tog 0.27 og fandt 2127 punkter
; Halton tog 0.21 sekunder for 2127 punkter
; rand() tog 0.04 sekunder for 2127 punkter

(debug-set! stack 0)

;(set! frame (+ 1 frame))
;(display frame)
;(newline)

(load "lib/raygay.scm")

;; Testing difference sampling distributions

(set-image-size '(512 512))
(set-image-size '(1024 1024))
(set-background #(0.99 0.99 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(0 0 1700)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 3)))

(define brown
  (make-material
    '( diffuse #(0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define grey85
  (make-material
    '( diffuse #(0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define chrome
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 30)))


(add-to-scene (make-pointlight #(5000 10300 10300)))

(define img (make-texture "gfx/larry.jpg" 1.0 1.0 "none"))

(define radius 20)
(define w 2000)    
(define num 2127)
(define type 'jitter)

(define pset 
  (case type
    ((poisson) 
       (make-poisson-disc-set w w radius num))
    ((halton) 
       (make-halton-set w w num))
    ((jitter)
       (let* ((across (sqrt num))
              (step (/ w across)))
         (let loop ((a '())
                    (y 0)
                    (x 0))
            (if (= (length a) num)
               a    
               (loop (cons (list (+ (* x step) (random2 0 step))       
                                 (+ (* y step) (random2 0 step))) 
                           a)
                     (if (>= x across) (+ 1 y) y)
                     (if (>= x across) 0 (+ x 1)))))))
    ((random) 
       (let loop ((a '()))
         (if (= (length a) num)
          a
          (loop (cons (list (random2 0 w) (random2 0 w)) a)))))))

(display (length pset))
(newline)    

(do ((i 0 (+ i 1)))
  ((= (length pset) i) i)
 ; (display (list-ref pset i))
 ; (newline)
  (add-to-scene
    (make-sphere 
      (v- (vector (list-ref (list-ref pset i) 0) (list-ref (list-ref pset i) 1) 0)
	  (vector (* 0.5 w) (* 0.5 w) (* 0.5 w)))
   radius
   brown)))
   