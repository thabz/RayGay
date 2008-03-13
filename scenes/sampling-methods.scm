;; Testing difference sampling distributions

;; Poisson dartthrowing tog 1:39.92 og fandt 1782 punkter
;; Poisson boundarysampling tog 0.27 og fandt 2127 punkter
;; Halton tog 0.21 sekunder for 2127 punkter
;; rand() tog 0.04 sekunder for 2127 punkter

;;(set! frame (+ 1 frame))
;;(display frame)
;;(newline)

(load "lib/raygay.scm")

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

(define radius 20)
(define w 2000)    
(define num 2127)
(define type 'poisson)

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

(define (to-0-1 p)
   (list (/ (car p) w) (/ (cadr p) w)))

(define (map-to-disc p)
  (let ((r (sqrt (car p)))
        ( φ (* 2 π (cadr p))))
    (list (* r (cos φ))
          (* r (sin φ)))))

(define (-1-1-to-0-w-w p)
  (let ((half-w (/ w 2)))
    (list (+ half-w (* half-w (car p)))
          (+ half-w (* half-w (cadr p))))))

(when #f 
(set! pset (map to-0-1 pset))
(set! pset (map map-to-disc pset))
(set! pset (map -1-1-to-0-w-w pset))
)

(dolist p pset
  (add-to-scene
    (make-sphere 
      (v- (vector (car p) (cadr p) 0)
	  (vector (* 0.5 w) (* 0.5 w) (* 0.5 w)))
   radius
   brown)))
   
