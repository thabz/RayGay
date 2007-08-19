
(define (set-pixel-scaled image coord-min coord-max coord color)
  (let* ((w (image-width image))
         (h (image-height image))
         (cw (- (vector-ref coord-max 0) (vector-ref coord-min 0)))
         (ch (- (vector-ref coord-max 1) (vector-ref coord-min 1)))
         (image-x (* w (/ (- (vector-ref coord 0) (vector-ref coord-min 0)) cw)))
         (image-y (* h (/ (- (vector-ref coord 1) (vector-ref coord-min 1)) ch))))
    (set-pixel img image-x (- h image-y) color)))     

; See http://en.wikipedia.org/wiki/Iterated_function_system
(define (fern v) 
  (let ((x (vector-ref v 0))
        (y (vector-ref v 1))
        (r (random 0 100)))
    (cond ((< r 1) (vector 0 (* 0.16 y)))
          ((< r 8) (vector (- (* 0.2 x) (* 0.26 y)) (+ (* 0.23 x) (* 0.22 y) 1.6)))
          ((< r 15) (vector (+ (* -0.15 x) (* 0.28 y)) (+ (* 0.26 x) (* 0.24 y) 0.44)))
          (else (vector (+ (* 0.85 x) (* 0.04 y)) (+ (* -0.04 x) (* 0.85 y) 1.6)))))) 


(define img (make-image 1000 1000 #(1 1 1)))

(do ((i 0 (+ i 1))
     (v #(0 0) (fern v)))
    ((= i 1000000))
    (set-pixel-scaled img #(-5 0) #(5 10) v #(0.02 0.7 0.01)))

(save-image img "fern.png")
