
;;; A collection of useful simple objects

;; Creates a list of numbers
;;
;; Usage (sequence 5) -> (0 1 2 3 4)
;;
(define (sequence num)
  (let iter ((i 0))
    (if (= i num) '()
      (cons i (iter (+ i 1))))))

;; A pill is a cylinder with a sphere at each end. The spheres
;; have the same radius as the cylinder.
;;
;; Usage: (make-pill '(0 0 0) '(0 100 0) 20 red)
;;
(define (make-pill from to radius material)
  (list (make-sphere from radius material)
	(make-cylinder from to radius material)
	(make-sphere to radius material)))

;; Place objects along a path
;;
;; Usage:
;; (make-necklace spiral 200 (lambda () (make-sphere '(0 0 0) 20 chrome))) 
;;
(define (make-necklace path num maker)
  (map
    (lambda (i)
      (translate
	(maker)
	(point-on-path path (/ i num))))
    (sequence num)))

(define (make-rounded-plate halfwidth radius material)
  (let* ((w (- halfwidth radius))
	 (x+y+ (list w w 0))
	 (x+y- (list w (- w) 0))
	 (x-y- (list (- w) (- w) 0))
	 (x-y+ (list (- w) w 0)))
    (list (make-sphere x+y+ radius material)
	  (make-sphere x+y- radius material)
	  (make-sphere x-y- radius material)
	  (make-sphere x-y+ radius material)
	  (make-cylinder x+y+ x+y- radius material)
	  (make-cylinder x+y- x-y- radius material)
	  (make-cylinder x-y- x-y+ radius material)
	  (make-cylinder x-y+ x+y+ radius material)
	  ;	     (make-box 
	  ;	      (list (- w) (- w) (- radius))
	  ;	      (list w w radius) material)
	  )))

(define (make-rounded-box lowercorner uppercorner radius material)
  (let* (
	 (xo+ (list-ref uppercorner 0))
	 (yo+ (list-ref uppercorner 1))
	 (zo+ (list-ref uppercorner 2))
	 (xo- (list-ref lowercorner 0))
	 (yo- (list-ref lowercorner 1))
	 (zo- (list-ref lowercorner 2))
	 (x+ (- (list-ref uppercorner 0) radius))
	 (y+ (- (list-ref uppercorner 1) radius))
	 (z+ (- (list-ref uppercorner 2) radius))
	 (x- (+ (list-ref lowercorner 0) radius))
	 (y- (+ (list-ref lowercorner 1) radius))
	 (z- (+ (list-ref lowercorner 2) radius))
	 (x+y+z+ (list x+ y+ z+))
	 (x+y+z- (list x+ y+ z-))
	 (x+y-z+ (list x+ y- z+))
	 (x+y-z- (list x+ y- z-))
	 (x-y+z+ (list x- y+ z+))
	 (x-y+z- (list x- y+ z-))
	 (x-y-z+ (list x- y- z+))
	 (x-y-z- (list x- y- z-)))
    (list (make-sphere x+y+z+ radius material)
	  (make-sphere x+y+z- radius material)
	  (make-sphere x+y-z+ radius material)
	  (make-sphere x+y-z- radius material)
	  (make-sphere x-y+z+ radius material)
	  (make-sphere x-y+z- radius material)
	  (make-sphere x-y-z+ radius material)
	  (make-sphere x-y-z- radius material)
	  ; Front
	  (make-cylinder x+y+z+ x+y-z+ radius material)
	  (make-cylinder x+y-z+ x-y-z+ radius material)
	  (make-cylinder x-y-z+ x-y+z+ radius material)
	  (make-cylinder x-y+z+ x+y+z+ radius material)
	  ; Back
	  (make-cylinder x+y+z- x+y-z- radius material)
	  (make-cylinder x+y-z- x-y-z- radius material)
	  (make-cylinder x-y-z- x-y+z- radius material)
	  (make-cylinder x-y+z- x+y+z- radius material)
	  ; Back to front
	  (make-cylinder x+y+z- x+y+z+ radius material)
	  (make-cylinder x+y-z- x+y-z+ radius material)
	  (make-cylinder x-y+z- x-y+z+ radius material)
	  (make-cylinder x-y-z- x-y-z+ radius material)
          ; Fill in boxes
          (make-solid-box (list xo- y- z-) (list xo+ y+ z+) material)
          (make-solid-box (list x- yo- z-) (list x+ yo+ z+) material)
	  (make-solid-box (list x- y- zo-) (list x+ y+ zo+) material))))

