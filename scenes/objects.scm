
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

