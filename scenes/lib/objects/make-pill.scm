;; A pill is a cylinder with a sphere at each end. The spheres
;; have the same radius as the cylinder.
;;
;; Usage: (make-pill #(0 0 0) #(0 100 0) 20 red)
;;
(define (make-pill from to radius material)
  (list (make-sphere from radius material)
	(make-uncapped-cylinder from to radius material)
	(make-sphere to radius material)))

