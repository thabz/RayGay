
;;; A collection of useful simple objects






;; Makes cylinders of all the linesegments and place
;; spheres at all the (unique) endpoints of the linesegments.
;; A hash should be used to determine uniqueness.
;; The linesegments is a list of pairs of points, where a point
;; is the using list of length 3.
(define (make-solid-wireframe linesegments radius material)
 (#f))
 

;; Create an egg with a joined half-sphere and an half-ellipsoid using CSG.
;; The ellipsoids long radius should be around (* 1.5 radius)
(define (make-egg radius material)
 (#f))
 

;; Vector operations    
(define (sum l) 
 (if (null? l) 
  0 (+ (car l) (sum (cdr l)))))

(define (.x vec) (car vec))
(define (.y vec) (list-ref vec 1))  
(define (.z vec) (list-ref vec 2))

;; Normalize a vector
(define (normalize v)
 (let ((l (sqrt (sum (map * v v)))))
  (if (zero? l)
  l ;; Throw error
  (map / v (list l l l)))))

(define (eqfloat? a b)
 (< (abs (- a b)) 0.0000001))

(define (eqvec? vec1 vec2)
 (and 
  (eqfloat? (.x vec1) (.x vec2))
  (eqfloat? (.y vec1) (.y vec2))
  (eqfloat? (.z vec1) (.z vec2))))

;; Comparator used for vecsort below    
(define (lessvec? vec1 vec2)
  (if 
    (eqfloat? (.x vec1) (.x vec2))
    (if 
      (eqfloat? (.y vec1) (.y vec2))
      (< (.z vec1) (.z vec2))
      (< (.y vec1) (.y vec2)))
    (< (.x vec1) (.x vec2))))

;; Sort a list of vecs, ie (vecsort '((1 2 3) (1 2 4) (1 3 0)))
(define (vecsort veclist)
 (sort-list veclist lessvec?))
 
