
(define (make-point-cloud num inside?)
  (let loop ((result '()))
    (if (= num (length result)) result)
    (let* ((x (random2 -3 3))
	   (y (random2 -3 3))
	   (z (random2 -3 3))
	   (v (vector x y z)))
     (if (inside? v)
       (loop (append result (list v)))
       (loop result)))))
     
(define (sphere-inside? v)
  (< 2 (vlength v)))

(define (box-inside? v)
  (and
    (< 2 (abs (.x v)))
    (< 2 (abs (.y v)))
    (< 2 (abs (.z v)))))

