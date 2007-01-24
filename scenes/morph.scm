
(load "lib/raygay.scm")
(load "lib/genetics.scm")

(set-renderer "none")

(define (make-point-cloud num inside?)
  (let loop ((result '()))
    (if (= num (length result)) 
      result
      (let* ((x (random2 -3 3))
  	     (y (random2 -3 3))
	     (z (random2 -3 3))
	     (v (vector x y z)))
       (if (inside? v)
         (loop (append result (list v)))
         (loop result))))))
     
(define (sphere-inside? v)
  (> 2 (vlength v)))

(define (box-inside? v)
  (and
    (> 2 (abs (.x v)))
    (> 2 (abs (.y v)))
    (> 2 (abs (.z v)))))

(define (fittness cloud-from cloud-to transform)
 (let loop ((sum 0)
	    (i 0))
  (if (= i (length transform)) 
   (/ sum i)
   (loop (+ sum (vlength
		 (v- (list-ref cloud-from i)
		     (list-ref cloud-to (list-ref transform i)))))
         (+ i 1)))))

(define num 100)
(define cloud-to (make-point-cloud num sphere-inside?))
(define cloud-from (make-point-cloud num box-inside?))

(genetics num 20 
 (lambda (chromosome)
  (fittness cloud-from cloud-to chromosome))
 1000)

