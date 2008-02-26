;; See http://en.wikipedia.org/wiki/Quicksort
;; Implementing the r6rs list-sort.
(define (list-sort proc list)
  (if (<= (length list) 1) 
    list
    (let loop ((less '())
                (greater '())
		(equal '())
                (list list)
                (pivot (car list)))
       (if (null? list)
         (append (list-sort proc less) equal (list-sort proc greater))
	 (cond
	  ((equal? (car list) pivot)
	   (loop less
	         greater
		 (cons (car list) equal)
		 (cdr list)
		 pivot))
          ((proc (car list) pivot)
           (loop (cons (car list) less)
                 greater
		 equal
                 (cdr list)
                 pivot))
          (else
           (loop less
                 (cons (car list) greater)
		 equal
                 (cdr list)
                 pivot)))))))

(define (vector-sort proc vector))

(define (vector-sort! proc vector))
