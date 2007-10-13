;; See http://en.wikipedia.org/wiki/Quicksort
;; Implementing the r6rs list-sort.
(define (list-sort proc list)
  (if (<= (length list) 1) 
    list
    (let loop ((less '())
                (greater '())
                (list list)
                (pivot (car list)))
       (if (null? list)
         (append (list-sort proc less) (list-sort proc greater))
         (if (proc (car list) pivot)
           (loop (cons (car list) less)
                 greater
                 (cdr list)
                 pivot)
           (loop less
                 (cons (car list) greater)
                 (cdr list)
                 pivot))))))
                 