
;; In mathematics, the convex hull for a set of points X in a real vector space V is the minimal convex set containing X.
;; For planar objects, i.e., lying in the plane, the convex hull may be easily visualized by imagining an elastic band 
;; stretched open to encompass the given object; when released, it will assume the shape of the required convex hull.
;; Description taken from Wikipedia.

;; Find the point with the lowest y-coordinate.
;; If there's a tie, the one of the tie-breaking
;; candidates with the lowest x-coordiate is returned.
(define (point-with-lowest-y l)
  (let loop ((r (car l))
             (l (cdr l)))
    (if (empty? l) r)
    (if (= (vector-ref r 1) (vector-ref (car l) 1))
      (if (< (vector-ref r 0) (vector-ref (car l) 0))
        (loop r (cdr l))
        (loop (car l) (cdr l))))
    (if (< (vector-ref r 1) (vector-ref (car l) 1))
      (loop r (cdr l))
      (loop (car l) (cdr l)))))

;; http://en.wikipedia.org/wiki/Jarvis_march
(define (jarvis-march points))

;; http://en.wikipedia.org/wiki/Graham_scan
(define (graham-scan points)
  
  )



(define (find-convex-hull points-in-plane . method)
   (case method
     (('jarvis-march) (jarvis-march points-in-plane))
     (('graham-scan) (graham-scan points-in-plane))
     (else (graham-scan points-in-plane))))
