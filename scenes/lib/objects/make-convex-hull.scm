(define (make-convex-hull points material)
 (define hull (convex-hull house-points))
 (make-mesh material (car hull) (cadr hull)))


(define (make-rounded-convex-hull points radius material)
 "Like make-convex-hull but with rounded edges and corners."

 (define hull (convex-hull points))
 (define result '())

 (define (make-thick-triangle v1 v2 v3 thickness material)
  (let* ((n (vnormalize (vcrossproduct (v- v3 v1) (v- v3 v2))))
         (nscaled+ (vscale n thickness))
         (nscaled- (vscale n (- thickness)))
         (points (list (v+ v1 nscaled+) (v+ v1 nscaled-) (v+ v2 nscaled+)
 	   	      (v+ v2 nscaled-) (v+ v3 nscaled+) (v+ v3 nscaled-)))
 	(hull (convex-hull points)))
        (make-mesh material (car hull) (cadr hull))))

 ; Add edges
 (for-each 
  (lambda (edge)
   (set! result (cons
    (make-cylinder 
     (list-ref (car hull) (car edge))
     (list-ref (car hull) (cadr edge))
     radius material) result)))
  (mesh-extract-edges hull))
 
 ; Add vertices
 (for-each
  (lambda (p)
   (set! result (cons
    (make-sphere p radius material) result)))
  (car hull))

 ; Add faces
 (for-each
  (lambda (face)
   (set! result (cons
    (make-thick-triangle
     (list-ref (car hull) (car face))
     (list-ref (car hull) (cadr face))
     (list-ref (car hull) (caddr face))
     radius material) result)))
  (cadr hull))

    
 result)


