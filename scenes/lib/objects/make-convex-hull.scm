(define (make-convex-hull points material)
 (define hull (convex-hull house-points))
 (make-mesh material (car hull) (cadr hull)))


(define (make-rounded-convex-hull points radius material)
 "Like make-convex-hull but with rounded edges and corners."

 (define hull (convex-hull points))

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
   (add-to-scene
    (make-cylinder 
     (list-ref (car hull) (car edge))
     (list-ref (car hull) (cadr edge))
     radius material)))
  (mesh-extract-edges hull))
 
 ; Add vertices
 (for-each
  (lambda (p)
   (add-to-scene (make-sphere p radius material)))
  (car hull))

 (for-each
  (lambda (face)
   (add-to-scene
   (make-thick-triangle
    (list-ref (car hull) (car face))
    (list-ref (car hull) (cadr face))
    (list-ref (car hull) (caddr face))
    radius material)))
  (cadr hull)))


