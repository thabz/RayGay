;; This library contains tools for working with
;; meshes.
;;
;; A mesh is two lists: vertices and faces.
;; A vertex is a points in space as 3d vectors. 
;; A face is a list of indices into the vertices.

(define (optimize-mesh m)
  "Optimizes a mesh by removing unused vertices"
  ; This works by using a hashtable where the
  ; keys are the old indices and the values
  ; and the renumbered indices.
  (define old->new (make-hashtable eqv-hash eqv?))
  (define new->old (make-hashtable eqv-hash eqv?))
  (define faces (cadr m))
  (define vertices (car m))
  
  ; Fill the hashtable
  (let insert-loop ((seq 0) (faces faces))
   (if (not (null? faces))
    (let face-loop ((seq seq)
		    (face (car faces)))
     (if (null? face)
       (insert-loop seq (cdr faces))
       (if (not (hashtable-ref old->new (car face)))
	 (begin
          (hashtable-set! old->new (car face) seq)
          (hashtable-set! new->old seq (car face))
 	  (face-loop (+ 1 seq) (cdr face)))
         (face-loop seq (cdr face)))))))
  
  (define new-vertices 
    (map (lambda (i)
	   (list-ref vertices 
	    (hashtable-ref new->old i))) 
         (hashtable-values old->new)))  

  (define new-faces
    (map (lambda (face)
	  (map (lambda (old-index)
		(hashtable-ref old->new old-index)) 
	       face))) 
         old-faces)

  ; Return the new mesh
  (list new-vertices new-faces))

(define (hidden-face? t1 t2 t3 p)
)

; Returns a mesh that is the convex hull of a group of points.
; http://www.cse.unsw.edu.au/~lambert/java/3d/source/Incremental.java
(define (convex-hull points)
 (if (< (length points) 2)
  '()
  (let point-loop ((faces '())
       	           (points points))
   (null? points)
     faces
   (let es-loop ((es 
   (let face-loop ((facs faces)
		   (es '()))
    (if (null? facs) 
     es
     (if (not (hidden-face? (car facs) (car points)))
 	; Update bounary of hole
 	(face-loop (cdr facs)
	 ; Append to es here
	)
        (face-loop (cdr facs) es))))))
    (if (null? es)
     (point-loop faces (cdr points))
     (point-loop (append faces
		 ; Make new faces out of es
		 (map ... es))
		 ) (cdr points))))))


