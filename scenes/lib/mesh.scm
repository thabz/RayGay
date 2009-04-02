;; This library contains tools for working with
;; meshes.
;;
;; A mesh is two lists: vertices and faces.
;; A vertex is a points in space as 3d vectors. 
;; A face is a list of indices into the vertices.

(define (optimize-mesh m)
  "Optimizes a mesh by removing unused vertices"
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

(define (collapse-nearby-vertices mesh ε)
  "Optimizes a mesh collapsing vertices that are closer than ε"
  ) 

(define (facing? v1 v2 v3 p)
 "Is the triangle with vertices t1, t2, t3 (clockwise) facing p?"
 (let ((n (vcrossproduct (v- v3 v1) (v- v3 v2))))
  (negative? (vdot n (v- p v1)))))

; 
; http://www.eecs.tufts.edu/~mhorn01/comp163/algorithm.html
; Using the 3D Incremental Convex Hull algorithm.
(define (convex-hull points)
 "Returns a mesh that is the convex hull of a group of points."
 
 (define (find-edges face)
  "Returns a list of pair representing edges. "
  "Ie. turns (a b c) into ((a b) (b c) (c a))"
  (let loop ((f face)
   	     (r '()))
   (if (null? (cdr f))
    (cons (list (car f) (car face)) r)
    (loop (cdr f) (list (car f) (cadr f))))))

 (define (find-border-edges faces)
  "From a group of connected faces return those edges that "
  "only connects to one face."
  (let face-loop ((faces faces)
		  (hash (make-hashtable equal?)))
   (if (null? faces)
    (hashtable-keys hash)
    (let edge-loop ((edges (find-edges (car faces))))
     (if (null? edges)
      (face-loop (cdr faces) hash)
      (begin
       (if (hashtable-ref hash (reverse (car edges)))
	(hashtable-delete! hash (reverse (car edges)))
 	(hashtable-put hash (car edges)))
       (edge-loop (cdr edges))))))))

 (define (find-facing-faces hull p)
  "Returns the faces of a hull that \"p can see.\""
  (filter 
   (lambda (face)
    ; Note this will work if face has more than 3 vertices. If the 
    ; triangle formed by the first 3 vertices is facing p then the 
    ; whole polygon is.
    (facing? (list-ref (car hull) (car face))
             (list-ref (car hull) (cadr face))
	     (list-ref (car hull) (caddr face))
	     p))
   (cadr hull)))

 (define (add-point hull p)
  "Incrementally grow the hull by adding a point p"
  (let ((facing-faces (find-facing-faces hull p)))
   (if (null? facing-faces)
    hull ; p is inside hull so return original hull.
    (let ((border-edges (find-border-edges facing-faces))
          ; Remove facing-faces from hull
          (faces (fold-left remove (cadr hull) facing-faces)))
     ; Add new faces from p to all border-edges

 (define initial-hull
  (list (car points) (cadr points) (caddr points) (cadddr points))
  (list '(0 3 1) '(0 1 2) '(1 3 2) '(3 0 2)))

 (if (< (length points) 3)
  '()
  (fold-right add-point initial-hull points)))



