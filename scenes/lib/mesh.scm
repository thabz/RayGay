;; This library contains tools for working with
;; meshes.
;;
;; A mesh is two lists: vertices and faces.
;; A vertex is a points in space as 3d vectors. 
;; A face is a list of indices into the vertices.

(define (unique items)
 "Returns the unique (in term of equal?) items"
 (let ((h (make-hashtable equal-hash equal?)))
   (do ((items items (cdr items)))
    ((null? items)  
     (vector->list (hashtable-keys h)))
    (hashtable-set! h (car items) #t))))

(define (remove-all to-be-removed target-list)
  "Remove all items from to-be-removed from target-list"
  (let loop ((to to-be-removed)
             (ta target-list))
   (if (null? to)
    ta
    (loop (cdr to) (remove (car to) ta)))))
; Ovenstående kunne måske skrives (fold-right remove to ta)

(define (optimize-mesh m)
  "Optimizes a mesh by removing unused vertices"
  (define old->new (make-eqv-hashtable))
  (define new->old (make-eqv-hashtable))
  (define faces (cadr m))
  (define vertices (car m))
  
  ; Fill the hashtable
  (let insert-loop ((seq 0) (faces faces))
   (if (not (null? faces))
    (let face-loop ((seq seq)
		    (face (car faces)))
     (if (null? face)
       (insert-loop seq (cdr faces))
       (if (not (hashtable-contains? old->new (car face)))
	 (begin
          (hashtable-set! old->new (car face) seq)
          (hashtable-set! new->old seq (car face))
 	  (face-loop (+ 1 seq) (cdr face)))
         (face-loop seq (cdr face)))))))
  
  (define new-vertices 
    (map 
     (lambda (i) 
      (list-ref vertices (hashtable-ref new->old i #f)))
     (list-sort < 
     (vector->list (hashtable-keys new->old)))))  

  (define new-faces
    (map (lambda (face)
	  (map (lambda (i)
		(hashtable-ref old->new i #f)) 
	       face)) 
         faces))

  ; Return the new mesh
  (list new-vertices new-faces))

(define (collapse-nearby-vertices mesh ε)
  "Optimizes a mesh collapsing vertices that are closer than ε"
  ) 

(define (face-find-edges face)
 "Returns a list of pair representing edges. "
 "Ie. turns (a b c) into ((a b) (b c) (c a))"
 (let loop ((f face)
  	     (r '()))
  (if (null? (cdr f))
   (reverse (cons (list (car f) (car face)) r))
   (loop (cdr f) (cons (list (car f) (cadr f)) r)))))

(define (mesh-extract-edges mesh)
  "Returns a list of two-item lists" 
  (let faceloop ((faces (cadr mesh))
                 (result '()))
   (if (null? faces)
    (unique result)
    (let edgeloop ((edges (face-find-edges (car faces)))
                   (result result))
     (if (null? edges)
      (faceloop (cdr faces) result)
      (edgeloop (cdr edges)
                 (cons (list-sort < (car edges)) result)))))))

(define (facing? v1 v2 v3 p)
 "Is the triangle with vertices t1, t2, t3 (clockwise) facing p?"
 (let ((n (vcrossproduct (v- v3 v1) (v- v3 v2))))
  (negative? (vdot n (v- p v1)))))

; 
; http://www.eecs.tufts.edu/~mhorn01/comp163/algorithm.html
; Using the 3D Incremental Convex Hull algorithm.
(define (convex-hull points)
 "Returns a mesh that is the convex hull of a group of points."

 (define (find-border-edges faces)
  "From a group of connected faces return those edges that "
  "only connects to one face."
  (let face-loop ((faces faces)
		  (hash (make-hashtable equal-hash equal?)))
   (if (null? faces)
    (vector->list (hashtable-keys hash))
    (let edge-loop ((edges (face-find-edges (car faces))))
     (if (null? edges)
      (face-loop (cdr faces) hash)
      (begin
       (if (hashtable-contains? hash (reverse (car edges)))
	(hashtable-delete! hash (reverse (car edges)))
 	(hashtable-set! hash (car edges) #t))
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
  (let* ((facing-faces (find-facing-faces hull p))
 	 (new-index (length (car hull)))
	 (vertices (append (car hull) (list p)))
         ; Remove facing-faces from hull's faces
	 ;(faces (fold-left remove (cadr hull) facing-faces))
	 (faces (remove-all facing-faces (cadr hull)))
	 (border-edges (find-border-edges facing-faces)))
   (if (null? facing-faces)
    hull ; p is inside hull so return original hull.
    ; Loop adds new faces from p to all border-edges
    (let loop ((border-edges border-edges)
               (faces faces))
     (if (null? border-edges)
      (list vertices faces) ; Return new hull
      (loop (cdr border-edges)
            (cons (list (caar border-edges) (cadar border-edges) new-index)
	          faces))))))) 

 (define initial-hull
  (list
   (list (car points) (cadr points) (caddr points) (cadddr points))
   (list '(0 3 1) '(0 1 2) '(1 3 2) '(3 0 2))))

 (if (< (length points) 3)
  '()
  (optimize-mesh (fold-left add-point initial-hull (cddddr points)))))
  ;(fold-left add-point initial-hull (cddddr points))))

