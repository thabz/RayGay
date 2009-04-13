;; Creating the vertices for the platonic solids.
;; Can be used as convex hulls:
;; 
;; (add-to-scene (make-rounded-convex-hull dodocahedron-vertices 0.05 red))
;; 

(define (prepend-lists p lists)
 (if (null? lists)
  (list (list p))
  (map (lambda (v) (cons p v)) lists)))

(define (expand verts)
 (cond
  ((null? verts) '())
  ((zero? (car verts))
   (prepend-lists 0 (expand (cdr verts))))
  (else
   (append
    (prepend-lists (car verts) (expand (cdr verts)))
    (prepend-lists (- (car verts)) (expand (cdr verts)))))))

(define (snap-to-unit-sphere list)
 (map vnormalize list))

(define phi (/ (+ 1 (sqrt 5)) 2))
(define inv_phi (/ phi))

(define tetrahedron-vertices
 (snap-to-unit-sphere
 '(#(1 1 1) #(-1 -1 1) #(-1 1 -1) #(1 -1 -1))))

(define cube-vertices
 (snap-to-unit-sphere
  (map list->vector 
   (expand (list 1 1 1)))))

(define octahedron-vertices
 (snap-to-unit-sphere
  (map list->vector (append
    (expand (list 1 0 0))
    (expand (list 0 1 0))
    (expand (list 0 0 1))))))

(define icosahedron-vertices
 (snap-to-unit-sphere
  (map list->vector (append
    (expand (list 0 1 phi))
    (expand (list 1 phi 0))
    (expand (list phi 0 1))))))

(define dodecahedron-vertices
 (snap-to-unit-sphere
  (map list->vector (append
   (expand (list 1 1 1))
   (expand (list 0 inv_phi phi))
   (expand (list inv_phi phi 0))
   (expand (list phi 0 inv_phi))))))

