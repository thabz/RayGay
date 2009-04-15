(load "../scenes/lib/vector-math.scm")
(load "../scenes/lib/mesh.scm")
(load "../scenes/lib/platonic-solids.scm")

(define tetrahedron-mesh (convex-hull tetrahedron-vertices))
(define hexahedron-mesh (convex-hull hexahedron-vertices))
(define octahedron-mesh (convex-hull octahedron-vertices))
(define dodecahedron-mesh (convex-hull dodecahedron-vertices))
(define icosahedron-mesh (convex-hull icosahedron-vertices))

(define tetrahedron-vertices-num (length (car tetrahedron-mesh)))
(define tetrahedron-faces-num (length (cadr tetrahedron-mesh)))
(define tetrahedron-edges-num (length (mesh-extract-edges tetrahedron-mesh)))

(define hexahedron-vertices-num (length (car hexahedron-mesh)))
(define hexahedron-faces-num (length (cadr hexahedron-mesh)))
(define hexahedron-edges-num (length (mesh-extract-edges hexahedron-mesh)))

(define octahedron-vertices-num (length (car octahedron-mesh)))
(define octahedron-faces-num (length (cadr octahedron-mesh)))
(define octahedron-edges-num (length (mesh-extract-edges octahedron-mesh)))

(define dodecahedron-vertices-num (length (car dodecahedron-mesh)))
(define dodecahedron-faces-num (length (cadr dodecahedron-mesh)))
(define dodecahedron-edges-num (length (mesh-extract-edges dodecahedron-mesh)))

(define icosahedron-vertices-num (length (car icosahedron-mesh)))
(define icosahedron-faces-num (length (cadr icosahedron-mesh)))
(define icosahedron-edges-num (length (mesh-extract-edges icosahedron-mesh)))

; Make a pointcloud with (+-1,+-1,+-1) and a lot of points
; inside (+-0.9,+-0.9,+-0.9). Check that the resulting hull
; is the unit-box.




; Test make-face->neighbours-hash
;(define h (make-face->neighbours-hash icosahedron-mesh))    

