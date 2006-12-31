
;; Makes a cylinder with round edges.
;; The cylinder has base at origin and ends at (0,height,0)
;; The radius of the cylinder is R and the radius of the
;; round edges are r.
(define (make-rounded-cylinder height R r material)
  (let* ((from (vector 0 0 0))
	       (to (vector 0 height 0))
      	 (dir (v- to from))
	       (unitdir (vnormalize dir))
	       (stepdir (vscale unitdir r))
	       (length (vlength dir)))
    (list (make-cylinder from to (- R r) material)
	        (make-cylinder (v+ from stepdir) (v- to stepdir) R material)
	        (translate
	          (make-torus (- R r) r material) 
	          (vector 0 r 0))
      	  (translate
	          (make-torus (- R r) r material) 
	          (vector 0 (- height r) 0)))))

;; Same as above but only rounded at the top
(define (make-top-rounded-cylinder height R r material)
  (let* ((from (vector 0 0 0))
	       (to (vector 0 height 0))
      	 (dir (v- to from))
	       (unitdir (vnormalize dir))
	       (stepdir (vscale unitdir r))
	       (length (vlength dir)))
    (list (make-cylinder from to (- R r) material)
	        (make-cylinder from (v- to stepdir) R material)
      	  (translate
	          (make-torus (- R r) r material) 
	          (vector 0 (- height r) 0)))))
