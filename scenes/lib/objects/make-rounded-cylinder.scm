
(define (make-rounded-cylinder height R r material)
  ;; Makes a cylinder with round edges.
  ;; The cylinder has base at origin and ends at (0,height,0)
  ;; The radius of the cylinder is R and the radius of the
  ;; round edges are r.
  (let* ((from (list 0 0 0))
	 (to (list 0 height 0))
	 (dir (v- to from))
	 (unitdir (vnormalize dir))
	 (stepdir (vscale unitdir r))
	 (length (vlength dir)))
    (begin
      (display stepdir)
      (newline)
      (list (make-cylinder from to (- R r) material)
	    (make-cylinder (v+ from stepdir) (v- to stepdir) R material)
	    (translate
	      (make-torus (- R r) r material) 
	      (list 0 r 0))
	    (translate
	      (make-torus (- R r) r material) 
	      (list 0 (- height r) 0))
	    ))))




