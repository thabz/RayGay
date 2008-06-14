; Stroke a path with cylinders with spheres as joints 
; A path is a function from [0,1] to R^3
(define (stroke-path path radius mat num)
  (let ((result '()))
    (dotimes i num
      (let* ((t1 (/ i num))
        (t2 (/ (+ i 1) num))
        (p1 (path t1))
        (p2 (path t2)))
   (set! result (cons (make-sphere p1 radius mat) result))
   (set! result (cons (make-cylinder p1 p2 radius mat) result))))
 result))  

;; Make a wiremesh out of a parametric surface.
;; @param surface-func the parametric description of the surface 
;;        as a function (u,v) -> R^3.
;; @param u-wires number of wires in the u-dimension.
;; @param v-wires number of wires in the v-dimension.
;; @param u-num number of cylinders to build each u-wire with. 
;; @param v-num number of cylinders to build each v-wire with. 
;; @param radius radius of the wires.
;; @param mat material of the wires.
(define (make-parametric-surface-as-wireframe 
	 surface-func u-wires v-wires u-num v-num radius mat)
 (define result '())
 (do ((u 0 (+ u 1))) ((= u u-wires))
  (set! result (append result
		(stroke-path
		 (lambda (t) (surface-func (/ u u-wires) t))
		 radius mat v-num))))
 (do ((v 0 (+ v 1))) ((= v v-wires))
  (set! result (append result
		(stroke-path
		 (lambda (t) (surface-func t (/ v v-wires)))
		 radius mat u-num))))
 result)

