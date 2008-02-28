
;; Makes a backdrop that has a backwall at the xy-plan and a floor at
;; the xz-plane. The floor and the wall meets in a curve with a radius.
;; Note that the backdrop is centered around x=0.
(define (make-continuous-studio-backdrop size-box radius material)
  (let ((hw (/ (vector-ref size-box 0) 2))
        (oradius (+ 0.5 radius)))
   (make-union
     ; Curve
     (make-difference
       (make-cylinder (vector (- hw) radius radius) (vector hw radius radius) oradius) 
       (make-union
         (make-cylinder (vector (- 0 hw 1) radius radius) (vector (+ hw 1) radius radius) radius)
         ; Horizonal cut 
         (make-solid-box (vector (- 0 hw 1) radius 0) (vector (+ hw 1) (* 2 oradius) (* 2 oradius)))
         ; Vertical cut
         (make-solid-box (vector (- 0 hw 1) 0 radius) (vector (+ hw 1) (* 2 oradius) (* 2 oradius)))))
     ; Floor     
     (make-solid-box (vector (- hw) -1 0) (vector hw 0 (vector-ref size-box 2)))
     ; Backwall
     (make-solid-box (vector (- hw) 0 -1) (vector hw (vector-ref size-box 1) 0))
     material)))