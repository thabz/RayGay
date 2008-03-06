
;; Makes a backdrop that has a backwall at the xy-plan and a floor at
;; the xz-plane. The floor and the wall meets in a curve with a radius.
;; Note that the backdrop is centered around x=0.
(define (make-continuous-studio-backdrop-csg size-box radius material)
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
     
;; Makes a backdrop that has a backwall at the xy-plan and a floor at
;; the xz-plane. The floor and the wall meets in one long curve with 
;; something kinda like a radius radius.
;; Note that the backdrop is centered around x=0.
(define (make-continuous-studio-backdrop-bezierpatch size-box radius material)
  (let* ((hw (/ (vector-ref size-box 0) 2))
         (hw/3 (/ hw 3))
         (x0 (- hw))
         (x1 (- hw/3))
         (x2 (+ hw/3))
         (x3 (+ hw))
         (y0 (vector-ref size-box 1))
         (y1 radius)
         (y2 0)
         (y3 0)
         (z0 0)
         (z1 0)
         (z2 radius)
         (z3 (vector-ref size-box 2)))
   (make-bezierpatch 
     (list 
       (vector x0 y3 z3) (vector x1 y3 z3) (vector x2 y3 z3) (vector x3 y3 z3)
       (vector x0 y2 z2) (vector x1 y2 z2) (vector x2 y2 z2) (vector x3 y2 z2)
       (vector x0 y1 z1) (vector x1 y1 z1) (vector x2 y1 z1) (vector x3 y1 z1)
       (vector x0 y0 z0) (vector x1 y0 z0) (vector x2 y0 z0) (vector x3 y0 z0)
     ) 20 20 material)))

;; The bezierpatch one is much faster to render than the CSG one, so that's the one
;; we use. Won't delete the code for the CSG model, as it might come in handy some day.
(define make-continuous-studio-backdrop make-continuous-studio-backdrop-bezierpatch)

