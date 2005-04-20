
(load "make-rounded-box.scm")

(define (make-bounding-box obj radius material)
 (apply make-rounded-wire-box
  (append (bounding-box obj) (list radius material))))

