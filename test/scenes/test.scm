
(load "../../scenes/lib/vector-math.scm")
(define test-define-1 10)

; Testing vector math

(define test-vdot-1 (vdot #(1 2 3) #(4 5 6)))
(define test-vdot-2 (vdot #(3 -1 2) #(4 5 6)))

(define test-vcross-1 (vcross #(1 2 3) #(4 5 6)))

(define test-vlength-1 (vlength #(1 2 3)))
(define test-vlength-2 (vlength #(4 -2 2)))

(define test-vscale-1 (vscale #(1 2 3) 2))
(define test-vscale-2 (vscale #(4 -2 2) 3))

(define test-vplus-1 (v+ #(1 2 3) #(2 5 9)))
(define test-vplus-2 (v+ #(4 -2 2) #(4 5 6)))

(define test-vminus-1 (v- #(1 2 3) #(2 5 9)))
(define test-vminus-2 (v- #(4 -2 2) #(4 5 6)))

(define test-vnormalize-1 (vnormalize #(1 2 3)))
(define test-vnormalize-2 (vnormalize #(4 -2 2)))

(define test-translate-1 (translate #(1 1 1) #(1 2 3)))
(define test-translate-2 (translate #(2 2 2) #(-1 -2 -3)))

(define test-rotate-1 (rotate #(1 0 0) #(0 0 1) 90))
(define test-rotate-2 (rotate #(0 1 0) #(0 0 1) 90))

; Testing RGBA

(define color-white #(1 1 1))
(define color-blue-trans #(0 0 1 0.5))
