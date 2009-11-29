
(load "../../scenes/lib/vector-math.scm")
(define test-define-1 10)

; Testing vector math

(define (test-vector-math)
 (test "dot 1" (near-equal? (vdot #(1 2 3) #(4 5 6)) (+ 4 10 18)))
 (test "dot 2" (near-equal? (vdot #(3 -1 2) #(4 5 6)) (+ 12 -5 12)))
; (test "vcross" (near-equal? (vcross #(1 2 3) #(4 5 6)) (sqrt 32)))
 (test "vlength 1" (near-equal? (vlength #(1 2 3)) (sqrt 14)))
 (test "vlength 2" (near-equal? (vlength #(4 -2 2)) (sqrt 24)))
 (test "vscale 1" (near-equal? (vscale #(1 2 3) 2) #(2 4 6)))
 (test "vscale 2" (near-equal? (vscale #(4 -2 2) 3) #(12 -6 6)))
 (test "vplus 1" (near-equal? (v+ #(1 2 3) #(2 5 9)) #(3 7 12)))
 (test "vplus 2" (near-equal? (v+ #(4 -2 2) #(4 5 6)) #(8 3 8)))
 (test "vminus 1" (near-equal? (v- #(1 2 3) #(2 5 9)) #(-1 -3 -6)))
 (test "vminus 2" (near-equal? (v- #(4 -2 2) #(4 5 6)) #(0 -7 -4)))
 (test "translate 1" (near-equal? (translate #(1 1 1) #(1 2 3)) #(2 3 4)))
 (test "translate 2" (near-equal? (translate #(2 2 2) #(-1 -2 -3)) #(1 0 -1)))
 (test "rotate 1" (near-equal? (rotate #(1 0 0) #(0 0 1) 90) #(0 -1 0))) 
 (test "rotate 1" (near-equal? (rotate #(0 1 0) #(0 0 1) 90) #(1 0 0))) 
 (test "1=1" (equal? 2 (+ 1 1))))

(run-test "Vector math" test-vector-math)
