(load "../../scenes/lib/vector-math.scm")
(load "../../scenes/lib/paths.scm")

(define (all-inside? obj points)
 (let loop ((result #t)
            (points points))
  (if (null? points)
   result
   (loop (and result (inside? obj (car points)))
         (cdr points)))))


(define (test-circle-path)
 ; Test at origin
 (define c (make-circle #(0 0 0) 10 #(0 0 1)))
 (test "closed circle" (path-closed? c))
 (test "inside box" 
   (all-inside?
   (make-solid-box #(-11 -11 -1) #(11 11 1))
   (points-on-path c 100)))
 ; Test tangent
 (test "tangent1" (near-equal? (tangent-to-path c 0/4) #(0 1 0)))
 (test "tangent2" (near-equal? (tangent-to-path c 1/4) #(-1 0 0)))
 (test "tangent3" (near-equal? (tangent-to-path c 2/4) #(0 -1 0)))
 (test "tangent4" (near-equal? (tangent-to-path c 3/4) #(1 0 0)))
 (test "point1" (near-equal? (point-on-path c 0.00) #(10 0 0)))
 (test "point2" (near-equal? (point-on-path c 0.25) #(0 10 0)))
 (test "point3" (near-equal? (point-on-path c 0.50) #(-10 0 0)))
 (test "point4" (near-equal? (point-on-path c 0.75) #(0 -10 0)))
 )
 
(run-test "Circle path" test-circle-path)
