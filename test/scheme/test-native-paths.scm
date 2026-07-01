(define (test-native-circle-path)
 ; This exercises the native C++ PathFactory binding before paths.scm
 ; replaces make-circle/point-on-path/tangent-to-path with Scheme versions.
 (define c (make-circle #(0 0 0) 10 #(0 1 0)))
 (test "point" (near-equal? (point-on-path c 0.0) #(-10 0 0)))
 (test "tangent" (near-equal? (tangent-to-path c 0.0) #(0 0 1))))

(run-test "Native circle path" test-native-circle-path)
