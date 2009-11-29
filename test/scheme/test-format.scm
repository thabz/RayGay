(load "../../scenes/lib/format.scm")

(define (test-format)
  (test "Passthrough" (equal? (format #f "Foo") "Foo"))
  (test "Format decimal" (equal? (format #f "Foo~dbar" 255) "Foo255bar"))
  (test "Format hex" (equal? (format #f "Foo~xbar" 255) "Fooffbar"))
  (test "Format binary" (equal? (format #f "Foo~bbar" 3) "Foo11bar"))
  (test "Format list" (equal? (format #f "Foo~abar" '(1 2)) "Foo(1 2)bar"))
  (test "1 = 1" (= 1 1)))


(run-test "Format" test-format)
