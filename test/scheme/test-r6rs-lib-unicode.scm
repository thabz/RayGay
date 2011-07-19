
(define (test-unicode)
 (test "string-upcase" (equal? (string-upcase "Scheme") "SCHEME"))
 (test "string-downcase" (equal? (string-downcase "Scheme") "scheme"))
)

(run-test "R6RS lib Unicode" test-unicode)
