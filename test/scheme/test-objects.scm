(load "scheme/test.scm")
(load "../scenes/lib/vector-math.scm")
(load "../scenes/lib/object-factory.scm")

(define (intersect? object origin direction)
 (not (eq? #f (intersect object origin direction))))

(define (intersection-point object origin direction)
 (car (intersect object origin direction)))

(define (intersection-normal object origin direction)
 (cadr (intersect object origin direction)))

(define (test-halfspace)
 (let ((y<10 (make-halfspace #(0 10 0) #(0 1 0))))
 (test 
  "Simple intersection" 
   (intersect? y<10 #(0 100 0) #(0 -1 0)))
 
 (test 
  "Simple intersection point" 
  (equal?
   (intersection-point y<10 #(0 100 0) #(0 -1 0))
   #(0 10 0)))
 (test "That 1 = 1" (= 1 1))))

(run-test "Halfspaces" test-halfspace)

