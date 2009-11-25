(load "scheme/test.scm")
(load "../scenes/lib/vector-math.scm")
(load "../scenes/lib/object-factory.scm")

;; ---------------------------------------------
;; Helpers for testing objects
;; ---------------------------------------------

(define (intersect? object origin direction)
 (not (eq? #f (intersect object origin direction))))

(define (intersection-point object origin direction)
 (car (intersect object origin direction)))

(define (intersection-normal object origin direction)
 (cadr (intersect object origin direction)))
 
;; ---------------------------------------------
;; Halfspace
;; ---------------------------------------------

(define (test-halfspace)
 (let ((y<10 (make-halfspace #(0 1 0) #(0 10 0)))
       (y>5  (make-halfspace #(0 -1 0) #(0 5 0))))
 (test "Intersection test from outside" 
   (intersect? y<10 #(0 100 0) #(0 -1 0)))
 (test "Intersection from the outside" 
  (near-equal?
   (intersect y<10 #(0 100 0) #(0 -1 0))
   (list #(0 10 0) #(0 1 0))))
 (test "Intersection from the inside" 
  (near-equal?
   (intersect y<10 #(0 -100 0) #(0 1 0))
   (list #(0 10 0) #(0 1 0))))
 (test "Points inside" 
  (and
   (inside? y<10 #(0 9 0))
   (not (inside? y<10 #(0 11 0)))
   (inside? y<10 #(10 9 10))
   (inside? y<10 #(0 -9 0))))
 (test "Inside slab with CSG intersection of two halfplanes" 
  (let ((slab (make-intersection y<10 y>5)))
   (and
    (not (inside? slab #(0 11 0)))
    (inside? slab #(0 7 0))
    (not (inside? slab #(0 4 0))))))
 (test "That 1 = 1" (= 1 1))))

;; ---------------------------------------------
;; Sphere
;; ---------------------------------------------

(define (test-sphere)
 (let ((s1 (make-sphere #(0 0 0) 10)))
  (test "Inside/outside tests"
   (and
    (inside? s1 #(0 5 0))
    (not (inside? s1 #(10 10 10)))
    (inside? s1 #(-5 -5 -5))))
  (test "1 = 1" (= 1 1))))

;; ---------------------------------------------
;; Run the suite 
;; ---------------------------------------------

(run-test "Halfspace" test-halfspace)
(run-test "Sphere" test-sphere)

