(load "scheme/test.scm")
(load "../../scenes/lib/vector-math.scm")
(load "../../scenes/lib/object-factory.scm")

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
 (let* ((y<10 (make-halfspace #(0 1 0) #(0 10 0)))
        (y>5  (make-halfspace #(0 -1 0) #(0 5 0)))
        (y<5  (make-halfspace #(0 1 0) #(0 5 0)))
        (ybetween5and10 (make-intersection y<10 y>5))
        (ybetween5and10-II (make-difference y<10 y<5))
        (xbetween5and10 
	 (rotate (make-instance ybetween5and10) #(0 0 1) 90)))
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
   (and
    (not (inside? ybetween5and10 #(0 11 0)))
    (inside? ybetween5and10 #(0 7 0))
    (not (inside? ybetween5and10 #(0 4 0)))))
 (test "Inside slab II with CSG intersection of two halfplanes" 
   (and
    (not (inside? ybetween5and10-II #(0 11 0)))
    (inside? ybetween5and10-II #(0 7 0))
    (not (inside? ybetween5and10-II #(0 4 0)))))
 (test "Intersect slab from over it"
  (near-equal?
   (intersect ybetween5and10 #(5 100 5) #(0 -1 0))
   (list #(5 10 5) #(0 1 0))))
; (display (intersect ybetween5and10-II #(5 100 5) #(0 -1 0)))
 (test "Intersect slab II from over it"
  (near-equal?
   (intersect ybetween5and10-II #(5 100 5) #(0 -1 0))
   (list #(5 10 5) #(0 1 0))))
 (test "Intersect slab from under it"
  (near-equal?
   (intersect ybetween5and10 #(1 -100 1) #(0 1 0))
   (list #(1 5 1) #(0 -1 0))))
 (test "All-intersections 1"
   (near-equal? 
    (all-intersections ybetween5and10 #(0 100 0) #(0 -1 0))
    '((#(0 10 0) #(0 1 0)) (#(0 5 0) #(0 -1 0)))))
 (test "All-intersections 2"
   (near-equal? 
    (all-intersections ybetween5and10-II #(0 100 0) #(0 -1 0))
    '((#(0 10 0) #(0 1 0)) (#(0 5 0) #(0 -1 0)))))
 (test "Inside rotated instance" 
   (and
    (not (inside? xbetween5and10 #(11 0 0)))
    (inside? xbetween5and10 #(7 0 0))
    (not (inside? xbetween5and10 #(4 0 0)))))
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
  (test "All intersections"
   (near-equal? 
    (all-intersections s1 #(0 100 0) #(0 -1 0))
    '((#(0 10 0) #(0 1 0)) (#(0 -10 0) #(0 -1 0)))))
  (test "1 = 1" (= 1 1))))

;; ---------------------------------------------
;; Test CSG Union
;; ---------------------------------------------
(define (test-csg-union)
 (let* ((s1 (make-sphere #(0 0 10) 15))
        (s2 (make-sphere #(0 0 -10) 15))
        (s3 (make-sphere #(0 0 0) 15))
        (s4 (make-sphere #(0 0 -50) 10))
        (csg (make-union s1 s2))
        (csg2 (make-union csg s3))
        (csg3 (make-union csg2 s4)))
  (test "Intersection 1" 
   (near-equal? 
    (map car (all-intersections csg #(0 0 100) #(0 0 -1)))
    '(#(0 0 25) #(0 0 -25))))
  (test "Intersection 2"
   (near-equal? 
    (map car (all-intersections csg2 #(0 0 100) #(0 0 -1)))
    '(#(0 0 25) #(0 0 -25))))
  (test "Intersection 3"
   (near-equal? 
    (map car (all-intersections csg3 #(0 0 100) #(0 0 -1)))
    '(#(0 0 25) #(0 0 -25) #(0 0 -40) #(0 0 -60))))
  (test "Intersection from inside"
   (near-equal? 
    (map car (all-intersections csg3 #(0 0 0) #(0 0 -1)))
    '(#(0 0 -25) #(0 0 -40) #(0 0 -60))))
  (test "." #t)))

;; ---------------------------------------------
;; Test CSG Intersection
;; ---------------------------------------------
(define (test-csg-intersection)
 (let* ((s1 (make-sphere #(0 0 10) 15))
        (s2 (make-sphere #(0 0 -10) 15))
        (s3 (make-sphere #(0 0 0) 15))
        (s4 (make-sphere #(0 0 -50) 10))
        (csg (make-intersection s1 s3))
        (csg2 (make-intersection csg s3))
        (csg3 (make-intersection csg2 s4)))
  (test "All intersections" 
   (near-equal? 
    (all-intersections csg #(0 0 100) #(0 0 -1))
    '((#(0 0 15) #(0 0 1)) (#(0 0 -5) #(0 0 -1)))))
  (test "Intersections"
   (and
    (near-equal?
     (intersect csg #(0 0 1000) #(0 0 -1)) '(#(0 0 15) #(0 0 1)))
    (near-equal?
     (intersect csg #(0 0 -1000) #(0 0 1)) '(#(0 0 -5) #(0 0 -1)))))
  (test "Inside"
   (and (not (inside? csg #(0 0 16)))
        (inside? csg #(0 0 14))
        (inside? csg #(0 0 -4))
        (not (inside? csg #(0 0 -6)))))
  (test "Ray that misses"
   (and
    (null? (all-intersections csg #(0 1000 -10) #(0 -1 0)))
    (null? (all-intersections csg #(0 1000 20) #(0 -1 0)))))
  (let* ((s1 (make-sphere #(0 0 10) 5))
         (s2 (make-sphere #(0 0 -10) 5))  
	 (csg (make-intersection s1 s2)))
   (test "Intersecting the void"
    (null? (all-intersections csg #(0 0 1000) #(0 0 -1))))
   (test "Inside void"
    (not (or (inside? csg #(0 0 0))
	     (inside? csg #(0 0 10))
	     (inside? csg #(0 0 -10))
	     (inside? csg #(0 0 26))
	     (inside? csg #(0 0 26))))))
  (test "." #t)))

;; ---------------------------------------------
;; Test CSG Difference
;; ---------------------------------------------
(define (test-csg-difference)
 (let* ((s1 (make-sphere #(0 0 10) 15))
        (s2 (make-sphere #(0 0 0) 15))
        (csg (make-difference s1 s2)))
  (test "All intersections" 
   (near-equal? 
    (all-intersections csg #(0 0 100) #(0 0 -1))
    '((#(0 0 25) #(0 0 1)) (#(0 0 15) #(0 0 -1)))))
  (test "Intersections"
   (and
    (near-equal?
     (intersect csg #(0 0 1000) #(0 0 -1)) '(#(0 0 25) #(0 0 1)))
    (near-equal?
     (intersect csg #(0 0 -1000) #(0 0 1)) '(#(0 0 15) #(0 0 -1)))
    (not (intersect csg #(0 0 25.1) #(0 0 1)))
    (not (intersect csg #(0 0 25) #(0 0 1)))
    (not (intersect csg #(0 0 14) #(0 0 -1)))
    (not (intersect csg #(0 0 14.99) #(0 0 -1)))
    (not (intersect csg #(0 0 15) #(0 0 -1)))))
  (test "Inside"
   (and (not (inside? csg #(0 0 26)))
        (inside? csg #(0 0 24))
        (inside? csg #(0 0 16))
        (not (inside? csg #(0 0 14)))
        (not (inside? csg #(0 0 0)))))
  (test "Hollow sphere"
   (let* ((s1 (make-sphere #(0 0 0) 30))
	  (s2 (make-sphere #(0 0 0) 29))
	  (csg (make-difference s1 s2)))
    (near-equal?
     (all-intersections csg #(0 0 1000) #(0 0 -1))
     '((#(0 0 30) #(0 0 1))
       (#(0 0 29) #(0 0 -1))
       (#(0 0 -29) #(0 0 1))
       (#(0 0 -30) #(0 0 -1))))))
  (test "Hollow ellipsoid"
   (let* ((s1 (make-ellipsoid #(0 0 0) #(10 20 30)))
	  (s2 (make-ellipsoid #(0 0 0) #(9 19 29)))
	  (csg (make-difference s1 s2)))
    (near-equal?
     (all-intersections csg #(0 0 1000) #(0 0 -1))
     '((#(0 0 30) #(0 0 1))
       (#(0 0 29) #(0 0 -1))
       (#(0 0 -29) #(0 0 1))
       (#(0 0 -30) #(0 0 -1))))))
  (test "." #t)))

;; ---------------------------------------------
;; Run the suite 
;; ---------------------------------------------

(run-test "Halfspace" test-halfspace)
(run-test "Sphere" test-sphere)
(run-test "CSG Union" test-csg-union)
(run-test "CSG Intersection" test-csg-intersection)
(run-test "CSG Difference" test-csg-difference)

