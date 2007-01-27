
(load "lib/raygay.scm")
(load "lib/genetics.scm")

(set-renderer "raytracer")

(define (make-point-cloud num inside?)
  (let loop ((result '()))
    (if (= num (length result)) 
      result
      (let* ((x (random2 -3 3))
      	     (y (random2 -3 3))
	           (z (random2 -3 3))
 	           (v (vector x y z)))
       (if (inside? v)
         (loop (append result (list v)))
         (loop result))))))
     
(define (sphere-inside? v)
  (> 2 (vlength v)))

(define (box-inside? v)
  (and
    (> 2 (abs (.x v)))
    (> 2 (abs (.y v)))
    (> 2 (abs (.z v)))))

(define (fittness cloud-from cloud-to transform)
 (let loop ((sum 0)
   	        (i 0)
   	        (l (length transform)))
  (if (= i l) 
   (/ sum i)
   (loop (+ sum (vdistance
		         (list-ref cloud-from i)
		         (list-ref cloud-to (list-ref transform i))))
         (+ i 1)
         l))))

; Test with num = 30, popsize = 30, exit-condition = 50
; 45.49s user 0.33s system 97% cpu 47.072 total
; 8.10s user 0.15s system 99% cpu 8.250 total
; 3.27s user 0.11s system 101% cpu 3.340 total
;(25 8 26 21 12 7 16 5 23 9 18 6 27 19 14 20 17 0 29 15 13 24 1 28 2 10 4 3 22 11)
;(25 8 26 21 12 7 16 5 23 9 18 6 27 19 14 20 17 0 29 15 13 24 1 28 2 10 4 3 22 11)

(define num 500)
(define cloud-to (make-point-cloud num sphere-inside?))
(define cloud-from (make-point-cloud num box-inside?))

(define transform
(if #t
   (genetics num 30 
      (lambda (chromosome)
         (fittness cloud-from cloud-to chromosome))
      50)
(random-list num)))
 
 (display transform)

(set-image-size '(800 600))
(set-background #(1.0 1.0 1.0))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(0 0 8)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 0)))

(define blue
  (make-material
    '( diffuse #(0.15 0.31 0.38)
       kd 1.0
       ks 0.0)))

(define red
  (make-material
    '( diffuse #(0.58 0.15 0.15)
       kd 1.0
       ks 0.0)))


(add-to-scene (make-pointlight #(100 1300 1300)))

(dotimes i num
  (add-to-scene
    (make-cone
     (list-ref cloud-from i)
     (list-ref cloud-to (list-ref transform i))
     0.05
     0.0
     blue)
    (make-sphere
      (list-ref cloud-to i)
      0.03 red)))

