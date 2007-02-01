
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
  (> 1.5 (vlength v)))

(define (box-inside? v)
  (and
    (> 1.2 (abs (.x v)))
    (> 1.2 (abs (.y v)))
    (> 1.2 (abs (.z v)))))

(define (heart-inside? v)
  (let ((x (* 0.8 (.z v)))
        (y (* 0.8 (.x v)))
        (z (* 0.8 (.y v))))
     (negative? (- (expt (+ (* 2 x x) (* y y) (* z z) -1) 3)
 	                 (* x x z z z 0.1)
                   (* y y z z z)))))

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

(define num 1000)
(define sphere-cloud (make-point-cloud num sphere-inside?))
(define box-cloud (make-point-cloud num box-inside?))
(define heart-cloud (make-point-cloud num heart-inside?))

(define (transform cloud-from cloud-to max-iter)
   (genetics num 30 
      (lambda (chromosome)
         (fittness cloud-from cloud-to chromosome))
   max-iter))

;(display transform)

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


(define (keyframing t keyframes)
  (let* ((sum (apply + (map car keyframes)))
         (scaled-t (* sum t)))
     (let loop ((k keyframes)
                (t 0))  
       (if (<= scaled-t (+ t (caar k))) 
         (cadar k)
         (loop (cdr k) (+ t (caar k)))))))     

(keyframing 0.1 '((10 1) (10 2) (10 3)))


(add-to-scene (make-pointlight #(100 1300 1300)))

(define (render-transition cloud-from cloud-to transform)
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
      0.03 red))))

(define (render-cloud cloud)
  (dotimes i num
    (add-to-scene
      (make-sphere
        (list-ref cloud i)
        0.08 red))))
  
(render-cloud heart-cloud)  
;(render-cloud box-cloud)
;(render-cloud sphere-cloud)

