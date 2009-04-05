
(load "lib/raygay.scm")
(load "lib/mesh.scm")

(set-background #(0.95 0.95 0.95))

(set-image-size image-size-24-inch-intel-imac)
(set-image-size '(1024 768))
(set-renderer "raytracer")

(set-camera 
  (make-pinhole-camera 
    `(pos #(1.5 0.2 4)
      lookat #(0 0 0)
      up ,y-axis
      fov 45
      aa 1)))

(define grey
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 1.0
       ks 0.0)))
       
(define white
  (make-material
    '( diffuse #(1.0 1.0 1.0)
       kd 1.0
       ks 0.0)))       

(add-to-scene (make-pointlight #(500 1300 1300)))
;;(add-to-scene (make-skylight 10 250 #(1 1 1)))
;(add-to-scene (make-arealight #(1000 2000 2000) #(-0.5 -1 -1) 500 64 0.1))

(add-to-scene
  (make-solid-box 
    #(-5 -2 -5) #(6 -1.5 6) white))

(define pyramid-points
 (list #(-1 -1 -1) #(1 -1 -1) #(-1 -1 1) #(1 -1 1) #(0 1 0)))

(define house-points (list 
  #(-1 0 -1) #(1 0 -1) #(-1 0 1) #(1 0 1)
  #(-1 1 -1) #(1 1 -1) #(-1 1 1) #(1 1 1)
  #(-1 2 0) #(1 2 0)))

(define random-unit-points
 (let loop ((result '()))
  (if (= (length result) 50)
   result
   (loop 
    (cons
     ;(vnormalize (vector (random -1 1) (random -1 1) (random -1 1)))
     (vector (random -1 1) (random -1 1) (random -1 1))
     result)))))

(display random-unit-points)(newline)

(define hull
 (convex-hull random-unit-points))
;(display hull)(newline)

(display (mesh-extract-edges hull))(newline)
(display (car hull))(newline)

(for-each 
 (lambda (edge)
  (add-to-scene
   (make-cylinder 
    (list-ref (car hull) (car edge))
    (list-ref (car hull) (cadr edge))
    0.05 white)))
 (mesh-extract-edges hull))

(for-each
 (lambda (p)
  (add-to-scene (make-sphere p 0.05 white)))
 (car hull))
 

