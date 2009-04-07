
(load "lib/raygay.scm")
(load "lib/mesh.scm")

(set-background #(1 1 1 0))

(set-image-size '(256 256))
(set-renderer "raytracer")

(set-camera 
  (make-pinhole-camera 
    `(pos #(2.5 0.2 6)
      lookat #(0 0 0)
      up ,y-axis
      fov 45
      aa 1)))

(define grey
  (make-material
    '( diffuse #(0.8 0.8 0.8)
       kd 1.0
       ks 0.0)))
       
(define trans
  (make-material
    '( diffuse #(1.0 1.0 1.0)
       kd 1.0
       ks 0.0)))       

(define red
  (make-material
    '( diffuse #(1.0 0.0 0.0)
       kd 1.0
       ks 0.0)))       

;(add-to-scene (make-pointlight #(500 1300 1300)))
;;(add-to-scene (make-skylight 10 250 #(1 1 1)))
(add-to-scene (make-arealight #(100 200 200) #(-0.5 -1 -1) 500 50 0.1))

(add-to-scene
  (make-solid-box 
    #(-5 -2 -5) #(6 -1.2 6) trans))

(define pyramid-points
 (list #(-1 -1 -1) #(1 -1 -1) #(-1 -1 1) #(1 -1 1) #(0 1 0)))

(define house-points (list 
  #(-1 -1 -1) #(1 -1 -1) #(-1 -1 1) #(1 -1 1)
  #(-1 0 -1) #(1 0 -1) #(-1 0 1) #(1 0 1)
  #(-1 1 0) #(1 1 0)))		      

(define random-unit-points
 (let loop ((result '()))
  (if (= (length result) 50)
   result
   (loop 
    (cons
     ;(vnormalize (vector (random -1 1) (random -1 1) (random -1 1)))
     (vector (random -1 1) (random -1 1) (random -1 1))
     result)))))


(make-rounded-convex-hull house-points 0.05 red)    

