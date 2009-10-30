
(load "lib/raygay.scm")

(set-background #(0.95 0.95 0.95))

(set-image-size '(1024 768))
(set-image-size image-size-24-inch-intel-imac)
(set-renderer "raytracer")

(set-camera 
  (make-pinhole-camera 
    `(pos #(1.5 0.2 4)
      lookat #(1.55 0.1 0)
      up ,y-axis
      fov 45
      aa 3)))

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

(add-to-scene (make-skylight 100000 250 #(1 1 1)))
;(add-to-scene (make-arealight #(1000 2000 2000) #(-0.5 -1 -1) 500 64 0.1))

(add-to-scene (make-halfspace #(0 1 0) #(0 0 0) white))
;(add-to-scene (make-solid-box #(-5 -1 -5) #(6 0 6) white))

;(add-to-scene (make-text "den 11. time" "HelveticaBold.ttf" 0.5 0.2 white))
(add-to-scene (make-text "META" "HelveticaBold.ttf" 0.5 0.2 white))
;(add-to-scene (make-text "den 11. time" "arial.ttf" 0.5 0.2 white))
