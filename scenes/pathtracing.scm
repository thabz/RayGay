
(load "lib/raygay.scm")

(define bgimage (make-image 1024 1024 #(1.2 1.2 1.2)))
;(define bgimage (make-image 1024 1024 (vector (/ 199 255) (/ 229 255) 1)))

(define tex (make-texture bgimage 1 1 'bilinear))

(set-background (make-texture bgimage 1 1 'bilinear))
(set-image-size '(640 480))
(set-renderer "pathtracer")

(set-camera 
  (make-pinhole-camera 
    (list 'pos #(0 1.8 6)
          'lookat #(0 0 0)
          'up y-axis
          'fov 45
          'sampler (make-halton-sampler 40))))

(define white
  (make-material
    '( diffuse #(1 1 1)
       kd 0.5
       gloss (100 10.0)
       ks 0.5)))

(define green
  (make-material
    '( diffuse #(0.2 1.0 0.2)
       kd 1.0
       ks 0.0)))

(define red
  (make-material
    '( diffuse #(1.0 0.2 0.2)
       kd 1.0
       ks 0.0)))

(define textured
  (make-material
    `( diffuse ,tex
       kd 1.0
       ks 0.0)))


(define skyblue
  (make-material
    (list 'diffuse (vector (/ 199 255) (/ 229 255) 1)
          'kd 1.0
          'ks 0.0)))

(add-to-scene
  (make-solid-box #(-4 -1 -4) #(4 0 4) white))

(add-to-scene
  (make-solid-box #(-0.5 0 -0.5) #(0.5 1 0.5) green))

(add-to-scene
  (make-sphere #(1 0.5 1) 0.5 red))
