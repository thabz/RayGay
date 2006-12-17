
(load "lib/raygay.scm")

(set-image-size '(800 600))
(set-background #(0.06 0.2 0.90))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(1000 500 1800)
       lookat #(0 180 0)
       up #(0 1 0)
       fov 45
       aa 1)))

(define ground
  (make-material
    '( diffuse #(0.9 0.6 0.164)
       kd 1.0
       ks 0.0)))


(define red
  (make-material
    '( diffuse #(0.9 0.2 0.01)
       kd 0.8
       specular #(0.5 0.5 0.5)
       ks 0.2
       specpow 45)))

(define chrome
  (make-material
    '( diffuse #(0.85 0.85 0.85)
       kd 0.2
       specular #(1.0 1.0 1.0)
       ks 0.8
       specpow 45)))

(add-to-scene (make-pointlight #(100 1300 1300)))

(add-to-scene (make-sphere #(0 -100000 0) 100000 ground))

(define layers 6)
(define o 200)

(define barsize 7)
(define jointsize 30)
(define ballsize 60)

(define h (make-hash-table 63))

(define (make-pyramid top level)
  (let* ((right-front (v+ top (vector (* 0.5 o) (* o -0.7) (* 0.5 o))))
	       (left-front  (v+ top (vector (* -0.5 o) (* o -0.7) (* 0.5 o))))
	       (right-back  (v+ top (vector (* 0.5 o) (* o -0.7) (* -0.5 o))))
         (left-back   (v+ top (vector (* -0.5 o) (* o -0.7) (* -0.5 o))))
         (next-level  (+ 1 level)))
    (unless (hash-ref h top)
      (hash-set! h top #t)            
      (add-to-scene 
        (make-sphere top jointsize chrome))
      (when (< level layers)
        (add-to-scene
          (make-sphere (v+ top `#(0 ,(* o -0.7) 0)) ballsize red)
          (make-cylinder top right-front barsize chrome)
          (make-cylinder top left-front barsize chrome)
          (make-cylinder top right-back barsize chrome)
          (make-cylinder top left-back barsize chrome)
          (make-cylinder right-front left-front barsize chrome)
          (make-cylinder left-front left-back barsize chrome)
          (make-cylinder left-back right-back barsize chrome)
          (make-cylinder right-back right-front barsize chrome))
        (make-pyramid right-front next-level)
        (make-pyramid left-front next-level)
        (make-pyramid right-back next-level)
        (make-pyramid left-back next-level)))))

(make-pyramid (vector 0 (+ jointsize (* 0.7 layers o)) 0) 0)
