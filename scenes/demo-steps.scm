
(load "lib/raygay.scm")

(set-image-size '(1024 768))
(set-background '(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos (2 17 20)
       lookat (0 0 0)
       up (0 1 0)
       fov 45
       aa 2)))

(define chrome
  (make-material
    '( diffuse (0.9 0.9 0.9)
       kd 0.4
       specular (1.0 1.0 1.0)
       ks 0.6
       specpow 45)))



(add-to-scene (make-pointlight '(-500 1300 1300)))

(define materials
  (list 
     (make-material
       '( diffuse (0.6 0.8 0.9)
          kd 1.0
          ks 0.0
          specpow 45))
      (make-material
        '( diffuse (0.6 0.9 0.8)
           kd 1.0
           ks 0.0
           specpow 45))
       (make-material
         '( diffuse (0.6 0.9 0.99)
            kd 0.8
            specular (1.0 1.0 1.0)
            ks 0.2
            specpow 45))
))          
                
(define (make-jittered-box x z)
(translate
  (rotate-z
    (rotate-x
      (rotate-y        
        (make-rounded-box '(-0.5 -0.2 -0.5) '(0.5 0.2 0.5) 0.1 (pick-random-from-list materials))        
        (random2 -10 10))
      (random2 -5 5))
    (random2 -5 5))
  (list x 0 z)))
                                                 
(do ((x -5 (+ 1 x)))
  ((= x 5))
    (do ((z -5 (+ 1 z)))
      ((= z 5))
        (add-to-scene (make-jittered-box x z))))

;(add-to-scene 
;  (make-sphere '(0 3 0) 2.8 chrome))