
(load "lib/raygay.scm")
(load "lib/objects/make-venus.scm")
(load "lib/objects/make-continuous-studio-backdrop.scm")
(load "lib/colors.scm")

(set-image-size '(1024 768))
(set-background (color 'white))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera '( 
       pos #(0 5 40)
       lookat #(0 5 16)
       up #(0 1 0)
       fov 45
       aa 3)))

(define (body-material c)
  (make-material
    (list 'diffuse c
	        'kd 0.8
	        'specular (color 'white)
   	      'ks 0.2
          'specpow 35)))

(define matte-white 
  (make-material
    (list 'diffuse (color 'white)
	  'kd 1.0
	  'ks 0.0)))

;(add-to-scene (make-pointlight #(1300 1300 1300)))
(add-to-scene (make-arealight #(1300 1300 1300) #(-1 -1 -1) 200 256 0.1))

(add-to-scene (make-continuous-studio-backdrop #(40 30 40) 10 matte-white))  

(add-to-scene (translate (make-venus (body-material (color 'papaya-whip))) #(-5.5 5 16)))
(add-to-scene (translate (make-venus (body-material (color 'peach-puff))) #(0 5 16)))
(add-to-scene (translate (make-venus (body-material (color 'navajo-white))) #(5.5 5 16)))
    
