; A scene for testing photon mapping

(load "lib/objects/make-rounded-box.scm")

(set! image-size '(640 480))
(set! background '(0.1 0.1 0.3))

(set! renderer "photonrenderer")
(set! renderer "raytracer")

(set! camera 
  (make-pinhole-camera 
    '( pos 	(10 150 500)
       up 	(0 1 0)
       lookat 	(0 0 0)
       fov 	90
       aa 	4)))

(set! settings 
 '( globalphotons  	500000
    causticphotons 	1500000
    estimateradius 	20
    estimatesamples 	200
    finalgatherrays 	10
    cachetolerance 	0.1))

(set! image-size '(640 480))
(set! background '(0.1 0.1 0.3))

(set! scene 
 (list 
  (make-pointlight '(0 290 0) '(200000 200000 200000))))

(define dullwhite
  (make-material
    '( diffuse 	(1.0 1.0 1.0)
       kd 	1.0
       ks 	0.0)))

(define dullred
  (make-material
    '( diffuse 	(1.0 0.4 0.4)
       kd 	1.0
       ks 	0.0)))

(define dullgreen
  (make-material
    '( diffuse 	(0.4 1.0 0.4)
       kd 	1.0
       ks 	0.0)))

(define dullblue
  (make-material
    '( diffuse 	(0.0 0.0 0.4)
       kd 	1.0
       ks 	0.0)))

(define dull-brown1
  (make-material
    '( diffuse (0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define dull-brown2
  (make-material
    '( diffuse (0.45 0.2 0.1)
       kd 1.0
       ks 0.0)))

(define water
  (make-material
    '( diffuse 	(0 0.2 0.2)
       kd 	0.0
       specular (1 1 1)
       specpow 	30
       ks 	0.5
       kt 	0.5
       eta 	1.33)))

(append! scene
 (list
  (make-box '(-300 -350 -300) '(300 -300 1200) dullblue) ; Floor
  (make-box '(-300  300 -300) '(300  350 1200) dullwhite) ; Ceiling
  (make-box '(-300 -300 -330) '(300  300 -300) dullwhite) ; Backwall
  (make-box '(-350 -300 -300) '(-300  300 1200) dullred) ; Left wall
  (make-box '( 300 -300 -300) '(330  300 1200) dullgreen) ; Right wall
  (make-box '(-300 -300 1200) '(300 -300 1230) dullwhite) ; Wall behind camera
 ))

(define watermap
 (make-texture "gfx/water.jpg" 1 1 "bilinear"))

(append! scene
	 (list 
	  (translate
	  (make-heightfield watermap '(600 30 600) 100 100 water)
	  '(0 -100 0))))

(let loopx ((x -300))
 (begin
(let loopz ((z -300))
 (begin
  (append! scene
   (make-rounded-box 
    (list x -310 z)
    (list (+ x 50) -295 (+ z 50))
    5
    (if (= (modulo (+ x z) 100) 0)
    dull-brown1
    dull-brown2)))
  (if (< z 300)
  (loopz (+ z 50)))))
  (if (< x 300)
  (loopx (+ x 50)))))

