; A scene for testing photon mapping

(set! image-size '(640 480))
(set! background '(0.1 0.1 0.3))

(set! renderer "raytracer")
(set! renderer "photonrenderer")

(set! camera 
  (make-pinhole-camera 
    '( pos 	(10 150 500)
       up 	(0 1 0)
       lookat 	(0 0 0)
       fov 	90
       aa 	0)))

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
 (make-texture "gfx/water.jpg" 1 1 "none"))

(append! scene
	 (list 
	  (translate
	  (make-heightfield watermap '(600 30 600) 100 100 water)
	  '(0 -100 0))))

