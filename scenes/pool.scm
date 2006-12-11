; A scene for testing photon mapping

(load "lib/raygay.scm")
(load "lib/objects/make-rounded-box.scm")

(set-image-size '(640 480))
(set-background #(0.1 0.1 0.3))

(set-renderer "raytracer")
(set-renderer "photonrenderer")

(set-camera 
  (make-pinhole-camera 
    '( pos 	#(10 150 500)
       up 	#(0 1 0)
       lookat 	#(0 0 0)
       fov 	90
       aa 	4)))

(set-settings 
 '( globalphotons  	500000
    causticphotons 	1500000
    estimateradius 	20
    estimatesamples 	200
    finalgatherrays 	10
    cachetolerance 	0.1))

(add-to-scene
  (make-pointlight #(0 290 0) #(200000 200000 200000)))

(define dullwhite
  (make-material
    '( diffuse 	#(1.0 1.0 1.0)
       kd 	1.0
       ks 	0.0)))

(define dullred
  (make-material
    '( diffuse 	#(1.0 0.4 0.4)
       kd 	1.0
       ks 	0.0)))

(define dullgreen
  (make-material
    '( diffuse 	#(0.4 1.0 0.4)
       kd 	1.0
       ks 	0.0)))

(define dullblue
  (make-material
    '( diffuse 	#(0.0 0.0 0.4)
       kd 	1.0
       ks 	0.0)))

(define dull-brown1
  (make-material
    '( diffuse #(0.7 0.4 0.2)
       kd 1.0
       ks 0.0)))

(define dull-brown2
  (make-material
    '( diffuse #(0.45 0.2 0.1)
       kd 1.0
       ks 0.0)))

(define water
  (make-material
    '( diffuse 	#(0 0.2 0.2)
       kd 	0.0
       specular #(1 1 1)
       specpow 	30
       ks 	0.5
       kt 	0.5
       eta 	1.33)))

(add-to-scene
 (list
  (make-solid-box #(-300 -350 -300) #(300 -300 1200) dullblue) ; Floor
  (make-solid-box #(-300  300 -300) #(300  350 1200) dullwhite) ; Ceiling
  (make-solid-box #(-300 -300 -330) #(300  300 -300) dullwhite) ; Backwall
  (make-solid-box #(-350 -300 -300) #(-300  300 1200) dullred) ; Left wall
  (make-solid-box #( 300 -300 -300) #(330  300 1200) dullgreen) ; Right wall
  (make-solid-box #(-300 -300 1200) #(300 -300 1230) dullwhite) ; Wall behind camera
 ))

(define watermap
 (make-texture "gfx/water.jpg" 1 1 "bilinear"))

(add-to-scene
	  (translate
	  (make-heightfield watermap #(600 30 600) 100 100 water)
	  #(0 -100 0)))

(do ((x -300 (+ x 50)))
  ((>= x 300))
  (do ((z -300 (+ z 50)))
    ((>= z 300))
    (add-to-scene
      (make-rounded-box 
        (vector x -310 z)
        (vector (+ x 50) -295 (+ z 50))
        5
        (if (= (modulo (+ x z) 100) 0)
          dull-brown1
          dull-brown2)))))

