
(load "lib/raygay.scm")
(load "lib/objects/make-venus.scm")
(load "lib/objects/make-continuous-studio-backdrop.scm")
(load "lib/colors.scm")

(define fast-preview #t)

; Times:
; Old allIntersections(vector<Intersection> ...)   50.28s
; allIntersections with alloca Intersections       19.62s

;; -------------------------------------------------------------------------
;; Materiale
;; -------------------------------------------------------------------------

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

(define string-material (body-material (color 'peach-puff)))

;; -------------------------------------------------------------------------
;; Objects 
;; -------------------------------------------------------------------------

(define (rotate-random obj)
 (rotate-z (rotate-y (rotate-x obj (random 0 360)) (random 0 360)) (random 0 360)))

(define (clip-with-sphere-shell radius thickness objects material)
 (make-intersection
  (apply make-union objects)
  (make-difference
    (make-sphere #(0 0 0) radius)
    (make-sphere #(0 0 0) (- radius thickness)))
  material))

(define (great-circles radius thickness circles-num material)
 (define (slab)
  (make-intersection
    (make-halfspace #(0 -1 0) (vector 0 (- thickness) 0))
    (make-halfspace #(0 1 0) (vector 0 thickness 0))))
;   (make-solid-box (vector -100 (- thickness) -100) (vector 100 thickness 100)))
 (let loop ((slabs '())
            (i circles-num))
  (if (zero? i)
   (clip-with-sphere-shell radius thickness slabs material) 
   (loop (cons (rotate-random (slab)) slabs)
         (- i 1)))))

(define (dandelions radius small-radius thickness circles-num material)
 (define (tube)
   (make-difference
     (make-cylinder (vector 0 (- radius) 0) (vector 0 radius 0) small-radius)
     (make-cylinder (vector 0 (- radius) 0) (vector 0 radius 0) (- small-radius thickness))))
 (let loop ((tubes '())
            (i circles-num))
  (if (zero? i)
   (clip-with-sphere-shell radius thickness tubes material) 
   (loop (cons (rotate-random (tube)) tubes)
         (- i 1)))))

(define (hang-ball obj radius pos string-radius)
   (define top (vector 
		(vector-ref pos 0) 
		(+ (vector-ref pos 1) radius)
		(vector-ref pos 2)))
   (add-to-scene (translate obj pos)) 
   (add-to-scene (make-cylinder 
		  (translate pos (vector 0 radius 0)) 
		  (translate pos (vector 0 1000 0)) 
                  string-radius
		  string-material)))

;; -------------------------------------------------------------------------
;; Scene creation
;; -------------------------------------------------------------------------

(define aa (if fast-preview 0 3))
(define radius 10)
(define small-radius 2)
(define thickness 0.25)
(define string-radius 0.1)

(if fast-preview
 #(256 160)
 (set-image-size image-size-30-inch-apple-cinema))

;(set-background (color 'white))
;(set-background (make-texture "gfx/christmas-tree-inside-the-house.jpg" 1 1 'bilinear))
(set-background (color 'dark-red))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera (list 
       'pos #(0 8 80)
       'lookat #(0 10 0)
       'up #(0 1 0)
       'fov 45
       'aa aa)))

(if fast-preview
 (add-to-scene (make-pointlight #(1300 1300 1300)))
 (add-to-scene (make-arealight #(1300 1300 1300) #(-1 -1 -1) 200 256 0.1)))

(hang-ball 
 (dandelions radius small-radius thickness 100 (body-material (color 'peach-puff)))
 radius #(10 30 20) string-radius)

(hang-ball
 (dandelions radius small-radius thickness 100 (body-material (color 'peach-puff)))
 radius #(-20 15 -10) string-radius)

(hang-ball 
 (great-circles radius thickness 20 (body-material (color 'papaya-whip)))
 radius #(20 10 10) string-radius)

(hang-ball 
 (great-circles radius thickness 20 (body-material (color 'papaya-whip)))
 radius #(0 10 -20) string-radius)

(hang-ball
 (dandelions radius small-radius thickness 100 (body-material (color 'peach-puff)))
 radius #(-20 5 20) string-radius)

