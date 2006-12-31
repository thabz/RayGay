(load "lib/raygay.scm")
(load "lib/objects/make-rounded-box.scm")
(load "lib/objects/make-rounded-cylinder.scm")

(define type 'slow)

(set-image-size '(1600 1200))
(set-image-size '(800 600))
(set-background #(0.3 0.6 0.7))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    '( pos #(200 200 400)
       lookat #(0 0 0)
       up #(0 1 0)
       fov 45
       aa 3)))

(case type
  ((fast)
    (add-to-scene (make-pointlight #(-500 1500 1300)))   
    (define (box v1 v2 color)
      (make-solid-box v1 v2 color))
    (define (cylinder v1 v2 r color)
      (make-cylinder v1 v2 r color)))
  ((slow)
    (add-to-scene (make-skylight 10000 512 #(1 1 1)))
    (add-to-scene (make-pointlight #(0 8 0)))   
    (define (box v1 v2 color)
      (make-rounded-box v1 v2 0.2 color))
    (define (cylinder v1 v2 r color)
      (let ((l (vlength (v- v1 v2))))
        (translate
          (make-top-rounded-cylinder l r 0.2 color)
          v1)))))


(define ground
  (make-material
    '( diffuse #(1.0 1.0 1.0)
       kd 1.0
       ks 0.0)))

(define grey85
  (make-material
    '( diffuse #(0.85 0.85 0.85)
       kd 1.0
       ks 0.0)))

(define red
  (make-material
    '( diffuse #(0.8 0.0 0.0)
       kd 1.0)))

(define green
  (make-material
    '( diffuse #(0.1 0.7 0.0)
       kd 1.0)))

(define yellow
  (make-material
    '( diffuse #(1.0 0.9 0.0)
       kd 1.0)))

(define gray
  (make-material
    '( diffuse #(0.6 0.6 0.6)
       kd 1.0)))

(define blue
  (make-material
    '( diffuse #(0.2 0.2 1.0)
       kd 1.0)))


(add-to-scene (make-box #(-1700 -51 -1700) #(1700 1 1700) ground))



(define heightmap (make-hash-table 63))

(define (get-height xbegin zbegin width depth height)
  (let ((max-height 0))
    (dotimes x width
       (dotimes z depth
         (let ((h (hash-ref heightmap 
                            (list (+ x xbegin) (+ z zbegin)))))
           (if (and h (> h max-height))
             (set! max-height h)))))
     max-height))         
           
(define (set-heights xbegin zbegin width depth height)
  (dotimes x width
    (dotimes z depth
       (hash-set! heightmap (list (+ x xbegin) (+ z zbegin)) height))))  

(define (stud width depth color)
  (let ((result (list width depth 9.6
                      (box #(0 0 0) (vector (* 8 width) 9.6 (* 8 depth)) color))))
     (dotimes x width
       (dotimes z depth
         (let ((c (vector (+ 4 (* 8 x)) 9.6 (+ 4 (* 8 z)))))
         (append! result 
           (list (cylinder c (v+ c #(0 1.8 0)) 2.4 color))))))
     result))           

(define (plate width depth color)
 (let ((result (list width depth 3.2 
                     (box #(0 0 0) (vector (* 8 width) 3.2 (* 8 depth)) color))))
    (dotimes x width
      (dotimes z depth
        (let ((c (vector (+ 4 (* 8 x)) 3.2 (+ 4 (* 8 z)))))
        (append! result 
          (list (cylinder c (v+ c #(0 1.8 0)) 2.4 color))))))
    result))           

(define (1x1-stud color)
  (stud 1 1 color))

(define (1x2-stud color)
  (stud 1 2 color))

(define (1x4-stud color)
  (stud 1 4 color))

(define (4x1-stud color)
  (stud 4 1 color))

(define (2x1-stud color)
  (stud 2 1 color))

(define (2x2-stud color)
  (stud 2 2 color))

(define (4x2-stud color)
  (stud 4 2 color))

(define (2x4-stud color)
  (stud 2 4 color))

(define (1x1-plate color)
  (plate 1 1 color))

(define (4x2-plate color)
  (plate 4 2 color))

(define (add-brick brick-generator x-offset z-offset color)
  (let* ((generated (brick-generator color))
         (obj (cdddr generated))
         (width (car generated))
         (depth (cadr generated))
         (height (caddr generated))
         (ypos (get-height x-offset z-offset width depth height)))
    (set-heights  x-offset z-offset width depth (+ ypos height))
    (add-to-scene
      (translate
        obj    
        (vector (* 8 x-offset) ypos (* 8 z-offset))))))


(dotimes i 2
(add-brick 4x1-stud -4 5 yellow)
(add-brick 4x1-stud 0 5 yellow)
(add-brick 2x1-stud 6 5 yellow)
(add-brick 4x1-stud -4 -4 yellow)
(add-brick 4x1-stud 0 -4 yellow)
(add-brick 4x1-stud 4 -4 yellow)
(add-brick 1x4-stud 7 -3 yellow)
(add-brick 1x4-stud 7 1 yellow)
(add-brick 1x4-stud -4 -3 yellow)
(add-brick 1x4-stud -4 1 yellow))

(dotimes i 2
(add-brick 1x1-stud -4 5 yellow)
(add-brick 2x1-stud -3 5 yellow)
(add-brick 2x1-stud 2 5 yellow)
(add-brick 2x1-stud 6 5 yellow)
(add-brick 4x1-stud -4 -4 yellow)
(add-brick 4x1-stud 0 -4 yellow)
(add-brick 4x1-stud 4 -4 yellow)
(add-brick 1x4-stud 7 -3 yellow)
(add-brick 1x4-stud 7 1 yellow)
(add-brick 1x4-stud -4 -3 yellow)
(add-brick 1x4-stud -4 1 yellow))

(add-brick 4x1-stud -4 5 yellow)
(add-brick 4x1-stud 0 5 yellow)
(add-brick 2x1-stud 6 5 yellow)
(add-brick 4x1-stud -4 -4 yellow)
(add-brick 4x1-stud 0 -4 yellow)
(add-brick 4x1-stud 4 -4 yellow)
(add-brick 1x4-stud 7 -3 yellow)
(add-brick 1x4-stud 7 1 yellow)
(add-brick 1x4-stud -4 -3 yellow)
(add-brick 1x4-stud -4 1 yellow)

(add-brick 2x1-stud -4 5 yellow)
(add-brick 4x1-stud -2 5 yellow)
(add-brick 4x1-stud  2 5 yellow)
(add-brick 2x1-stud  6 5 yellow)
(add-brick 4x1-stud -4 -4 yellow)
(add-brick 4x1-stud 0 -4 yellow)
(add-brick 4x1-stud 4 -4 yellow)
(add-brick 1x4-stud 7 -3 yellow)
(add-brick 1x4-stud 7 1 yellow)
(add-brick 1x4-stud -4 -3 yellow)
(add-brick 1x4-stud -4 1 yellow)


; Roof 1st layer
(add-brick 4x2-stud -5 5 red)
(add-brick 4x2-stud -1 5 red)
(add-brick 2x2-stud 3 5 red)
(add-brick 4x2-stud 5 5 red)
(add-brick 4x2-stud -5 -5 red)
(add-brick 4x2-stud -1 -5 red)
(add-brick 2x2-stud 3 -5 red)
(add-brick 4x2-stud 5 -5 red)

(add-brick 2x4-stud -5 -3 red)
(add-brick 2x4-stud -5 1 red)
(add-brick 2x4-stud 7 -3 red)
(add-brick 2x4-stud 7 1 red)

; Roof 2nd layer
(add-brick 4x2-stud -4 -4 red)
(add-brick 4x2-stud 0 -4 red)
(add-brick 4x2-stud 4 -4 red)
(add-brick 4x2-stud -2 4 red)
(add-brick 4x2-stud 2 4 red)

(add-brick 2x4-stud -4 -2 red)
(add-brick 2x4-stud -4 2 red)
(add-brick 2x4-stud 6 -2 red)
(add-brick 2x4-stud 6 2 red)

; Roof 3rd layer
(add-brick 4x2-stud -3 -3 red)
(add-brick 4x2-stud 1 -3 red)
(add-brick 2x4-stud -3 -1 red)
(add-brick 2x4-stud 5 -3 red)
(add-brick 2x4-stud 5 1 red)
(add-brick 4x2-stud -3 3 red)
(add-brick 4x2-stud 1 3 red)

; Roof 4th layer
(add-brick 4x2-stud -2 -2 red)
(add-brick 4x2-stud 2 -2 red)
(add-brick 2x4-stud -2 0 red)
(add-brick 2x4-stud 4 0 red)
(add-brick 4x2-stud 0 2 red)

; Roof 5th layer
(add-brick 4x2-stud -1 -1 red)
(add-brick 2x2-stud 3 -1 red)
(add-brick 2x2-stud -1 1 red)
(add-brick 4x2-stud 1 1 red)

; Roof 6th layer
(add-brick 4x2-stud 0 0 red)


; Tree
(add-brick 2x1-stud -10 10 green)
(add-brick 4x1-stud -13 10 green)
(add-brick 4x1-stud -9 10 green)
(add-brick 4x1-stud -13 10 green)
(add-brick 4x1-stud -9 10 green)
(add-brick 4x1-stud -12 10 green)
(add-brick 2x1-stud -8 10 green)
(add-brick 4x1-stud -12 10 green)
(add-brick 2x1-stud -8 10 green)
(add-brick 4x1-stud -11 10 green)
(add-brick 4x1-stud -11 10 green)
(add-brick 2x1-stud -10 10 green)
(add-brick 2x1-stud -10 10 green)
