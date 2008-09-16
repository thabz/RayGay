;; Texture is from http://www.oera.net/How2/TextureMaps2.htm
;; They also supply textures of skies and bumpmaps.
;; HQ textures from
;; http://www.celestiamotherlode.net/catalog/earth.php

(load "lib/raygay.scm")

(define new-york   '(40 43 0 N 74 0 0 W))
(define paris      '(48 52 0 N 2 19 59 E))
(define tokyo      '(35 41 0 N 139 46 0 E))
(define london     '(51 30 28 N 0 7 41 W))
(define aarhus     '(56 09 00 N 10 13 0 E))
(define moscow     '(55 45 08 N 37 37 56 E))
(define berlin     '(52 31 00 N 13 25 00 E))
(define oslo       '(59 56 58 N 10 45 23 E))
(define stockholm  '(59 21 00 N 18 4 0 E))
(define cairo      '(30 03 00 N 31 22 0 E))
(define casablanca '(33 32 0 N 7 35 0 W))
(define madrid     '(40 24 0 N -3 -41 0 E))
(define rome       '(41 54 0 N 12 30 0 E))
(define rio-de-janeiro '(22 54 30 S 43 11 47 W))
(define buenos-aires '(34 36 0 S 58 22 11 W))
(define santiago   '(33 26 0 S 70 40 0 W))
(define sydney   '(33 51 36 S 151 12 40 E))
(define wellington   '(41 17 20 S 174 46 38 E))

(define (degrees->radians deg)
  (* 2 π (/ deg 360.0)))

(define (latlong->unitsphere latlong)
  (let* ((lat (degrees->radians (+ (car latlong) (/ (cadr latlong) 60) (/ (caddr latlong) 360))))
         (lo (cddddr latlong))
         (long (degrees->radians (+ (car lo) (/ (cadr lo) 60) (/ (caddr lo) 360))))
         (phi (- π/2 (if (equal? (cadddr latlong) 'N) lat (- lat))))
         (rho (if (equal? (cadddr lo) 'W) (- long) long)))
    (vector
      (* (sin rho) (sin phi))
      (cos phi)
      (* (cos rho) (sin phi)))))

(define (add-pin latlong height)
  (let ((p (latlong->unitsphere latlong)))
    (add-to-scene
      (make-cylinder
        (vscale p earth-radius)
        (vscale p (+ earth-radius height))
        (/ height 50)
        chrome))
    (add-to-scene
      (make-sphere
        (vscale p (+ earth-radius height))
        (/ height 10)
        red))))  
    

(define earth-radius 6371.0)
(define camera-height 10)
(define camera-position aarhus)
(define camera-lookat paris)
(define camera-unit-sphere (latlong->unitsphere camera-position))
(define sun-above casablanca)  

(set-image-size '(640 480))
(set-image-size image-size-23-inch-apple-cinema)
(set-background #(0 0 0))
(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    (list 'pos (vscale camera-unit-sphere (+ camera-height earth-radius))
          ;'pos (vscale #(0 0 1) (+ camera-height earth-radius))
          'lookat (vscale (latlong->unitsphere camera-lookat) earth-radius)
          'up camera-unit-sphere
          'fov 45
          'aa 0)))


(define tiles '())

(define ground-texture
  (if #f
    (make-texture "gfx/EarthMap_2500x1250.jpg" 1 1 'bilinear)
    (make-multi-texture
    (do ((y 0 (+ y 1))) ((= y 32) (reverse tiles))
     (do ((x 0 (+ x 1))) ((= x 64))
       (set! tiles (cons 
         (string-append "gfx/level5/tx_" (number->string x) "_" (number->string y) ".jpg") tiles))))
    64 50 1 1 'bilinear)))

(define ground-material
  (make-material
    (list 'diffuse ground-texture
          'kd 1.0
          'ks 0.0)))

(define red
  (make-material
    '( diffuse #(0.8 0.2 0.3)
       kd 0.5
       specular #(1.0 1.0 1.0)
       ks 0.5
       specpow 30)))

(define chrome
  (make-material
    '( diffuse #(0.5 0.5 0.5)
       kd 0.1
       specular #(1.0 1.0 1.0)
       ks 0.9
       specpow 30)))

(add-to-scene 
  (make-pointlight (vscale (latlong->unitsphere sun-above) 1.495e8)))
 

;(make-pointlight (vscale camera-unit-sphere 30000))

(add-to-scene
  (rotate 
    (make-ellipsoid #(0 0 0) (vector earth-radius earth-radius earth-radius) ground-material)
    y-axis
  -90))

(define l 300)  
(add-pin london l)
(add-pin paris l)
;(add-pin aarhus l)
;(add-pin moscow l)
(add-pin berlin l)
(add-pin oslo l)
(add-pin stockholm l)
(add-pin cairo l)
(add-pin casablanca l)
(add-pin madrid l)
(add-pin buenos-aires l)
(add-pin santiago l)
(add-pin rio-de-janeiro l)
(add-pin wellington l)
(add-pin sydney l)


