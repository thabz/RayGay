
;; Benchmark scene som bruges til optimere kode med
;; 
;; Tid: 14:09 minutter
;; Light occlusion stack: 11:20
;; Udskyd fresnel udregning: 11:01
;; Inlining i raytracer: 10:25
;; Inlining Vector::xProduct(): 10:10
;; Bruger Boundingbox::intersect i KdTree: 9:14
;; const i kdtree: 9.04
;; simplere callstack til shadow: 9.00
;; Valg af bedste splitdimension: 7:18
;; fastIntersect og fullIntersect på object: 6:13
;; precalc sager i Triangle::_fastIntersect: 4:30
;; inline Object::fastIntersect: 4:04
;; Backface culling in Triangle::_fastIntersect: 3:00
;; Compiler flags: 2:37
;; Fjernet inline Vector math i Triange::_fastIntersect: 1:38
;; Fjernede Object::_intersect: 1:26
;; Faster stats: 1:23
;; Brug world_bbox i alle KdTree::intersects: 1:07
;; Surface Area Heuristic cost function: 0:42
;; Fixed SAH: 0:25
;; Omrokering i Triangle::_fastIntersect: 0:22
;; Whitted-adaptive supersamling baserede på luminance: 0:18

;; Rays per pixel: 3.92 (1203260 primary rays, 3867820 total rays)

(load "lib/raygay.scm")

(set-image-size '(640 480))
(set-background '(0.1 0.1 0.3))

(set-renderer "raytracer")
(set-camera 
  (make-pinhole-camera 
    (list 'pos '(1000 1000 2000)
       'up '(0 1 0)
       'lookat '(0 100 0)
       'fov 45
       'sampler (make-whitted-adaptive-sampler 3))))

(define blue
  (make-material
    '( diffuse (0.2 0.2 0.8)
       kd 0.4
       specular (1 1 1)
       specpow 15
       ks 0.5)))

(define grey85
  (make-material
    '( diffuse (0.85 0.85 0.85)
       kd 0.7
       specular (0.85 0.85 0.85)
       specpow 15
       ks 0.3)))

(define red
  (make-material
    '( diffuse (0.8 0.2 0.2)
       kd 0.4
       specular (1 1 1)
       specpow 15
       ks 0.5)))


(add-to-scene (make-pointlight '(100 1300 1300)))

(add-to-scene (make-box '(-700 -50 -700) '(700 0 700) blue))
(add-to-scene (make-box '(-750 -50 -750) '(750 -0.01 750) blue))

(add-to-scene
  (make-extrusion 
   (make-spiral (make-circle '(0 300 0) 400 '(0 1 0))
   200 8 0)
   (make-circle '(0 0 0) 50 '(0 0 1))
   0
   12 100 red))

