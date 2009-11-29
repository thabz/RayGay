;;
;; Wrappers for built-in functions dealing with scene objects.
;;

(define (make-ply-mesh filename material)
  "Loads a PLY mesh"
  ($make-ply-mesh filename material))

(define (make-obj-mesh filename material)
  "Loads a PLY mesh"
  ($make-obj-mesh filename material))

(define (bounding-box o)
  "Returns a list (min max) of two vectors. min being the minimum 
   corner and max being the maximum corner of the axis-aligned
   boundingbox of the sceneobject o."
  ($bounding-box o))

(define (inside? object point)
  "Says whether the point is inside the solid object"
  ($inside? object point))

(define (intersect object ray-origin ray-direction)
  "Find the intersection between an object and a ray.
   Returns #f if no intersection; otherwise a list (point normal)"
  ($intersect object ray-origin ray-direction))

(define (all-intersections object ray-origin ray-direction)
  "Find all the intersection between a solid object and a ray.
   Returns a list of (point normal) lists."
  ($all-intersections object ray-origin ray-direction))

