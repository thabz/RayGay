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
