
(define (make-rounded-wire-box lowercorner uppercorner radius material)
  ;; Makes a wireframe box with round edges.
  ;; The radius of the round edges are 'radius'.
  (let* (
	 (x+ (- (.x uppercorner) radius))
	 (y+ (- (.y uppercorner) radius))
	 (z+ (- (.z uppercorner) radius))
	 (x- (+ (.x lowercorner) radius))
	 (y- (+ (.y lowercorner) radius))
	 (z- (+ (.z lowercorner) radius))
	 (x+y+z+ (vector x+ y+ z+))
	 (x+y+z- (vector x+ y+ z-))
	 (x+y-z+ (vector x+ y- z+))
	 (x+y-z- (vector x+ y- z-))
	 (x-y+z+ (vector x- y+ z+))
	 (x-y+z- (vector x- y+ z-))
	 (x-y-z+ (vector x- y- z+))
	 (x-y-z- (vector x- y- z-)))
    (list (make-sphere x+y+z+ radius material)
	  (make-sphere x+y+z- radius material)
	  (make-sphere x+y-z+ radius material)
	  (make-sphere x+y-z- radius material)
	  (make-sphere x-y+z+ radius material)
	  (make-sphere x-y+z- radius material)
	  (make-sphere x-y-z+ radius material)
	  (make-sphere x-y-z- radius material)
	  ; Front
	  (make-cylinder x+y+z+ x+y-z+ radius material)
	  (make-cylinder x+y-z+ x-y-z+ radius material)
	  (make-cylinder x-y-z+ x-y+z+ radius material)
	  (make-cylinder x-y+z+ x+y+z+ radius material)
	  ; Back
	  (make-cylinder x+y+z- x+y-z- radius material)
	  (make-cylinder x+y-z- x-y-z- radius material)
	  (make-cylinder x-y-z- x-y+z- radius material)
	  (make-cylinder x-y+z- x+y+z- radius material)
	  ; Back to front
	  (make-cylinder x+y+z- x+y+z+ radius material)
	  (make-cylinder x+y-z- x+y-z+ radius material)
	  (make-cylinder x-y+z- x-y+z+ radius material)
	  (make-cylinder x-y-z- x-y-z+ radius material))))


(define (make-rounded-box lowercorner uppercorner radius material)
  ;; Makes a box with round edges.
  ;; The radius of the round edges are 'radius'.
  (let* (
	 (xo+ (.x uppercorner))
	 (yo+ (.y uppercorner))
	 (zo+ (.z uppercorner))
	 (xo- (.x lowercorner))
	 (yo- (.y lowercorner))
	 (zo- (.z lowercorner))
	 (x+ (- (.x uppercorner) radius))
	 (y+ (- (.y uppercorner) radius))
	 (z+ (- (.z uppercorner) radius))
	 (x- (+ (.x lowercorner) radius))
	 (y- (+ (.y lowercorner) radius))
	 (z- (+ (.z lowercorner) radius))
	 (x+y+z+ (vector x+ y+ z+))
	 (x+y+z- (vector x+ y+ z-))
	 (x+y-z+ (vector x+ y- z+))
	 (x+y-z- (vector x+ y- z-))
	 (x-y+z+ (vector x- y+ z+))
	 (x-y+z- (vector x- y+ z-))
	 (x-y-z+ (vector x- y- z+))
	 (x-y-z- (vector x- y- z-)))
    (list (make-sphere x+y+z+ radius material)
	  (make-sphere x+y+z- radius material)
	  (make-sphere x+y-z+ radius material)
	  (make-sphere x+y-z- radius material)
	  (make-sphere x-y+z+ radius material)
	  (make-sphere x-y+z- radius material)
	  (make-sphere x-y-z+ radius material)
	  (make-sphere x-y-z- radius material)
	  ; Front
	  (make-cylinder x+y+z+ x+y-z+ radius material)
	  (make-cylinder x+y-z+ x-y-z+ radius material)
	  (make-cylinder x-y-z+ x-y+z+ radius material)
	  (make-cylinder x-y+z+ x+y+z+ radius material)
	  ; Back
	  (make-cylinder x+y+z- x+y-z- radius material)
	  (make-cylinder x+y-z- x-y-z- radius material)
	  (make-cylinder x-y-z- x-y+z- radius material)
	  (make-cylinder x-y+z- x+y+z- radius material)
	  ; Back to front
	  (make-cylinder x+y+z- x+y+z+ radius material)
	  (make-cylinder x+y-z- x+y-z+ radius material)
	  (make-cylinder x-y+z- x-y+z+ radius material)
	  (make-cylinder x-y-z- x-y-z+ radius material)
          ; Fill in boxes
          (make-solid-box (vector xo- y- z-) (vector xo+ y+ z+) material)
          (make-solid-box (vector x- yo- z-) (vector x+ yo+ z+) material)
	  (make-solid-box (vector x- y- zo-) (vector x+ y+ zo+) material))))

(define (make-solid-rounded-box lowercorner uppercorner radius material)
  ;; Makes a box with round edges. 
  ;; The radius of the round edges are 'radius'.
  ;; The elements are joined in a union so that the object
  ;; can be used in a CSG operation.
  (apply 
    make-union 
    (append 
      (make-rounded-box lowercorner uppercorner radius material) 
      (vector material))))



 
