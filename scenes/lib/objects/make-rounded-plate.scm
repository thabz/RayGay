(define (make-rounded-plate halfwidth radius material)
  (let* ((w (- halfwidth radius))
	 (x+y+ (vector w w 0))
	 (x+y- (vector w (- w) 0))
	 (x-y- (vector (- w) (- w) 0))
	 (x-y+ (vector (- w) w 0)))
    (list (make-sphere x+y+ radius material)
	  (make-sphere x+y- radius material)
	  (make-sphere x-y- radius material)
	  (make-sphere x-y+ radius material)
	  (make-cylinder x+y+ x+y- radius material)
	  (make-cylinder x+y- x-y- radius material)
	  (make-cylinder x-y- x-y+ radius material)
	  (make-cylinder x-y+ x+y+ radius material)
	  (make-solid-box 
	     (vector (- w) (- w) (- radius))
	     (vector w w radius) material))))


