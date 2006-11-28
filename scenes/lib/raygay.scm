
(load "vector-math.scm")
(load "handy-extensions.scm")

(define (add-to-scene thing)
  "Add a sceneobject or a list of sceneobjects to scene"
  (if (list? thing)
    (for-each add-to-scene thing)
    (set! __scene__ (cons thing __scene__))))

(define (set-image-size size)
  (set! __image-size__ size))

(define (set-background b)
  (set! __background__ b))

(define (set-renderer r)
  (set! __renderer__ r))

(define (set-camera c)
  (set! __camera__ c))

(load "objects.scm")    
