
(load "vector-math.scm")
(load "handy-extensions.scm")
(load "image-sizes.scm")

(define (add-to-scene thing . rest)
  "Add a sceneobject or a list of sceneobjects to scene"
  (if (list? thing)
    (for-each __add-to-scene__ thing)
    (__add-to-scene__ thing))
  (if (not (null? rest)) (add-to-scene rest)))  

(define (set-image-size size)
  (set! __image-size__ size))

(define (set-background b)
  (set! __background__ b))

(define (set-renderer r)
  (set! __renderer__ r))

(define (set-camera c)
  (set! __camera__ c))

(load "objects.scm")    
