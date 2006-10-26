
(load "handy-extensions.scm")

(define (add-to-scene thing)
  ;; Add a sceneobject or a list of sceneobjects to scene
  (if (list? thing)
    (for-each add-to-scene thing)
    (if (null? __scene__)
      (set! __scene__ (list thing))
      (append! __scene__ (list thing)))))

(define (set-image-size size)
  (set! __image-size__ size))

(define (set-background b)
  (set! __background__ b))

(define (set-renderer r)
  (set! __renderer__ r))

(define (set-camera c)
  (set! __camera__ c))

(load "objects.scm")    
