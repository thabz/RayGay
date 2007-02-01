
(define (list-position o l)
  "Returns the position object o has in the list l"        
  (let loop ((i 0) (l l))
    (if (null? l) #f
      (if (eqv? (car l) o) i
         (loop (+ i 1) (cdr l))))))

(define (extract-keyword-argument keyword args)
  (let ((pos (list-position keyword args)))
    (if (not pos) #f
      (list-ref args (+ 1 pos)))))
           

;(extract-keyword-argument 'erik '(kurt 1 svend 2 kaj 3 erik 4))


(define (make-julia . args)
  (let ((quaternion (extract-keyword-argument ':quaternion))
        (max-iter   (extract-keyword-argument ':max-iter))
        (steps      (extract-keyword-argument ':steps))
        (accuracy   (extract-keyword-argument ':accuracy))
        (material   (extract-keyword-argument ':material)))
    (builtin-make-julia quaternion max-iter steps accuracy material)))        

