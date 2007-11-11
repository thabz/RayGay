
;(define PI  3.1415926535897932384626433832795029)
;(define TWO-PI 6.283185307179586476925286766552)
(define PI (* 4.0 (atan 1.0)))
(define TWO-PI (* 2.0 PI))
(define HALF-PI (/ PI 2.0))

(define (first vec) (car vec))
(define (second vec) (cadr vec))
(define (third vec) (caddr vec))

; Shortcut for rotating around the x-axis
(define (rotate-x obj angle)
    (rotate obj x-axis angle))

; Shortcut for rotating around the y-axis        
(define (rotate-y obj angle)
    (rotate obj y-axis angle))

; Shortcut for rotating around the z-axis
(define (rotate-z obj angle)
    (rotate obj z-axis angle))

; Shortcut for rotating 180 degress around the x-axis
(define (flip-x obj)
    (rotate-x obj 180))        

; Shortcut for rotating 180 degress around the y-axis
(define (flip-y obj)
    (rotate-y obj 180))        

; Shortcut for rotating 180 degress around the z-axis
(define (flip-z obj)
    (rotate-z obj 180))        
                  
; Print followed by a newline                                            
(define (displayln t)
    (display t)
    (newline ))

(define (pick-random-from-list l)
  "Returns a random element from a list"        
  (list-ref l (random (length l))))

(define (keyframing t keyframes)
  (let* ((sum (apply + (map car keyframes)))
         (scaled-t (* sum t)))
         (curframe (do ((i 0 (+ i 1)))
                        (k keyframes (cdr keyframes))
                        (t scaled-t (- t (caar keyframes)))  
                     ((<= t 0) k)))
      (display (cdr curframe))))               

(define-macro (unless test . consequent)
   "A Common LISP style unless macro"        
   `(if (not ,test) (begin ,@consequent)))    

(define-macro (when test . consequent)
   "A Common LISP style when macro"        
   `(if ,test (begin ,@consequent)))    

(define-macro (dotimes var times . body)
  "A Common LISP style dotimes macro"        
  `(do ((,var 0 (+ 1 ,var)))
    ((= ,var ,times))
    ,@body))

(define-macro (dolist var items . body)
  "A Common LISP style dolist macro"        
  `(let loop ((,var (car ,items))
	      (rest (cdr ,items)))
    ,@body
    (unless (null? rest)
      (loop (car rest) (cdr rest)))))
      
(define-macro (dostep var from to step . body)
  "FOR loop construct as found in BASIC"
  `(do ((,var ,from (+ ,step ,var)))
    ((>= ,var ,to))
    ,@body))
      
      
      