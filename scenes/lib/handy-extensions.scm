; See http://www.processing.org/reference/index_ext.html
; for some great functions.

;(define π  3.1415926535897932384626433832795029)
;(define 2π 6.283185307179586476925286766552)
(define π (* 4.0 (atan 1.0)))
(define 2π (* 2.0 π))
(define π/2 (/ π 2.0))

(define (first vec) (car vec))
(define (second vec) (cadr vec))
(define (third vec) (caddr vec))

(define (rotate-x obj angle)
  "Shortcut for rotating around the x-axis"
  (rotate obj x-axis angle))

(define (rotate-y obj angle)
  "Shortcut for rotating around the y-axis"
  (rotate obj y-axis angle))

(define (rotate-z obj angle)
  "Shortcut for rotating around the z-axis"
  (rotate obj z-axis angle))

(define (flip-x obj)
  "Shortcut for rotating 180 degress around the x-axis"
  (rotate-x obj 180))        

(define (flip-y obj)
  "Shortcut for rotating 180 degress around the y-axis"
  (rotate-y obj 180))        

(define (flip-z obj)
  "Shortcut for rotating 180 degress around the z-axis"
  (rotate-z obj 180))        
                  
(define (displayln t)
  "Print the string t, followed by a newline"
    (display t)
    (newline ))

(define (pick-random-from-list l)
  "Returns a random element from a list"        
  (list-ref l (random (length l))))

(define (sum . l)
 "Sums of a list of numbers"
 (fold-left + 0 l))

(define (avg . l)
 "Average of a list of numbers"
 (/ (sum l)
    (length l)))

(define (lerp a b t)
  "Calculates a number between two numbers at a specific increment. The t parameter is the 
   amount to interpolate between the two values where 0.0 equal to the first point, 0.1 is 
   very near the first point, 0.5 is half-way in between, etc. The lerp function is convenient 
   for creating motion along a straight path and for drawing dotted lines."
  (+ a (* t (- b a))))

(define (constrain value upperbound lowerbound)
  (max (min value upperbound) lowerbound))

(define (remap value low1 high1 low2 high2)
  "Remaps a value in the interval [low1,high1] to the interval [low2,high2]"
  (lerp low2 high2 (/ (- value low1) (- high1 low1))))
  
(define (degrees radian-value)
  "Converts a radian measure to its corresponding value in degrees"
  (* 360.0 (/ radian-value 2π)))  

(define (radians degree-value)
  "Converts a degree measure to its corresponding value in radians"
  (* 2π (/ degree-value 360.0)))  

; TODO: Make (random b) be an alias for (random 0 b)
  
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
      
;; Unicode fun

;(define ￮ make-sphere)
;(define □ make-box)
;(define ■ make-solid-box)

