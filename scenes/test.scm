
(load "globals.scm")

(define red-mat 
 (make-material
  '( "diffuse" (0.8 0.4 0.4)
      "kd" 0.8
      "ks" 0.2)))

(define kurt '(7 9 13))

;; Tilføjer en kugle    
(define a 21)

(display #(a 0 0))    

;; Markerer renderer    
;(set-renderer "raytracer")
(define kugle (make-sphere '(0 50 0) 95 red-mat))
(define boks (make-box '(0 50 0) '(10 60 10) red-mat))
(define circle (make-circle (list . (a 1 10)) 20 '(0 1 0)))
(define spiral (make-spiral circle 100 10 0.0))

(make-bezierspline '((1 2 3) (3 4 5) (1 2 4)))
;(make-texture "gfx/water.jpg" 1 1 "none")

(define (make-necklace num)
  (let iter ((i num))
    (cond ((= i 0) '())
	  (else (append 
		  (list . (i)) 
		  (iter (- i 1)))))))

(display (sequence 10))    
(newline)
(display (/ 1 2))
(newline)
