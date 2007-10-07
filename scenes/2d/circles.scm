
(define img (make-image 1000 1000 #(1 1 1)))


(do ((i 0 (+ 1 i)))
  ((= i 50))
    (draw-circle
       img 
       (vector (random 0 1000) (random 0 1000)) 
       (random 20 600)
       #(0 0 0)))

(save-image img "circles.png")
