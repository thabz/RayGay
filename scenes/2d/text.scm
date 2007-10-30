
(define img (make-image 1200 500 #(1 1 0.9)))


(draw-string img
     #(100 50)
     (string (integer->char 955) (integer->char 960) (integer->char 238))
;     "fix æ λ πfiles flies france wi wj Wo бомбардировки"
;      "Tove er en steg...!"
     50
;     "amazone.ttf"
     "arial.ttf"
;     "tahoma.ttf"
;    "DejaVuSerif.ttf"
     #(0 0 0))

  
;(draw-line img #(50 100) #(50 4) #(0 0 0))


(do ((y 0 (+ 1 y)))
  ((= y 8))
  (let loop ((l '()))
    (if (= (length l) 32)
        (draw-string 
          img
          (vector 10 (+ 70 (* y 58)))
          (list->string (reverse l))
          50 
          "DejaVuSerif.ttf" 
;          "amazone.ttf"
;          "tahoma.ttf"
;          "arial.ttf"
          #(0 0 0.1))
        (loop (cons (integer->char (+ (* y 32) (length l))) l)))))



(save-image img "text2a.png")
