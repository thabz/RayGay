#!/opt/local/bin/raygay-repl

(load "lib/colors.scm")

(define image (make-image 800 600 #(1 1 1)))

(do ((h 0 (+ h 0.1)))
  ((>= h 1.0))
  (let ((c (hsv->rgb (vector h 1 0.5)))
        (x (* h 300)))
     (draw-filled-box image (vector x 0) #(20 20) c)
     (draw-filled-box image (vector x 20) #(20 20) (color-brighten c 0.2))))

(define gradient (color-gradient 40 (list color-azure color-linen color-orange)))

(let loop ((i 0) (gradient gradient))
  (if (< i 40) 
    (begin
      (draw-filled-box image (vector (* i 20) 50) #(20 20) (car gradient))
      (loop (+ i 1) (cdr gradient)))))

(save-image image "colors.png")
