
(load "scenes/lib/colors.scm")

(define image (make-image 800 600 #(1 1 1)))

(do ((h 0 (+ h 10)))
  ((>= h 360))
  (let ((c (hsv->rgb (vector h 0.5 1))))
     (draw-filled-box image (vector h 0) #(20 20) c)
     (draw-filled-box image (vector h 20) #(20 20) (color-complement c))))

(save-image image "colors.png")
