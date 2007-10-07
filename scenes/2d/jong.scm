
; Plotting a Peter De Jong attractor
; See http://local.wasp.uwa.edu.au/~pbourke/fractals/peterdejong/

(define image-width 1280)
(define image-height 1024)

(define image-width/4 (/ image-width -4))
(define image-height/4 (/ image-height 4))
(define image-width/2 (/ image-width 2))
(define image-height/2 (/ image-height 2))

(set-alpha-combine-mode 'decal)

(define img (make-image image-width image-height #(0 0 0)))

(define x0 0)
(define y0 0)

(define a 1.4)
(define b -2.3)
(define c 2.4)
(define d -2.1)

(define NUM 1000000)
(define SKIP 1000)

(do ((x x0 (- (sin (* a y)) (cos (* b x))))
     (y y0 (- (sin (* c x)) (cos (* d y))))
     (i 0  (+ i 1)))
  ((= i NUM))
  (if (> i SKIP)
     (set-pixel img 
       (+ image-width/2 (* x image-width/4))
       (+ image-height/2 (* y image-height/4))
       #(1 1 1 0.1))))

(apply-gaussian-blur img 200)

(save-image img "jong2.png")
