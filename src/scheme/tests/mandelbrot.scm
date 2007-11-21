; Plots a Mandelbrot fractal to the console

(define max-iterations 100)

(define (mandelbrot c)
 (define (mandelbrot-inner z iteration)
  (cond ((> (magnitude z) 4.0) 'escaped)
        ((= iteration max-iterations) 'did-not-escape)
	(else (mandelbrot-inner (+ c (* z z)) (+ 1 iteration)))))
 (mandelbrot-inner c 0))

(do ((y -1 (+ y 0.1)))
 ((>= y 1))
 (do ((x -2 (+ x 0.05)))
  ((>= x 1) (newline))
  (case (mandelbrot (make-rectangular x y))
    ((escaped) (display "."))
    ((did-not-escape) (display "#")))))

