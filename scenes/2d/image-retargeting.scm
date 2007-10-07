
;; Implementing a version of "Seam Carving for Content-Aware Image Resizing"
;; as invented by Shai Avidan and Ariel Shamir for their SIGGRAPH 2007 paper.
;; See http://www.faculty.idc.ac.il/arik/

; Returns a new image from image shifted along a seam.
; The new image is one pixel smaller in width.
(define (shift-image image seam)
  (let* ((new-width (- (image-width image) 1))
         (new-image (make-image new-width (image-height image))))
    (let yloop ((seam (cdr seam))
                (y 0))
      (if (null? seam)
         new-image
         (let xloop ((x 0))
           (if (>= x new-width)
             (yloop (cdr seam) (+ 1 y))
             (begin
                (set-pixel new-image x y 
                  (get-pixel image 
                    (if (>= x (car seam)) (+ x 1) x)
                    y))
                (xloop (+ 1 x)))))))))

; Draw a seam for testing
(define (draw-seam image seam colour)
  (let loop ((y 0)
            (seam (cdr seam)))
    (if (null? seam)
        image
        (begin
          (set-pixel image (car seam) y colour)
          (loop (+ 1 y) (cdr seam))))))

; Calculates the difference between two colours.
(define (colour-diff a b)
  (+ (abs (- (vector-ref a 0) (vector-ref b 0)))
     (abs (- (vector-ref a 1) (vector-ref b 1)))
     (abs (- (vector-ref a 2) (vector-ref b 2)))))

; Find the penalty of going from (x-from,y-from) to (x-to,y-from+1).
; Punishes going outside the image.
(define (penalty-of-step image x-from y-from x-to)
  (if (or (< x-to 0) (>= x-to (image-width image)))
    1000
    (colour-diff (get-pixel image x-from y-from)
                 (get-pixel image x-to (+ 1 y-from)))))

; Returns the pair (x . penalty) of the best next step for 
; an ongoing seam at position (x,y).
(define (best-next-step image x y)
  (let ((penalty-1 (penalty-of-step image x y (- x 1)))
        (penalty+0 (penalty-of-step image x y x))
        (penalty+1 (penalty-of-step image x y (+ x 1))))
    (cond
       ((and (<= penalty-1 penalty+0) (<= penalty-1 penalty+1))
        (cons penalty-1 (- x 1)))
       ((and (<= penalty+0 penalty-1) (<= penalty+0 penalty+1))
        (cons penalty+0 x))
       ((and (<= penalty+1 penalty-1) (<= penalty+1 penalty+0))
        (cons penalty+1 (+ x 1))))))

(define (get-cached cache x y w)
   (vector-ref cache (floor (+ x (* y w)))))
   
(define (set-cached! cache x y w v)
  (vector-set! cache (floor (+ x (* y w))) v))   

; Returns best seam from (x,y) and downwards
; Returns a seam, which is a list (p x0 x1 x2 x3 ...)
; where p is the penalty and xn describe the pixels
; of the path.
(define (best-seam-onwards image cache x y)
   (let ((cached (get-cached cache x y (image-width image))))
     (if cached
       cached
       (if (= y (- (image-height image) 1))
         (cons 0 '())
         (let* ((next (best-next-step image x y))
                (next-seam (best-seam-onwards image cache (cdr next) (+ 1 y)))
                (penalty (+ (car next) (car next-seam)))
                (this-seam (cons penalty (cons x (cdr next-seam)))))
            (set-cached! cache x y (image-width image) this-seam)
            this-seam)))))   

; Find the seam with smallest penalty.
; Returns a seam.
(define (best-seam image)
   (let loop ((best-seam (list 100000))
              (cache (make-vector (* (image-width image) (image-height image)) #f))
              (x 0))
       (if (= x (image-width image))
           best-seam
           (let ((cur-seam (best-seam-onwards image cache x 0)))
             (loop (if (< (car cur-seam) (car best-seam)) cur-seam best-seam)
                   cache
                   (+ x 1))))))

; Resize image to target-width.
; Returns a new image.
(define (retarget-image original-image target-width)
  (let loop ((image original-image)
             (pixels (- (image-width image) target-width)))
      (if (<= pixels 0)
        image
        (loop (shift-image image (best-seam image))
              (- pixels 1)))))

(define image (load-image "faar.png"))
;(define seam (best-seam image))
;(define new-image (draw-seam image seam #(1 0 0)))
(define new-image (retarget-image image 400))
(save-image new-image "out.png")

