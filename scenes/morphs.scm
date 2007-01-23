(load "lib/raygay.scm")

; Swap two elements by index
(define (list-swap l i1 i2)
  (let ((tmp (list-ref l i2)))
    (list-set! l i2 (list-ref l i1))
    (list-set! l i1 tmp)
    l))

; Swap two random elements
(define (mutate l)
  (list-swap l (random (length l)) (random (length l)))
  l)

; Do a genetic crossover from two parent chromosomes.
; One-point crossover technique.
(define (crossover c1 c2)
   (let loop ((filler c2)
              (result (reverse (list-head c1 (random (length c1))))))
     (if (null? filler)
       (reverse result)
       (if (member (car filler) result)
           (loop (cdr filler) result)
           (loop (cdr filler) (cons (car filler) result))))))

(define (evaluate-population pop fitness-function)
  (map fitness-function pop))

; Randomize list
(define (list-shuffle l)
  (dotimes i (length l)
    (list-swap l i (random (length l))))
  l)

; Randomized list of 0,1,...,n
(define (random-list n)
  (do ((l '() (append! l (list (length l)))))
    ((= n (length l)) (list-shuffle l))))

; Make population-size number of lists of size chromosome-size
(define (random-population population-size chromosome-size)
  (do ((l '() (append! l (list (random-list chromosome-size)))))
    ((= population-size (length l)) l)))


(define (genetics chromosome-size population-size fittness-function max-iters)
  (let loop ((curpop (random-population population-size chromosome-size))  
             (nextpop '())
             (i 0))
    (if (= i max-iters) (car curpop))
    ; Evaluate population and sort curpop
    ; Elitism, picks best two
    (set! nextpop (list (car )))
    ; Generate crossovers
    ; Rinse, repeat
    (loop nextpop '())))


(define l1 (list 1 2 3 4 5 6 7 8 9 10))
(define l2 (list 10 9 8 7 6 5 4 3 2 1))
(display (crossover l1 l2))
(newline)


(define pop (random-population 5 10))
(display pop)
(newline)


(define ll (list 1 2 3 4 5 6 7 8 9 10))
(mutate ll)
(display ll)
(mutate ll)
(display ll)


(list-shuffle ll)
(display ll)
(list-shuffle ll)
(display ll)

(newline)