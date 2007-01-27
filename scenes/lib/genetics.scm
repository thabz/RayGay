(load "raygay.scm")

; Swap two elements by index
(define (list-swap l i1 i2)
  (let ((tmp (list-ref l i2)))
    (list-set! l i2 (list-ref l i1))
    (list-set! l i1 tmp)
    l))

; Swap two random elements
(define (mutate l percent)
  (if (< (random 100) percent)
    (list-swap l (random (length l)) (random (length l))))
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

(define (sort-by-fittness pop fittness-function)
  (map cdr
   (sort 
     (map (lambda (p) (cons (fittness-function p) p)) pop)
     (lambda (a b) (< (car a) (car b))))))

(define (pick-chromosome population dist-table)
  (list-ref population 
   (list-ref dist-table (random (length dist-table)))))

; For pop-size 4 this results in (0 0 0 0 1 1 1 2 2 3)
(define (precalc-dist-table pop-size)
 (let loop ((i pop-size) 
	          (result '()))
  (if (< i 0) 
    result
    (loop (- i 1) 
          (append result (make-list i (- pop-size i)))))))

(define (genetics chromosome-size population-size fittness-function max-iters)
  (let ((dist-table (precalc-dist-table population-size)))
    (let loop ((curpop (random-population population-size chromosome-size))  
               (nextpop '())
               (lastbest '())
               (i 0))
    (if (= i max-iters)
      (car curpop)
      (begin
    ; Sort population by fittness
    (set! curpop (sort-by-fittness curpop fittness-function))
    (if (not (equal? lastbest (car curpop))) (set! i 0))
    ; Elitism, picks best two
    (set! nextpop (list (car curpop) (cadr curpop)))
    (display i)
    (display " ")
    (display (fittness-function (car curpop)))
    (newline)
    ; Generate crossovers
    (do ()
      ((= (length nextpop) (length curpop)))
      (append! nextpop (list 
         (mutate			
            (crossover (pick-chromosome curpop dist-table) 
	               (pick-chromosome curpop dist-table))
   	        100))))
    ; Rinse, repeat
    (loop nextpop '() (car nextpop) (+ i 1)))))))
