;; Creates a list of numbers
;;
;; Usage (sequence 5) -> (0 1 2 3 4)
;;
(define (sequence num)
  (let iter ((i 0))
    (if (= i num) '()
      (cons i (iter (+ i 1))))))

;; Place objects along a path
;;
;; Usage:
;; (make-necklace spiral 200 (lambda () (make-sphere '(0 0 0) 20 chrome))) 
;;
(define (make-necklace path num maker)
  (map
    (lambda (i)
      (translate
	(maker)
	(point-on-path path (/ i num))))
    (sequence num)))
