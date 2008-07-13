
;; This is the simple regexp engine written by Rob Pike and 
;; described by Brian Kernighan in "Beautiful Code", O'Reilly, 2007.
;; 
;; Usage: (match? "abc.*e$" "abcde") => #t
;; 
;; Supported are ^ * . $

(define (substr str offset)
 (substring str offset (string-length str)))

(define (match? regexp text)
  (if (char=? #\^ (string-ref regexp 0))
    (matchhere? (substr regexp 1) text)
    (let loop ((text text))
      (if (matchhere? regexp text)
        #t
        (if (zero? (string-length text))
          #f
          (loop (substr text 1))))))) 

(define (matchhere? regexp text)
  (cond
    ((zero? (string-length regexp)) 
      #t)
    ((and (< 1 (string-length regexp)) (char=? #\* (string-ref regexp 1))
      (match*? (string-ref regexp 0) (substr regexp 2) text)))
    ((and (= 1 (string-length regexp)) (char=? #\$ (string-ref regexp 0)))
      (zero? (string-length text)))
    ((and (not (zero? (string-length text)))
          (or (char=? #\. (string-ref regexp 0))
              (char=? (string-ref regexp 0) (string-ref text 0))))
      (matchhere? (substr regexp 1) (substr text 1)))
    (else #f)))
      
(define (match*? c regexp text)
  (let loop ((text text))
    (if (matchhere? regexp text)
       #t
       (if (and (not (zero? (string-length text)))
                (or (char=? #\. c)
                    (char=? c (string-ref text 0))))
           (loop (substr text 1))
           #f))))

  
