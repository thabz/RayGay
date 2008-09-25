(load "scenes/lib/handy-extensions.scm")

;; Build the documentation
(define source-files (list
  "scenes/lib/colors.scm"
  "scenes/lib/vector-math.scm"
  "scenes/lib/handy-extensions.scm"
  "scenes/lib/raygay.scm"))  
  
(define functions '())  
  
(define (extract-from-source-file source-file)
  "Returns a list of pairs. A pair (definition . description) for each
   function found in the source-file."
  (let ((f (open-file-input-port source-file #f #f (native-transcoder))))
    (let loop ((result '()))
      (if (port-eof? f) result
      (let ((datum (read f)))
        (if (and (not (eof-object? datum))
                 (list? datum)
                 (not (null? datum))
                 (equal? (car datum) 'define)
                 (list? (cadr datum))
                 (string? (caddr datum)))
            (loop (cons (cons (cadr datum) (caddr datum)) result))
            (loop result)))))))

(define (parse-source-files source-files)
(let loop-files ((files source-files))
  (when (not (null? files))
    (set! functions (append functions (extract-from-source-file (car files))))
    (loop-files (cdr files)))))

(parse-source-files source-files)

        